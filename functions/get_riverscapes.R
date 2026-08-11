# ============================================================
# Get Riverscapes Data
# ============================================================
source("packages.R")

plan(multisession, workers = parallel::detectCores() - 1)

API_URL <- "https://api.data.riverscapes.net/"

# --- GraphQL query templates ---
search_query <- '
query SearchProjects($searchParams: ProjectSearchParamsInput!, $limit: Int!, $offset: Int!) {
  searchProjects(limit: $limit, offset: $offset, params: $searchParams) {
    results {
      item { id meta { key value } }
    }
  }
}'

files_query <- '
query GetProjectFiles($projectId: ID!) {
  project(id: $projectId) {
    files { localPath downloadUrl }
  }
}'

# ---- Per-project-type configuration ----
project_configs <- list(
  rcat = list(
    project_type_id = "rcat",
    gpkg_path = "outputs/rcat.gpkg",
    layer = "vwDgos",
    prepare = function(df) {
      df |>
        rename(Area = segment_area) |>
        mutate(across(c(Area, LUI, FloodplainAccess, RiparianDeparture, Condition),
                      ~na_if(.x, -9999)))
    },
    output_cols = c("Area", "LUI", "FloodplainAccess", "RiparianDeparture", "Condition")
  ),
  brat = list(
    project_type_id = "riverscapes_brat",
    gpkg_path = "outputs/brat.gpkg",
    layer = "vwDgos",
    prepare = function(df) {
      df |>
        mutate(
          ConsVRest = Opportunity,
          StreamMiles = na_if(centerline_length * 0.000621371, -9999 * 0.000621371)
        )
    },
    output_cols = c("ConsVRest", "StreamMiles")
  )
)

# ---- Post a GraphQL request and return the raw httr2 response ----
api_post <- function(body) {
  token <- Sys.getenv("RIVERSCAPES_TOKEN")
  request(API_URL) |>
    req_headers("Authorization" = paste("Bearer", token), "Content-Type" = "application/json") |>
    req_body_json(body) |>
    req_perform()
}

# ---- Extract the model version value out of a project's metadata ----
extract_version <- function(meta_df) {
  if (is.data.frame(meta_df)) {
    hit <- meta_df[tolower(meta_df$key) == "model version", "value"]
  } else {
    hit <- sapply(meta_df, function(m) if (tolower(m[["key"]]) == "model version") m[["value"]] else NA)
    hit <- hit[!is.na(hit)]
  }
  if (length(hit) > 0) hit[[1]] else NA_character_
}

# ---- Find the best (highest model version) project of a given type for each HUC10 ----
get_best_projects <- function(huc10s, project_type) {
  config <- project_configs[[project_type]]
  token <- Sys.getenv("RIVERSCAPES_TOKEN")
  
  reqs <- map(huc10s, function(huc) {
    request(API_URL) |>
      req_headers("Authorization" = paste("Bearer", token), "Content-Type" = "application/json") |>
      req_body_json(list(
        query = search_query,
        variables = list(limit = 50, offset = 0,
                         searchParams = list(
                           projectTypeId = config$project_type_id,
                           meta = list(list(key = "HUC", value = huc))))
      )) |>
      req_timeout(30)
  })
  resps <- req_perform_parallel(reqs, max_active = 25)
  
  set_names(map(resps, function(resp) {
    projects <- resp |> resp_body_json(simplifyVector = TRUE) |>
      pluck("data", "searchProjects", "results", "item")
    if (is.null(projects) || nrow(projects) == 0) return(NULL)
    
    projects |>
      mutate(model_version = sapply(meta, extract_version)) |>
      arrange(desc(numeric_version(ifelse(is.na(model_version), "0.0.0", model_version)))) |>
      slice(1) |>
      pull(id)
  }), huc10s)
}

# ---- Download each project's geopackage ----
download_gpkgs <- function(project_ids, download_dir, project_type) {
  config <- project_configs[[project_type]]
  valid <- compact(project_ids)
  
  file_reqs <- map(valid, function(id) {
    request(API_URL) |>
      req_headers("Authorization" = paste("Bearer", Sys.getenv("RIVERSCAPES_TOKEN")), "Content-Type" = "application/json") |>
      req_body_json(list(query = files_query, variables = list(projectId = id))) |>
      req_timeout(30)
  })
  file_resps <- req_perform_parallel(file_reqs, max_active = 25)
  
  gpkg_urls <- map_chr(file_resps, function(resp) {
    files_df <- resp |> resp_body_json(simplifyVector = TRUE) |> pluck("data", "project", "files")
    files_df |> as_tibble() |> filter(localPath == config$gpkg_path) |> pull(downloadUrl) |> first()
  })
  
  dest_files <- file.path(download_dir, paste0(names(valid), ".gpkg"))
  dl_reqs <- map(gpkg_urls, ~ request(.x) |> req_timeout(180) |> req_retry(max_tries = 3))
  req_perform_parallel(dl_reqs, paths = dest_files, max_active = 8)
  
  set_names(dest_files, names(valid))
}

# ---- Get Riverscapes project data (RCAT or BRAT), aggregated to HUC12 ----
get_riverscapes <- function(huc12, project_type, download_dir = tempdir()) {
  
  config <- project_configs[[project_type]]
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  
  huc12$huc10 <- substr(huc12$huc12, 1, 10)
  huc10s <- unique(huc12$huc10)
  
  message(sprintf("Searching %s projects for %d HUC10(s)", project_type, length(huc10s)))
  project_ids <- get_best_projects(huc10s, project_type)
  
  n_found <- length(compact(project_ids))
  message(sprintf("Downloading %d geopackage(s)", n_found))
  gpkgs <- download_gpkgs(project_ids, download_dir, project_type)
  
  huc12_split <- split(huc12[, "huc12"], huc12$huc10)
  
  message("Reading, preparing, and joining to HUC12s")
  result <- future_map(names(gpkgs), function(huc) {
    gpkg <- gpkgs[[huc]]
    huc12_sub <- st_make_valid(huc12_split[[huc]])

    raw <- st_read(gpkg, layer = config$layer, quiet = TRUE)
    prepared <- config$prepare(raw)

    invalid <- !st_is_valid(prepared)
    if (any(invalid)) prepared[invalid, ] <- st_make_valid(prepared[invalid, ])

    prepared <- prepared |> select(all_of(config$output_cols))

    st_join(prepared, st_transform(huc12_sub, st_crs(prepared)), join = st_intersects) |>
      st_drop_geometry() |>
      select(huc12, all_of(config$output_cols))
  }, .options = furrr_options(seed = TRUE)) |>
    
  bind_rows()
}
