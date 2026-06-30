# --- LOAD PACKAGES
library(arcgislayers)
library(sf)
library(httr2)
library(dplyr)
library(purrr)

# --- API Token Setup
# This script requires a Riverscapes API token, stored as an environment variable
# To set up your token:
# 1. Log in to https://data.riverscapes.net
# 2. Open DevTools (F12) and search "Bearer"
# 3. Copy the Authorization (everything after "Bearer")
# 4. In R, run: usethis::edit_r_environ()
# 4. In the file that opens, add this line:
#      RIVERSCAPES_TOKEN=your_actual_token_here and paste the Authorization in your_actual_token_here
# 5. Save this file and restart R

# Note: Because tokens expire, you will need to get a new token if you get an error when validating your token

# --- BEFORE RUNNING
# validate token
token <- Sys.getenv("RIVERSCAPES_TOKEN")
if (!nzchar(token)) {
  stop("RIVERSCAPES_TOKEN is not set. See setup instructions at the top of this script.")
}

API_URL <- "https://api.data.riverscapes.net/"

test_resp <- request(API_URL) |>
  req_headers("Authorization" = paste("Bearer", token), "Content-Type" = "application/json") |>
  req_body_json(list(query = "query { searchProjects(limit: 1, offset: 0, params: {}) { results { item { id } } } }")) |>
  req_error(is_error = \(r) FALSE) |>
  req_perform()

if (resp_status(test_resp) %in% c(401, 403)) {
  stop(
    "Riverscapes API token is expired or invalid (HTTP ", resp_status(test_resp), ").\n",
    "Get a fresh token (see setup instructions at top of script), update RIVERSCAPES_TOKEN ",
    "via usethis::edit_r_environ(), then restart R."
  )
}

# adjust state or directories if desired
state <- "WY"
download_dir <- "data/rcat"
csv_dir <- "data/WY_HUC10s_RCAT.csv"

# --- HUC10s DATA
huc10 <- arc_open("https://services1.arcgis.com/754BERmVIq3RqSf8/arcgis/rest/services/huc10s_PriorityWaters/FeatureServer/0")
huc10_df <- arc_select(huc10, where = paste0("States LIKE '%", state, "%'")) |>
  st_drop_geometry() |>
  select(huc10)

# --- QUERIES
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

# --- HELPERS
extract_version <- function(meta_df) {
  if (is.data.frame(meta_df)) {
    hit <- meta_df[tolower(meta_df$key) == "modelversion", "value"]
  } else {
    hit <- sapply(meta_df, function(m) if (tolower(m[["key"]]) == "modelversion") m[["value"]] else NA)
    hit <- hit[!is.na(hit)]
  }
  if (length(hit) > 0) hit[[1]] else NA_character_
}

api_post <- function(token, body) {
  request(API_URL) |>
    req_headers("Authorization" = paste("Bearer", token), "Content-Type" = "application/json") |>
    req_body_json(body) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
}

get_best_project_id <- function(huc, token) {
  resp <- api_post(token, list(
    query = search_query,
    variables = list(limit = 50, offset = 0, 
                     searchParams = list(
                       projectTypeId = "rcat",
                       meta = list(list(key = "HUC", value = huc))))
  ))
  
  if (resp_status(resp) != 200) return(NULL)
  
  projects <- resp |> resp_body_json(simplifyVector = TRUE) |>
    pluck("data", "searchProjects", "results", "item")
  
  if (is.null(projects) || nrow(projects) == 0) {
    message("No RCAT projects found for HUC ", huc)
    return(NULL)
  }
  
  projects |>
    mutate(model_version = sapply(meta, extract_version)) |>
    arrange(desc(numeric_version(ifelse(is.na(model_version), "0.0.0", model_version)))) |>
    slice(1) |>
    pull(id)
}

download_gpkg <- function(project_id, huc, token, download_dir) {
  resp <- api_post(token, list(query = files_query, variables = list(projectId = project_id)))
  
  if (resp_status(resp) != 200) return(NULL)
  
  files_df <- resp |> resp_body_json(simplifyVector = TRUE) |> pluck("data", "project", "files")
  
  if (is.null(files_df) || nrow(files_df) == 0) {
    message("No files found for project ", project_id)
    return(NULL)
  }
  
  gpkg_url <- files_df |> 
    filter(localPath == "outputs/rcat.gpkg") |> 
    pull(downloadUrl)
  
  if (length(gpkg_url) == 0 || is.na(gpkg_url)) {
    message("No .gpkg found for project ", project_id)
    return(NULL)
  }
  
  dest_file <- file.path(download_dir, paste0(huc, ".gpkg"))
  request(gpkg_url) |> 
    req_perform(path = dest_file)
  message("Downloaded: ", dest_file)
  dest_file
}

# --- RUN QUERY
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

downloaded_files <- map(huc10_df$huc10, function(huc) {
  message("Processing HUC: ", huc)
  project_id <- get_best_project_id(huc, token)
  if (is.null(project_id)) return(NULL)
  download_gpkg(project_id, huc, token, download_dir)
}) |> compact()

# --- BIND HUC10s
all_rcat <- map_dfr(downloaded_files, function(f) {
  huc <- tools::file_path_sans_ext(basename(f))
  st_read(f, layer = "vwDgos", quiet = TRUE) |>
    st_drop_geometry() |>
    mutate(huc10 = huc) |>
    select(huc10, Area = segment_area, LUI, FloodplainAccess, RiparianDeparture, Condition) |>
    mutate(across(where(is.numeric), ~na_if(.x, -9999)))
})

# --- WRITE CSV
write.csv(all_rcat, csv_dir, row.names = FALSE)
