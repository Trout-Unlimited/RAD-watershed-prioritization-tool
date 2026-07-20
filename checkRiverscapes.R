# ============================================================================
# --- Check Riverscapes Project Availability ---------------------------------
# ============================================================================
source("getRiverscapes.R")

# ----------------------------------------------------------------------------
#' Search for the best-matching project (by model version) for every HUC10
#'
#' @return tibble: huc10, id (NA if none found), model_version (NA if none)
.search_projects_report <- function(huc10s, project_type) {
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
      ))
  })
  resps <- req_perform_parallel(reqs, max_active = 25, progress = FALSE, on_error = "continue")
  
  map2(huc10s, resps, function(huc, resp) {
    if (inherits(resp, "error") || resp_status(resp) != 200) {
      return(tibble(huc10 = huc, id = NA_character_, model_version = NA_character_))
    }
    projects <- resp |> resp_body_json(simplifyVector = TRUE) |>
      pluck("data", "searchProjects", "results", "item")
    if (is.null(projects) || nrow(projects) == 0) {
      return(tibble(huc10 = huc, id = NA_character_, model_version = NA_character_))
    }
    best <- projects |>
      mutate(model_version = sapply(meta, extract_version)) |>
      arrange(desc(numeric_version(ifelse(is.na(model_version), "0.0.0", model_version)))) |>
      slice(1)
    tibble(huc10 = huc, id = best$id, model_version = best$model_version)
  }) |>
    bind_rows()
}

# ----------------------------------------------------------------------------
#' Download + open each found project's gpkg layer, recording the
#' reason for any failure
#'
#' @return tibble: huc10, status ("ok"/"no_gpkg_found"/"download_failed"/
#'   "layer_read_failed"), detail (failure message, NA if ok)
.attempt_reads <- function(search_df, project_type, download_dir) {
  config <- project_configs[[project_type]]
  found <- filter(search_df, !is.na(id))
  
  if (nrow(found) == 0) {
    return(tibble(huc10 = character(), status = character(), detail = character()))
  }
  
  # --- look up each project's file list, to find its gpkg download URL ---
  file_reqs <- map(found$id, function(id) {
    request(API_URL) |>
      req_headers("Authorization" = paste("Bearer", Sys.getenv("RIVERSCAPES_TOKEN")), "Content-Type" = "application/json") |>
      req_body_json(list(query = files_query, variables = list(projectId = id)))
  })
  file_resps <- req_perform_parallel(file_reqs, max_active = 25, progress = FALSE, on_error = "continue")
  
  urls <- map2_chr(file_resps, found$id, function(resp, id) {
    if (inherits(resp, "error") || resp_status(resp) != 200) return(NA_character_)
    files_df <- tryCatch(
      resp |> resp_body_json(simplifyVector = TRUE) |> pluck("data", "project", "files") |> as_tibble(),
      error = function(e) NULL
    )
    if (is.null(files_df) || nrow(files_df) == 0) return(NA_character_)
    url <- files_df |> filter(localPath == config$gpkg_path) |> pull(downloadUrl)
    if (length(url) == 0) NA_character_ else url[[1]]
  })
  has_url <- !is.na(urls)
  
  # --- download every gpkg that has a URL ---
  dl_hucs <- found$huc10[has_url]
  dl_urls <- urls[has_url]
  dest_files <- file.path(download_dir, paste0(dl_hucs, "_", project_type, ".gpkg"))
  
  dl_resps <- if (length(dl_urls) > 0) {
    req_perform_parallel(map(dl_urls, request), paths = dest_files,
                         max_active = 25, progress = FALSE, on_error = "continue")
  } else {
    list()
  }
  dl_ok <- map_lgl(dl_resps, ~ !inherits(.x, "error"))
  
  # --- assemble per-HUC10 status ---
  map(seq_len(nrow(found)), function(i) {
    huc <- found$huc10[i]
    
    if (!has_url[i]) {
      return(tibble(huc10 = huc, status = "no_gpkg_found", detail = NA_character_))
    }
    
    j <- which(dl_hucs == huc)
    if (!dl_ok[j]) {
      return(tibble(huc10 = huc, status = "download_failed", detail = NA_character_))
    }
    
    gpkg <- dest_files[j]
    read_result <- tryCatch({
      st_read(gpkg, layer = config$layer, quiet = TRUE)
      TRUE
    }, error = function(e) conditionMessage(e))
    
    if (isTRUE(read_result)) {
      tibble(huc10 = huc, status = "ok", detail = NA_character_)
    } else {
      available <- tryCatch(suppressWarnings(paste(st_layers(gpkg)$name, collapse = ", ")), error = function(e) NA_character_)
      tibble(huc10 = huc, status = "layer_read_failed",
             detail = paste0(read_result, " | available layers: ", available))
    }
  }) |>
    bind_rows()
}

# ----------------------------------------------------------------------------
#' Report Riverscapes project availability for RCAT and/or BRAT
#'
#' @param huc12 sf object of HUC12s (same AOI used by getFlowmet()/getNorwest())
#' @param project_types character vector; one or both of "rcat", "brat"
#' @return invisibly, a named list (by project_type) of list(search, read)
#'   detail tibbles
checkRiverscapes <- function(huc12, project_types = c("rcat", "brat")) {
  
  download_dir <- file.path(tempdir(), "riverscapes_check")
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(download_dir, recursive = TRUE))
  
  huc10s <- unique(substr(huc12$huc12, 1, 10))
  n_total <- length(huc10s)
  
  results <- set_names(map(project_types, function(pt) {
    cli_progress_step(sprintf("Checking %s availability for %d HUC10(s)", pt, n_total))
    search_df <- .search_projects_report(huc10s, pt)
    read_df <- .attempt_reads(search_df, pt, download_dir)
    list(search = search_df, read = read_df)
  }), project_types)
  
  cat("\n============================================================\n")
  cat("Riverscapes Project Availability --", n_total, "HUC10(s) in AOI\n")
  cat("============================================================\n")
  
  for (pt in project_types) {
    search_df <- results[[pt]]$search
    read_df <- results[[pt]]$read
    
    n_found <- sum(!is.na(search_df$id))
    n_missing <- n_total - n_found
    
    cat("\n---", toupper(pt), "---\n")
    cat(sprintf("Projects found: %d / %d HUC10(s) (%d missing)\n", n_found, n_total, n_missing))
    
    if (n_found > 0) {
      cat("\nModel version breakdown (of found projects):\n")
      version_counts <- search_df |> filter(!is.na(id)) |> count(model_version, sort = TRUE)
      for (i in seq_len(nrow(version_counts))) {
        v <- ifelse(is.na(version_counts$model_version[i]), "unknown", version_counts$model_version[i])
        cat(sprintf("  %s: %d\n", v, version_counts$n[i]))
      }
    }
    
    n_ok <- sum(read_df$status == "ok")
    cat(sprintf("\nSuccessfully opened: %d / %d found project(s)\n", n_ok, nrow(read_df)))
    
    n_failed <- nrow(read_df) - n_ok
    if (n_failed > 0) {
      cat("Failed to read, by reason:\n")
      reason_counts <- read_df |> filter(status != "ok") |> count(status, sort = TRUE)
      for (i in seq_len(nrow(reason_counts))) {
        cat(sprintf("  %s: %d\n", reason_counts$status[i], reason_counts$n[i]))
      }
    }
  }
  cat("\n============================================================\n")
  
  invisible(results)
}
