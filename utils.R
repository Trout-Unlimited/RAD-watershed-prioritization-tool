# ============================================================================
# --- GDB Helpers ------------------------------------------------------------
# ============================================================================
# Helpers for pulling USFS file geodatabases (NorWeST and FlowMet). Sourced by 
# getNorwest.R and getFlowmet.R.
# ============================================================================
source("packages.R")
# ----------------------------------------------------------------------------
#' Download and unzip a .gdb.zip if not already present
#'
#' @param zip_url URL of the .gdb.zip file
#' @param data_dir local folder to store the zip and unzipped .gdb
#' @return path to the unzipped .gdb folder
.ensure_gdb <- function(zip_url, data_dir, timeout = 900) {
  
  zip_name <- basename(zip_url)
  gdb_name <- sub("\\.zip$", "", zip_name)
  gdb_path <- file.path(data_dir, gdb_name)
  zip_path <- file.path(data_dir, zip_name)
  
  if (dir.exists(gdb_path) && length(st_layers(gdb_path)$name) > 0) {
    return(gdb_path)
  }
  
  old_timeout <- getOption("timeout")
  options(timeout = timeout)
  on.exit(options(timeout = old_timeout))
  
  cli_progress_step(sprintf("Downloading %s", zip_name))
  download.file(zip_url, destfile = zip_path, mode = "wb", quiet = TRUE)
  cli_progress_step(sprintf("Unzipping %s", zip_name))
  unzip(zip_path, exdir = data_dir)
  
  gdb_path
}

# ----------------------------------------------------------------------------
#' Read (and spatially pre-filter) a layer from a file geodatabase
#'
#' @param gdb_path path to the .gdb folder
#' @param layer layer name within the geodatabase
#' @param fields character vector of field names to keep
#' @param bbox an sf/`st_bbox` object (any CRS) used as the spatial pre-filter
#' @param geometry logical; keep the geometry column (default TRUE)
#' @return an sf object (or plain data frame if `geometry = FALSE`), in EPSG:4326
.read_gdb_layer <- function(gdb_path, layer, fields, bbox, geometry = TRUE) {
  
  layer_crs <- st_layers(gdb_path)$crs[[match(layer, st_layers(gdb_path)$name)]]
  wkt <- st_as_text(st_transform(st_as_sfc(bbox), layer_crs)[[1]])
  
  sf_obj <- st_read(gdb_path, layer = layer, wkt_filter = wkt, quiet = TRUE)
  
  geom_col <- tolower(attr(sf_obj, "sf_column"))
  names(sf_obj) <- tolower(names(sf_obj))
  st_geometry(sf_obj) <- geom_col
  
  sf_obj <- st_transform(sf_obj, 4326)
  fields <- tolower(fields)
  
  if (geometry) {
    sf_obj[, c(fields, geom_col)]
  } else {
    st_drop_geometry(sf_obj)[, fields]
  }
}

# ----------------------------------------------------------------------------
#' Clip line features to polygons and compute length-weighted summaries
#'
#' @param lines_sf sf line object, already spatially pre-filtered to the AOI bbox
#' @param polys_sf sf polygon object of HUC12s to clip against, with an id
#'   column named `poly_id_col`
#' @param poly_id_col name of the HUC12 id column in `polys_sf`
#' @param value_cols character vector of metric columns in `lines_sf` to
#'   length-weight and summarize
#' @param crs_planar EPSG code for a projected, equal-area CRS to intersect in
#' @return a dataframe with one row per HUC12, with a length-weighted mean of
#'   each column in `value_cols`
.clip_and_summarize <- function(lines_sf, polys_sf, poly_id_col, value_cols,
                                crs_planar = 5070) {
  
  lines_planar <- st_transform(lines_sf, crs_planar)
  polys_planar <- st_transform(polys_sf, crs_planar) |> st_make_valid()
  
  lines_candidates <- st_filter(lines_planar, polys_planar, .predicate = st_intersects)
  
  clipped <- st_intersection(lines_candidates, polys_planar[, poly_id_col])
  clipped$seg_length <- as.numeric(st_length(clipped))
  clipped <- st_drop_geometry(clipped)
  
  clipped |>
    group_by(.data[[poly_id_col]]) |>
    summarise(
      across(all_of(value_cols), ~ weighted.mean(.x, w = seg_length, na.rm = TRUE)),
      .groups = "drop"
    )
}
