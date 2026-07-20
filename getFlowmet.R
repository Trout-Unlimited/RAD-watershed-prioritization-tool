# ============================================================================
# --- Get FlowMet Data -------------------------------------------------------
# ============================================================================
source("utils.R")
# ----------------------------------------------------------------------------
#' Get modeled flow metrics aggregated to HUC12
#'
#' @param huc12 sf object of HUC12s
#' @param data_dir character; local folder to store the downloaded .gdb.zip
#'   files and their unzipped .gdb folders. Default "data/flowmet_gdb".
#'
#' @return a dataframe with one row per HUC12, with columns HUC12 and the
#'   length-weighted average percent/absolute change flow metrics.
getFlowmet <- function(huc12, data_dir = "data/flowmet_gdb") {
  
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  
  bbox_4326 <- st_bbox(huc12)
  
  gdb_sources <- list(
    list(
      zip_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.Hydro_FlowMet_2040sChgPct.gdb.zip",
      layer = "Hydro_FlowMet_2040sChgPct",
      fields = c("comid", "ma_p2040", "mjja_p2040", "hiq1_5_p2040")
    ),
    list(
      zip_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.Hydro_FlowMet_2080sChgPct.gdb.zip",
      layer = "Hydro_FlowMet_2080sChgPct",
      fields = c("comid", "ma_p2080", "mjja_p2080", "hiq1_5_p2080")
    ),
    list(
      zip_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.Hydro_FlowMet_2040sChgAbs.gdb.zip",
      layer = "Hydro_FlowMet_2040sChgAbs",
      fields = c("comid", "ma_a2040", "mjja_a2040", "hiq1_5_a2040", "cfm_a2040")
    ),
    list(
      zip_url = "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.Hydro_FlowMet_2080sChgAbs.gdb.zip",
      layer = "Hydro_FlowMet_2080sChgAbs",
      fields = c("comid", "ma_a2080", "mjja_a2080", "hiq1_5_a2080", "cfm_a2080")
    )
  )
  
  n_src <- length(gdb_sources)
  layer_data <- imap(gdb_sources, function(src, i) {
    gdb_path <- .ensure_gdb(src$zip_url, data_dir)
    list(gdb_path = gdb_path, layer = src$layer, fields = src$fields)
  })
  
  first <- layer_data[[1]]
  cli_progress_step("Reading and pre-filtering layers")
  base_sf <- .read_gdb_layer(first$gdb_path, first$layer, first$fields, bbox_4326, geometry = TRUE) |>
    st_make_valid()
  
  rest_tables <- map(layer_data[-1], function(ld) {
    .read_gdb_layer(ld$gdb_path, ld$layer, ld$fields, bbox_4326, geometry = FALSE)
  })
  
  cli_progress_step("Joining flow metric layers by comid")
  flow_combined <- reduce(
    c(list(st_drop_geometry(base_sf)), rest_tables),
    left_join,
    by = "comid"
  )
  
  flow_sf <- base_sf |>
    select("comid") |>
    left_join(flow_combined, by = "comid")
  
  value_cols <- c(
    "ma_p2040", "ma_p2080", "mjja_p2040", "mjja_p2080",
    "hiq1_5_p2040", "hiq1_5_p2080",
    "ma_a2040", "ma_a2080", "mjja_a2040", "mjja_a2080",
    "hiq1_5_a2040", "hiq1_5_a2080", "cfm_a2040", "cfm_a2080"
  )
  
  cli_progress_step(sprintf("Clipping and summarizing to HUC12s"))
  .clip_and_summarize(
    lines_sf = flow_sf,
    polys_sf = huc12,
    poly_id_col = "huc12",
    value_cols  = value_cols
  ) |>
    rename(
      HUC12 = huc12,
      MA_flow_Pct_Chg_2040 = ma_p2040,
      MA_flow_Pct_Chg_2080 = ma_p2080,
      JJA_flow_Pct_Chg_2040 = mjja_p2040,
      JJA_flow_Pct_Chg_2080 = mjja_p2080,
      HIQ1_5_flow_Pct_Chg_2040 = hiq1_5_p2040,
      HIQ1_5_flow_Pct_Chg_2080 = hiq1_5_p2080,
      MA_flow_Abs_Chg_2040 = ma_a2040,
      MA_flow_Abs_Chg_2080 = ma_a2080,
      JJA_flow_Abs_Chg_2040 = mjja_a2040,
      JJA_flow_Abs_Chg_2080 = mjja_a2080,
      HIQ1_5_flow_Abs_Chg_2040 = hiq1_5_a2040,
      HIQ1_5_flow_Abs_Chg_2080 = hiq1_5_a2080,
      CFM_flow_Abs_Chg_2040 = cfm_a2040,
      CFM_flow_Abs_Chg_2080 = cfm_a2080
    )
}
