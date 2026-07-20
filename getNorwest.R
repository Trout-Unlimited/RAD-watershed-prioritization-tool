# ============================================================================
# --- Get NorWeST Data -------------------------------------------------------
# ============================================================================
source("utils.R")
# ----------------------------------------------------------------------------
#' Get NorWeST modeled stream temperature data, aggregated to HUC12
#'
#' @param huc12 sf object of HUC12s
#' @param data_dir character; local folder to store the downloaded .gdb.zip file 
#'   and its unzipped .gdb folder. Default "data/norwest_gdb".
#'
#' @return a dataframe with one row per HUC12, with columns:
#'   HUC12, st_Abs_Chg_2040 and st_Abs_Chg_2080 (length-weighted average
#'   absolute change, degrees C, from S30_2040D / S32_2080D), and
#'   st_Pct_Chg_2040 and st_Pct_Chg_2080 (length-weighted average percent
#'   change, computed per-segment as absolute change divided by the
#'   1993-2011 historical baseline S1_93_11, times 100, before averaging
getNorwest <- function(huc12, data_dir = "data/norwest_gdb") {
  
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  
  bbox_4326 <- st_bbox(huc12)
  
  zip_url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.NorWeST_PredictedStreams.gdb.zip"
  layer <- "NorWeST_PredictedStreams"
  fields <- c("comid", "gnis_name", "nhdv", "s1_93_11", "s30_2040d", "s32_2080d")
  
  gdb_path <- .ensure_gdb(zip_url, data_dir)
  
  cli_progress_step("Reading + pre-filtering NorWeST_PredictedStreams layer")
  norwest_sf <- .read_gdb_layer(gdb_path, layer, fields, bbox_4326, geometry = TRUE) |>
    st_make_valid()

  norwest_sf <- norwest_sf |>
    mutate(
      st_Abs_Chg_2040 = s30_2040d - s1_93_11,
      st_Abs_Chg_2080 = s32_2080d - s1_93_11,
      st_Pct_Chg_2040 = (s30_2040d - s1_93_11) / s1_93_11 * 100,
      st_Pct_Chg_2080 = (s32_2080d - s1_93_11) / s1_93_11 * 100
    )
  
  cli_progress_step(sprintf("Clipping and summarizing to HUC12s"))
  .clip_and_summarize(
    lines_sf = norwest_sf,
    polys_sf = huc12,
    poly_id_col = "huc12",
    value_cols = c("st_Pct_Chg_2040", "st_Pct_Chg_2080", "st_Abs_Chg_2040", "st_Abs_Chg_2080")
  )|>
    rename(HUC12 = huc12)
}
