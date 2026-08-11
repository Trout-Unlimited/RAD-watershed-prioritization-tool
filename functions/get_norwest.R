# ============================================================
# Get NorWeST Data
# ============================================================
get_norwest <- function(huc12, data_dir = "inputs/norwest_gdb") {
  
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  
  bbox_4326 <- st_bbox(huc12)
  
  zip_url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.NorWeST_PredictedStreams.gdb.zip"
  layer <- "NorWeST_PredictedStreams"
  fields <- c("comid", "gnis_name", "nhdv", "s1_93_11", "s30_2040d", "s32_2080d")
  
  gdb_path <- .ensure_gdb(zip_url, data_dir)
  
  message("Reading and pre-filtering NorWeST_PredictedStreams layer")
  norwest_sf <- .read_gdb_layer(gdb_path, layer, fields, bbox_4326, geometry = TRUE)
  invalid <- !st_is_valid(norwest_sf)
  if (any(invalid)) norwest_sf[invalid, ] <- st_make_valid(norwest_sf[invalid, ])
  
  norwest_sf <- norwest_sf |>
    mutate(
      st_Abs_Chg_2040 = s30_2040d - s1_93_11,
      st_Abs_Chg_2080 = s32_2080d - s1_93_11,
      st_Pct_Chg_2040 = (s30_2040d - s1_93_11) / s1_93_11 * 100,
      st_Pct_Chg_2080 = (s32_2080d - s1_93_11) / s1_93_11 * 100
    )
  
  message("Clipping and summarizing to HUC12s")
  .clip_and_summarize(
    lines_sf = norwest_sf,
    polys_sf = huc12,
    poly_id_col = "huc12",
    value_cols = c("st_Pct_Chg_2040", "st_Pct_Chg_2080", "st_Abs_Chg_2040", "st_Abs_Chg_2080")
  )
}
