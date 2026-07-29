# ============================================================
#  Get Model Inputs
# ============================================================
# This script grabs the data used in model.R
# ============================================================

# ---- Source functions ----
invisible(lapply(list.files("functions", pattern = "\\.R$", full.names = TRUE), source))

# ============================================================
# User options
# ============================================================
# State to query priority waters
state <- "Wyoming"

# Export ID
id <- "WY"

# ============================================================
# Get huc12s for priority waters
# ============================================================
url <- "https://services1.arcgis.com/754BERmVIq3RqSf8/arcgis/rest/services/TU_National_Priority_Waters_Official_view/FeatureServer/2"
tu_layer <- arc_open(url)

get_priority_waters <- function(state, pwName = NULL, layer = tu_layer) {
  where_clause <- sprintf("state = '%s'", state)
  
  if (!is.null(pwName)) {
    where_clause <- paste(
      where_clause,
      sprintf("AND pwName = '%s'", pwName)
    )
  }
  
  arc_select(
    layer,
    where = where_clause
  )
}

aoi <- get_priority_waters(state) |>
  st_make_valid() |>
  st_union()
  
huc12s <- get_huc(AOI = aoi, type = "huc12")

# ============================================================
# Get Riverscapes, FlowMet, and NorWeST data
# ============================================================
rcat <- get_riverscapes(huc12 = huc12s, project_type = "rcat")
brat <- get_riverscapes(huc12 = huc12s, project_type = "brat")
flowmet <- get_flowmet(huc12 = huc12s)
norwest <- get_norwest(huc12 = huc12s)

# ============================================================
# Export
# ============================================================
dir.create("inputs", showWarnings = FALSE, recursive = TRUE)

write.csv(rcat, file.path("inputs", paste0(id, "_huc12s_rcat.csv")), row.names = FALSE)
write.csv(brat, file.path("inputs", paste0(id, "_huc12s_brat.csv")), row.names = FALSE)
write.csv(flowmet, file.path("inputs", paste0(id, "_huc12s_flowmet.csv")), row.names = FALSE)
write.csv(norwest, file.path("inputs", paste0(id, "_huc12s_norwest.csv")), row.names = FALSE)
st_write(huc12s, file.path("inputs", paste0(id, "_huc12s.gpkg")), delete_dsn = TRUE)
