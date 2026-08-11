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

# Export id
export_id <- "WY_pw"

# ============================================================
# Get huc12s
# ============================================================
base_url = "https://services1.arcgis.com/754BERmVIq3RqSf8/arcgis/rest/services/TU_National_Priority_Waters_Official_view/FeatureServer/2/query"

get_priority_waters <- function(state) {
  
  where_clause <- sprintf("state = '%s'", state)
  
  where_enc <- URLencode(where_clause, reserved = TRUE)
  
  q_url <- sprintf(
    "%s?where=%s&outFields=*&f=geojson",
    base_url,
    where_enc
  )
  
  st_read(q_url, quiet = TRUE)
}

pw <- get_priority_waters(state) |> st_make_valid()

plot(pw$geometry)

unique(pw$pwName)

huc12s_list <- unique(pw$pwName) |>
  set_names() |>
  map(function(nm) {
    aoi <- pw |>
      filter(pwName == nm) |>
      st_make_valid() |>
      st_union()
    get_huc(AOI = aoi, type = "huc12")
  })

# ============================================================
# Get Riverscapes, FlowMet, and NorWeST data
# ============================================================
results <- huc12s_list |>
  imap(function(huc12s, nm) {
    message("Processing: ", nm)
    list(
      huc12s = huc12s,
      rcat = get_riverscapes(huc12 = huc12s, project_type = "rcat"),
      brat = get_riverscapes(huc12 = huc12s, project_type = "brat"),
      flowmet = get_flowmet(huc12 = huc12s),
      norwest = get_norwest(huc12 = huc12s)
    )
  })

# ============================================================
# Export
# ============================================================
dir.create("inputs", showWarnings = FALSE, recursive = TRUE)

iwalk(results, function(res, id) {
  id <- gsub(" ", "_", id)
  full_id <- paste0(export_id, "_", id)
  
  write.csv(res$rcat, file.path("inputs", paste0(full_id, "_huc12s_rcat.csv")), row.names = FALSE)
  write.csv(res$brat, file.path("inputs", paste0(full_id, "_huc12s_brat.csv")), row.names = FALSE)
  write.csv(res$flowmet, file.path("inputs", paste0(full_id, "_huc12s_flowmet.csv")), row.names = FALSE)
  write.csv(res$norwest, file.path("inputs", paste0(full_id, "_huc12s_norwest.csv")), row.names = FALSE)
  st_write(res$huc12s, file.path("inputs", paste0(full_id, "_huc12s.gpkg")), delete_dsn = TRUE, quiet = TRUE)
})
