# ============================================================================
# --- Get Model Data ---------------------------------------------------------
# ============================================================================
source("getPWhuc10s.R")
source("getRiverscapes.R")
source("getFlowmet.R")
source("getNorwest.R")
source("checkRiverscapes.R")

# Get priority watersheds
wy_huc10s <- getPWhuc10s(state = "WY") |> st_union()
wy_huc12s <- get_huc(AOI = wy_huc10s, type = "huc12")

# Check Riverscapes data availability
avail <- checkRiverscapes(huc12 = wy_huc12s, project_types = "brat")

failed <- avail$brat$read |> filter(status == "layer_read_failed")
unique(failed$detail)

avail$brat$search |>
  inner_join(failed, by = "huc10") |>
  count(model_version, sort = TRUE)

# Get Riverscapes data
wy_rcat <- getRiverscapes(project_type = "rcat", huc12 = wy_huc12s)
wy_brat <- getRiverscapes(project_type = "brat", huc12 = wy_huc12s)

# Get FlowMet data
wy_flow <- getFlowmet(huc12 = wy_huc12s)

# Get NorWeST data
wy_norwest <- getNorwest(huc12 = wy_huc12s)

# Write to csvs
write.csv(wy_rcat, "data/wy_huc12s_rcat.csv", row.names = FALSE)
write.csv(wy_brat, "data/wy_huc12s_brat.csv", row.names = FALSE)
write.csv(wy_flow, "data/wy_huc12s_flowmet.csv", row.names = FALSE)
write.csv(wy_norwest, "data/wy_huc12s_norwest.csv", row.names = FALSE)
