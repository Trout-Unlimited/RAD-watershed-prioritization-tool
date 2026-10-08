st_layers("data/GreaterYellowstoneArea_LandscapeDynamics.gpkg")

gye <- st_read("data/GreaterYellowstoneArea_LandscapeDynamics.gpkg", "GYCC_GYA_AreaOfAnalysis")

huc12s <- arc_select(
  huc12_fl,
  fields = "huc12",
  filter_geom = gye |> st_geometry() |> st_combine()
) |>
  st_transform(st_crs(gye))

huc12s <- huc12s |>
  mutate(huc10 = substr(huc12, 1, 10)) |>
  st_transform(5070)

huc10s <- unique(huc12s$huc10)
