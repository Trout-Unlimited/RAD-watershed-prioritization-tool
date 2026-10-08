# ---- Get HUC12s for TU Priority Waters ----
source("R/setup.R")

# state to query priority waters
state <- "Wyoming"

pw <- st_read(
  paste0(
    "https://services1.arcgis.com/754BERmVIq3RqSf8/",
    "arcgis/rest/services/TU_National_Priority_Waters_Official_view/",
    "FeatureServer/2/query",
    "?where=state%3D%27", state, "%27",
    "&outFields=pwName",
    "&f=geojson"
  ),
  quiet = TRUE
) |>
  st_make_valid()         

# connect to huc12s feature layer
huc12_fl <- arc_open(
  "https://services5.arcgis.com/7weheFjxuNkGGiZi/ArcGIS/rest/services/Watershed_Boundary_HUC12/FeatureServer/0"
)

huc12s <- arc_select(
  huc12_fl,
  fields = "huc12",
  filter_geom = pw |> st_geometry() |> st_combine()
) |>
  st_transform(st_crs(pw))

# spatial join priority waters to huc12s by largest overlap
pw_huc12s <- huc12s |>
  st_join(pw, left = FALSE, largest = TRUE) |>
  mutate(huc10 = substr(huc12, 1, 10)) |>  # get huc10s by taking the first 10 digits of huc12 codes
  st_transform(5070)

# theses huc10s will be used to query Riverscapes data

# write to disk
st_write(pw_huc12s, "data/WY_pw_huc12s.gpkg", delete_dsn = TRUE)
