# ---- Run Watershed Prioritization Workflow ----
source("R/model.R")

# ---- Import Data ----
rcat <- read_parquet("data/GYE_huc12s_rcat.parquet")
brat <- read_parquet("data/GYE_huc12s_brat.parquet")
sfm <- read_parquet("data/GYE_huc12s_flowmet.parquet")
temp <- read_parquet("data/GYE_huc12s_norwest.parquet")
huc12s <- st_read("data/GYE_huc12s.gpkg")
pw_sf <- st_read("data/GYE_huc12s.gpkg")

climate <- sfm |>
  left_join(temp, by = c("huc12")) |>
  rename(
    ST_A2040 = st_Abs_Chg_2040,
    ST_A2080 = st_Abs_Chg_2080,
    ST_P2040 = st_Pct_Chg_2040,
    ST_P2080 = st_Pct_Chg_2080
  )

pw_names <- unique(rcat$pwName)

# data_list holds each priority water's data separately. This is only needed
# if you wish to run the model separately per priority water
data_list <- setNames(
  lapply(pw_names, function(pw) {
    list(
      rcat = filter(rcat, pwName == pw),
      brat = filter(brat, pwName == pw),
      sfm  = filter(sfm,  pwName == pw),
      temp = filter(temp, pwName == pw)
    )
  }),
  pw_names
)

# ---- Retained variables for PCA ----
# MA = mean annual flow
# JJA = summer flow
# HIQ1_5 = 1.5-year bankfull flood magnitude
# CFM = center of flow mass timing
# ST = summer stream temperature

pca_vars <- c(
  "MA", 
  "JJA",
  "HIQ1_5",
  "CFM",
  "ST"
)


# ---- Run model ----
# to run the model for a single priority water
# run e.g.:
# run_model(id = pw_names[1], time_period = "2040", change_type = "P")

# Run for full study area (id = NULL)
results <- run_model(
  time_period = "2040", 
  change_type = "P", 
  pca_vars = pca_vars, 
  pcs_to_keep = c(1,2), 
  reverse_pcs = 1
)

names(results)

summary(rcat$LUI)
summary(rcat$RiparianDeparture)
summary(rcat$FloodplainAccess)

st_write(final_sf, "data/WY_pw_RAD.gpkg", delete_dsn = TRUE)
