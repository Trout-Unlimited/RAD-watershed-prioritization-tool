# ============================================================
# Run Watershed Prioritization Workflow
# ============================================================
source("functions/model.R")

# ============================================================
# Required input files and columns
# ============================================================
# 1. RCAT watershed table
# File: WY_huc12s_rcat.csv
# Required columns:
# - huc12
# - Area
# - LUI
# - FloodplainAccess
# - RiparianDeparture
# - Condition
#
# 2. BRAT watershed table
# File: WY_huc12s_brat.csv
# Required columns:
# - huc12
# - ConsVRest
# - StreamMiles
#
# 3. Stream flow summary table
# File: WY_huc12s_flowmet.csv
# Required columns:
# - huc12
# - MA_flow_Pct_Chg_2040 / 2080
# - MA_flow_Abs_Chg_2040 / 2080
# - JJA_flow_Pct_Chg_2040 / 2080
# - JJA_flow_Abs_Chg_2040 / 2080
# - HIQ1_5_flow_Pct_Chg_2040 / 2080
# - HIQ1_5_flow_Abs_Chg_2040 / 2080
# - CFM_flow_Abs_Chg_2040 / 2080
#
# 4. Summer stream temperature table
# File: WY_huc12s_norwest.csv
# Required columns:
# - huc12
# - st_Pct_Chg_2040 / 2080
# - st_Abs_Chg_2040 / 2080

# ============================================================
# Import Data
# ============================================================
ids <- list.files("inputs", pattern = "^WY_pw.*_huc12s_rcat\\.csv$") |>
  sub("_huc12s_rcat\\.csv$", "", x = _)

data_list <- ids |>
  set_names() |>
  map(function(id) {
    list(
      rcat_hucs = read.csv(file.path("inputs", paste0(id, "_huc12s_rcat.csv"))),
      brat = read.csv(file.path("inputs", paste0(id, "_huc12s_brat.csv"))),
      sfm = read.csv(file.path("inputs", paste0(id, "_huc12s_flowmet.csv"))),
      temp = read.csv(file.path("inputs", paste0(id, "_huc12s_norwest.csv"))),
      huc12s = st_read(file.path("inputs", paste0(id, "_huc12s.gpkg")), quiet = TRUE)
    )
  })

# ============================================================
# User options
# ============================================================

# ---- Assign numeric scores to BRAT restoration categories ----
# Higher values indicate higher immediate beaver restoration potential.
# Users may need to adjust these lines depending on what management 
# categories are used in the specific BRAT project.
data_list |>
  map(~ .x$brat$ConsVRest) |>
  unlist() |>
  unique()

category_scores <- c(
  "Encourage Beaver Expansion/Colonization" = 6,  
  "Conservation/Appropriate for Translocation" = 5, 
  "Beaver Mimicry" = 4,    
  "Potential Floodplain/Side Channel Opportunities" = 3,
  "Conflict Management" = 2,                            
  "Land Management Change" = 1,
  "Natural or Anthropogenic Limitations" = 0
) 

# ---- Retained variables for PCA ----
pca_vars <- c(
  "MA", 
  "JJA",
  "HIQ1_5",
  "CFM",
  "ST"
)

# ============================================================
# Run model
# ============================================================
# check priority water IDs
names(data_list)
id <- names(data_list)[4]

result <- run_model(
  id, 
  time_period = "2080", 
  change_type = "Pct", 
  pca_vars = pca_vars, 
  pcs_to_keep = c(1,2), 
  reverse_pcs = 1
)

names(result)

maplibre_view(result$final_sf$geom)
