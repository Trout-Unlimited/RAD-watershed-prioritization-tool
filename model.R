# ============================================================
# Watershed Prioritization Workflow
# ============================================================
# This script creates watershed-scale indices and management strategies.
#
# Sections:
# A. RCAT watershed summaries
# B. BRAT beaver restoration potential
# C. Climate exposure index from PCA
# D. RAD classification and management strategies
#
# Notes:
# - RCAT scores are summarized to HUC12 using area-weighted averages
# - BRAT scores are summarized to HUC12 using stream-mile-weighted averages
# - Climate exposure is based on PCA of five variables
# - Climate users can choose 2040 or 2080
# - Climate users can choose percent or absolute change for all variables
# except CFM, which is always absolute change
# ============================================================

source("packages.R")

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
# User options
# ============================================================
# ---- Climate time period ----
# Choose "2040" or "2080"
time_period <- "2040"

# ---- Climate change metric for non-CFM variables ----
# Choose "Pct" or "Abs"
change_type <- "Pct"

# ---- Export ID ----
id <- "WY"

# ============================================================
# Import Data
# ============================================================
rcat_hucs <- read.csv(file.path("inputs", paste0(id, "_huc12s_rcat.csv")))
brat <- read.csv(file.path("inputs", paste0(id, "_huc12s_brat.csv")))
sfm <- read.csv(file.path("inputs", paste0(id, "_huc12s_flowmet.csv")))
temp <- read.csv(file.path("inputs", paste0(id, "_huc12s_norwest.csv")))
huc12s <- st_read(file.path("inputs", paste0(id, "_huc12s.gpkg")))

# ============================================================
# A. RCAT watershed condition
# ============================================================
# ---- Calculate area-weighted watershed averages ----
# Larger valley-bottom polygons contribute proportionally more
# to the watershed-scale summary.
rcat_condition <- rcat_hucs %>%
  group_by(huc12) %>%
  summarise(
    weighted_condition = sum(Condition * Area, na.rm = TRUE) / sum(Area, na.rm = TRUE),
    .groups = "drop"
  )

rcat_lui <- rcat_hucs %>%
  group_by(huc12) %>%
  summarise(
    weighted_LUI = sum(LUI * Area, na.rm = TRUE) / sum(Area, na.rm = TRUE),
    .groups = "drop"
  )

rcat_fpaccess <- rcat_hucs %>%
  group_by(huc12) %>%
  summarise(
    weighted_fpaccess = sum(FloodplainAccess * Area, na.rm = TRUE) / sum(Area, na.rm = TRUE),
    .groups = "drop"
  )

rcat_vegdep <- rcat_hucs %>%
  group_by(huc12) %>%
  summarise(
    weighted_vegdep = sum(RiparianDeparture * Area, na.rm = TRUE) / sum(Area, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Merge RCAT summaries ----
rcat <- list(rcat_condition, rcat_lui, rcat_fpaccess, rcat_vegdep) %>%
  reduce(full_join, by = "huc12")

# ---- Re-scale LUI so larger values indicate better condition ----
# Original LUI is assumed to range from 0 to 100, where larger values
# indicate greater land-use intensity. This transformation reverses it.
rcat <- rcat %>%
  mutate(
    weighted_LUI = 1 - (weighted_LUI / 100)
  )

# ---- Calculate empirical percentiles ----
# Percentiles describe relative standing among watersheds in the study area.
rcat <- rcat %>%
  mutate(
    ConditionPercentile = ecdf(weighted_condition)(weighted_condition),
    LUIPercentile = ecdf(weighted_LUI)(weighted_LUI),
    FPAccessPercentile = ecdf(weighted_fpaccess)(weighted_fpaccess),
    VegDepPercentile = ecdf(weighted_vegdep)(weighted_vegdep)
  )

# ============================================================
# B. BRAT beaver restoration potential
# ============================================================
# ---- Assign numeric scores to BRAT restoration categories ----
# Higher values indicate higher immediate beaver restoration potential.
# Users may need to adjust these lines depending on what management 
# categories are used in the specific BRAT project
unique(brat$ConsVRest)

category_scores <- c(
  "Natural or Anthropogenic Limitations" = 0,        
  "Conflict Management" = 1,                          
  "Land Management Change" = 2,                      
  "Potential Floodplain/Side Channel Opportunities" = 3, 
  "Beaver Mimicry" = 4,                              
  "Conservation/Appropriate for Translocation" = 5,  
  "Encourage Beaver Expansion/Colonization" = 6     
)

# ---- Calculate stream-mile-weighted watershed scores ----
beaver_potential <- brat %>%
  mutate(ConsVRest = as.character(ConsVRest)) %>%
  mutate(score = category_scores[ConsVRest]) %>%
  group_by(huc12) %>%
  summarise(
    total_stream_miles = sum(StreamMiles, na.rm = TRUE),
    weighted_sum = sum(StreamMiles * score, na.rm = TRUE),
    restoration_potential = weighted_sum / total_stream_miles,
    .groups = "drop"
  )

# ---- Calculate empirical percentiles ----
beaver_potential <- beaver_potential %>%
  mutate(
    beaver_percentile = ecdf(restoration_potential)(restoration_potential)
  )

# ============================================================
# C. Climate exposure index from PCA
# ============================================================
# ---- Merge stream flow and temperature data ----
climate_raw <- sfm %>%
  left_join(temp, by = "huc12")

# ---- Select the five variables used in the PCA ----
# MA = mean annual flow
# JJA = summer flow
# HIQ1_5 = 1.5-year bankfull flood magnitude
# CFM = center of flow mass timing
# ST = summer stream temperature
#
# Important:
# - MA, JJA, HIQ1_5, and ST use the user-selected change type
# - CFM always uses absolute change in days
climate_selected <- climate_raw %>%
  select(
    huc12,
    MA = all_of(paste0("MA_flow_", change_type, "_Chg_", time_period)),
    JJA = all_of(paste0("JJA_flow_", change_type, "_Chg_", time_period)),
    HIQ1_5 = all_of(paste0("HIQ1_5_flow_", change_type, "_Chg_", time_period)),
    CFM = all_of(paste0("CFM_flow_Abs_Chg_", time_period)),
    ST = all_of(paste0("st_", change_type, "_Chg_", time_period))
  )

# ---- Check the selected climate data ----
print(head(climate_selected))
print(colSums(is.na(climate_selected)))

# User should look for:
# - six columns: HUC12, MA, JJA, HIQ1_5, CFM, ST
# - CFM should come from the absolute-change column
# - no selected variable should be entirely NA
#
# If one selected variable is all NA, check file names, join keys, or column names.

# ---- Remove incomplete cases ----
climate_analysis <- climate_selected %>%
  filter(complete.cases(MA, JJA, HIQ1_5, CFM, ST))

# ---- Keep numeric variables for PCA ----
climate_vars <- climate_analysis %>%
  select(MA, JJA, HIQ1_5, CFM, ST)

# ---- Quick check of PCA inputs ----
print(names(climate_vars))
summary(climate_vars)

# User should look for:
# - five variables only
# - plausible ranges and signs
# - enough complete watersheds for PCA

# ---- Correlation matrix ----
climate_cor <- cor(climate_vars)
print(round(climate_cor, 3))
corrplot(climate_cor, method = "color", type = "upper")

# User should look for:
# - at least some moderate correlations
# - if nearly all correlations are close to zero, PCA may not summarize well

# ---- KMO test ----
climate_kmo <- KMO(climate_cor)
print(climate_kmo)

# Interpretation:
# - KMO evaluates whether the data are suitable for PCA
# - A value around 0.60 or higher is usually acceptable

# ---- Bartlett's test ----
climate_bartlett <- cortest.bartlett(climate_cor, n = nrow(climate_vars))
print(climate_bartlett)

# Interpretation:
# - A small p-value, usually < 0.05, supports using PCA

# ---- Perform PCA ----
climate_pca <- PCA(climate_vars, scale.unit = TRUE, graph = FALSE)

# ---- Inspect explained variance ----
print(climate_pca$eig)
fviz_eig(climate_pca, addlabels = TRUE)

# How to interpret explained variance:
# - PC1 explains the largest share of variation in the five climate variables
# - PC2 explains the second-largest share
# - A common rule of thumb is to consider components with eigenvalue > 1
# - Another common rule is to ask whether the retained components explain a
# large enough share of total variance to be meaningful
#
# What the user should look for:
# - If PC1 explains a large share of variance and represents a clear,
# interpretable exposure gradient, PC1 alone may be sufficient
# - If PC2 also has eigenvalue > 1 and captures an additional climate signal
# that is important to the study, then PC1 and PC2 can be combined

# ---- Inspect loadings and variable contributions ----
print(round(climate_pca$var$coord[, 1:2], 2))
print(round(climate_pca$var$contrib[, 1:2], 2))

# ---- Build a data frame for plotting ----
var_explained <- climate_pca$eig[, 2]

# ---- Plot ----
fviz_pca_biplot(
  climate_pca,
  geom.ind = "point",
  pointsize = 1,
  alpha.ind = 0.5,
  addEllipses = TRUE,
  ellipse.type = "confidence",
  col.var = "red",
  repel = TRUE,                     
  label = "var",                  
) +
  labs(
    x = paste0("PC1 (", round(var_explained[1], 1), "%)"),
    y = paste0("PC2 (", round(var_explained[2], 1), "%)")
)+
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  )

# How to interpret loadings:
# - Loadings show how strongly each variable is associated with each component
# - Large positive loadings mean the variable increases as that component increases
# - Large negative loadings mean the variable decreases as that component increases
# - Variables with larger absolute loadings help define that component
#
# What the user should look for:
# - For PC1: does it represent the main climate exposure gradient you want?
# - For PC2: does it capture an additional climate dimension that is still
# important enough to include?
#
# Example:
# - PC1 is strongly associated with MA, JJA, CFM, and HIQ1_5
# - PC2 is strongly associated with ST and HIQ1_5
# This suggests:
# - PC1 mainly reflects flow/timing change
# - PC2 adds a strong temperature component

# ---- Extract PCA scores ----
climate <- as.data.frame(climate_pca$ind$coord)
climate$huc12 <- climate_analysis$huc12

# ============================================================
# Option 1. Use PC1 only
# ============================================================
# Use this option if:
# - PC1 explains a large share of variance
# - PC1 represents the main climate exposure gradient of interest
# - the goal is to create the simplest possible index
#
# Decision check:
# - Look at the PC1 loadings above
# - Ask: do higher PC1 scores correspond to greater climate exposure?
#
# If YES:
climate$exposure_index <- climate$Dim.1

# If NO, comment out the line above and use this instead:
# climate$exposure_index <- -climate$Dim.1

# How to decide whether to reverse PC1:
# - The sign of a principal component is arbitrary
# - Reverse PC1 only if higher PC1 values correspond to lower exposure
# - Keep PC1 as-is if higher PC1 values correspond to greater exposure
#
# Example:
# - If larger changes in MA, JJA, HIQ1_5, and CFM indicate greater exposure,
# and those variables have positive PC1 loadings, then PC1 should usually
# be kept as-is
# - If the loadings imply that higher PC1 means less exposure, then reverse it

# ============================================================
# Option 2. Use a weighted combination of PC1 and PC2
# ============================================================
# Uncomment this option if:
# - PC2 also has eigenvalue > 1 or explains a meaningful amount of variance
# - PC2 captures an additional climate signal you want to retain
# - you want the index to reflect more than one climate dimension
#
# Before using this option:
# - Inspect whether PC1 and/or PC2 should be reversed
# - Make sure higher combined values will mean greater exposure
#
# Example: if both PC1 and PC2 point in the desired direction
# climate$exposure_index <-
# (climate$Dim.1 * (climate_pca$eig[1, 2] / 100)) +
# (climate$Dim.2 * (climate_pca$eig[2, 2] / 100))

# Example: if PC1 should be reversed but PC2 should not
 climate$exposure_index <-
 (-climate$Dim.1 * (climate_pca$eig[1, 2] / 100)) +
 ( climate$Dim.2 * (climate_pca$eig[2, 2] / 100))

# Example: if PC2 should be reversed but PC1 should not
# climate$exposure_index <-
# ( climate$Dim.1 * (climate_pca$eig[1, 2] / 100)) +
# (-climate$Dim.2 * (climate_pca$eig[2, 2] / 100))

# ---- Rescale climate exposure to 0-100 ----
climate$exposure_scaled <- rescale(climate$exposure_index, to = c(0, 100))

# Interpretation:
# - 0 = lowest relative exposure in the dataset
# - 100 = highest relative exposure in the dataset
# - This is a relative ranking within the chosen scenario, not an absolute threshold

# ---- Inspect climate exposure results ----
summary(climate$exposure_scaled)
hist(climate$exposure_scaled)

most_exposed <- climate %>%
  arrange(desc(exposure_scaled)) %>%
  head(10)

least_exposed <- climate %>%
  arrange(exposure_scaled) %>%
  head(10)

print(most_exposed)
print(least_exposed)

# User should look for:
# - whether the score distribution looks reasonable
# - whether the highest- and lowest-ranked watersheds make ecological sense

# ============================================================
# D. RAD classification and management strategies
# ============================================================
# ---- Merge watershed-scale datasets ----
# Climate object uses huc12; RCAT and BRAT also use huc12.
rad_input <- list(rcat, climate, beaver_potential) %>%
  reduce(full_join, by = "huc12")

# ---- Select the core variables used in RAD classification ----
final_df <- rad_input %>%
  select(
    watershed_id = huc12,
    rcat_score = weighted_condition,
    climate_exposure = exposure_index,
    beaver_potential = restoration_potential
  )

# ---- Rescale all three inputs to 0-100 ----
final_df <- final_df %>%
  mutate(
    rcat_scaled = rescale(rcat_score, to = c(0, 100)),
    climate_scaled = rescale(climate_exposure, to = c(0, 100)),
    beaver_scaled = rescale(beaver_potential, to = c(0, 100))
  )

# ---- Classify RCAT condition and climate exposure ----
# These thresholds are based on 33rd and 67th percentile breakpoints
# after re-scaling to 0-100.
final_df <- final_df %>%
  mutate(
    rcat_category = case_when(
      rcat_scaled >= 67 ~ "High_Quality",
      rcat_scaled >= 33 ~ "Moderate_Quality",
      TRUE ~ "Low_Quality"
    ),
    climate_category = case_when(
      climate_scaled >= 67 ~ "High_Exposure",
      climate_scaled >= 33 ~ "Moderate_Exposure",
      TRUE ~ "Low_Exposure"
    )
  )

# User should look for:
# - whether these thresholds are appropriate for the study area
# - optional changes can be made below if different thresholds are desired

# ---- Optional: custom thresholds ----
# Uncomment and edit if users want different cut points.
# final_df <- final_df %>%
# mutate(
# rcat_category = case_when(
# rcat_scaled >= 75 ~ "High_Quality",
# rcat_scaled >= 40 ~ "Moderate_Quality",
# TRUE ~ "Low_Quality"
# ),
# climate_category = case_when(
# climate_scaled >= 75 ~ "High_Exposure",
# climate_scaled >= 40 ~ "Moderate_Exposure",
# TRUE ~ "Low_Exposure"))

# ---- Assign primary RAD strategy ----
final_df <- final_df %>%
  mutate(
    rad_strategy = case_when(
      rcat_category == "High_Quality" &
        climate_category %in% c("Low_Exposure", "Moderate_Exposure") ~ "RESIST",
      rcat_category == "Moderate_Quality" &
        climate_category == "Low_Exposure" ~ "RESIST",
      
      rcat_category == "Low_Quality" &
        climate_category == "High_Exposure" ~ "ACCEPT",
      rcat_category == "Moderate_Quality" &
        climate_category == "High_Exposure" ~ "ACCEPT",
      
      rcat_category == "High_Quality" &
        climate_category == "High_Exposure" ~ "DIRECT",
      rcat_category %in% c("Low_Quality", "Moderate_Quality") &
        climate_category == "Moderate_Exposure" ~ "DIRECT",
      rcat_category == "Low_Quality" &
        climate_category == "Low_Exposure" ~ "DIRECT",
      
      TRUE ~ "ASSESS"
    )
  )

# ---- Classify beaver restoration potential ----
final_df <- final_df %>%
  mutate(
    beaver_category = case_when(
      is.na(beaver_scaled) ~ "Unknown",
      beaver_scaled >= 67 ~ "High",
      beaver_scaled >= 33 ~ "Moderate",
      TRUE ~ "Low"
    )
  )

# ---- Assign management strategies ----
final_df <- final_df %>%
  mutate(
    management_action = case_when(
      rad_strategy == "RESIST" & beaver_category == "High" ~
        "Conservation with beaver enhancement",
      rad_strategy == "RESIST" & beaver_category == "Moderate" ~
        "Conservation with potential beaver",
      rad_strategy == "RESIST" & beaver_category == "Low" ~
        "Traditional conservation",
      rad_strategy == "RESIST" & beaver_category == "Unknown" ~
        "Assess beaver potential for conservation",
      
      rad_strategy == "ACCEPT" & beaver_category == "High" ~
        "Adaptation with beaver",
      rad_strategy == "ACCEPT" & beaver_category == "Moderate" ~
        "Mixed adaptation",
      rad_strategy == "ACCEPT" & beaver_category == "Low" ~
        "Managed adaptation",
      rad_strategy == "ACCEPT" & beaver_category == "Unknown" ~
        "Assess beaver potential for adaptation",
      
      rad_strategy == "DIRECT" & beaver_category == "High" ~
        "Active beaver restoration",
      rad_strategy == "DIRECT" & beaver_category == "Moderate" ~
        "Mixed restoration",
      rad_strategy == "DIRECT" & beaver_category == "Low" ~
        "Alternative restoration",
      rad_strategy == "DIRECT" & beaver_category == "Unknown" ~
        "Assess beaver potential for restoration",
      
      TRUE ~ "Assess further"
    )
  )

# ---- Summarize results ----
rad_summary <- final_df %>%
  group_by(rad_strategy) %>%
  summarise(
    n_watersheds = n(),
    mean_rcat = mean(rcat_scaled, na.rm = TRUE),
    mean_climate = mean(climate_scaled, na.rm = TRUE),
    mean_beaver = mean(beaver_scaled, na.rm = TRUE),
    .groups = "drop"
  )

print(rad_summary)

# User should look for:
# - whether the distribution of watersheds among RESIST, ACCEPT, and DIRECT
# is ecologically plausible
# - whether category means align with expectations

# ---- Join back to huc12s geometry ----
final_sf <- huc12s %>%
  select(id, geom) %>%
  mutate(id = as.character(id)) %>%
  left_join(
    final_df %>% mutate(watershed_id = as.character(watershed_id)),
    by = c("id" = "watershed_id")
  )

# ---- Run hotspot analysis ----
data <- final_sf %>%
  filter(!is.na(climate_exposure))

nb <- poly2nb(data, queen = TRUE)
nb_self <- include.self(nb)
lw <- nb2listw(nb_self, style = "B")
gi_star <- localG(data$climate_exposure, lw)

data <- data %>%
  mutate(gi_star = as.numeric(gi_star))

data <- data %>%
  mutate(hotspot = case_when(
    gi_star >= 2.58 ~ "Hotspot 99%",
    gi_star >= 1.96 ~ "Hotspot 95%",
    gi_star >= 1.65 ~ "Hotspot 90%",
    gi_star <= -2.58 ~ "Coldspot 99%",
    gi_star <= -1.96 ~ "Coldspot 95%",
    gi_star <= -1.65 ~ "Coldspot 90%",
    TRUE ~ "Not significant"
  ))

ggplot(data) +
  geom_sf(aes(fill = hotspot), color = "white", linewidth = 0.1) +
  scale_fill_manual(values = c(
    "Hotspot 99%" = "#67000d", "Hotspot 95%" = "#cb181d", "Hotspot 90%" = "#fc9272",
    "Not significant" = "grey90",
    "Coldspot 90%" = "#9ecae1", "Coldspot 95%" = "#3182bd", "Coldspot 99%" = "#08306b"
  )) +
  theme_void()

# ---- Plot results ----
management_action_colors <- c(
  "Conservation with beaver enhancement" = "#1b7837",  
  "Conservation with potential beaver" = "#5aae61",
  "Traditional conservation" = "#a6dba0",
  "Assess beaver potential for conservation" = "#c7e9c0",
  "Adaptation with beaver" = "#b35806",  
  "Mixed adaptation" = "#e08214",
  "Managed adaptation" = "#fdb863",
  "Assess beaver potential for adaptation" = "#fee0b6",  
  "Active beaver restoration" = "#2166ac",  
  "Mixed restoration" = "#4393c3", 
  "Alternative restoration" = "#92c5de", 
  "Assess beaver potential for restoration" = "#d1e5f0",  
  "Assess further" = "#bdbdbd"
)

management_action_order <- c(
  "Conservation with beaver enhancement",
  "Conservation with potential beaver",
  "Traditional conservation",
  "Assess beaver potential for conservation",
  "Adaptation with beaver",
  "Mixed adaptation",
  "Managed adaptation",
  "Assess beaver potential for adaptation",
  "Active beaver restoration",
  "Mixed restoration",
  "Alternative restoration",
  "Assess beaver potential for restoration",
  "Assess further"
)

final_sf <- final_sf %>%
  mutate(management_action = factor(management_action, levels = management_action_order))

p <- ggplot(final_sf) +
  geom_sf(aes(fill = management_action), color = NA) +
  scale_fill_manual(
    values = management_action_colors,
    name = "Management Action",
    na.value = "grey90"
  ) +
  labs(title = paste0(id, " RAD Management Strategies ", time_period, " ", change_type, " Change")) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p

# Export final plot
ggsave(file.path("outputs", paste0(id, "_RAD_management_strategies_", time_period, "_", change_type, ".png")), p)

# Export final RAD output
st_write(final_sf, file.path("outputs", paste0(id, "_RAD_management_strategies_", time_period, "_", change_type, ".gpkg")), delete_dsn = TRUE)
