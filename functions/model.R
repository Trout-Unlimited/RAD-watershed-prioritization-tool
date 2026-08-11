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
# run_model(): runs Sections A-D for one watershed
# ============================================================
# Arguments:
# - current_id: the watershed id (one of names(data_list))
# - pca_vars: which candidate variables to include in the climate PCA.
#                  Candidates are "MA", "JJA", "HIQ1_5", "CFM", "ST".
#                  Default excludes MA, matching the prior commented-out setup.
# - pcs_to_keep: which principal component(s) to combine into the climate
#                  exposure index. Use 1 for PC1 only, or c(1, 2) to combine
#                  PC1 and PC2 (weighted by their share of explained variance).
# - reverse_pcs: which of the kept PCs (by number) should have their sign
#                  flipped before combining. NULL means no reversal.
#
# Workflow:
# 1. Call run_model(current_id) once with your best-guess pca_vars to see
#    the printed diagnostics (correlation matrix, KMO, Bartlett's, eigenvalues,
#    loadings, contributions) for that watershed.
# 2. Inspect the diagnostics and decide: which variables belong in the PCA,
#    which PC(s) to keep, and whether any need to be sign-reversed (see the
#    "How to decide" notes inline below).
# 3. Re-call run_model(current_id, pca_vars = ..., pcs_to_keep = ...,
#    reverse_pcs = ...) with your chosen settings. This only re-runs that one
#    watershed - it does not touch data_list or any other watershed's results.
run_model <- function(current_id,
                      time_period = "2040",
                      change_type = "Pct",
                      pca_vars = c("JJA", "HIQ1_5", "CFM", "ST"),
                      pcs_to_keep = 1,
                      reverse_pcs = NULL) {
  
  cat("\n\n============================================================\n")
  cat("Processing watershed:", current_id, "\n")
  cat("============================================================\n")
  
  rcat_hucs <- data_list[[current_id]]$rcat_hucs
  brat <- data_list[[current_id]]$brat
  sfm <- data_list[[current_id]]$sfm
  temp <- data_list[[current_id]]$temp
  huc12s <- data_list[[current_id]]$huc12s
  
  id <- current_id
  pw_select <- current_id
  
  # ============================================================
  # A. RCAT watershed condition
  # ============================================================
  # ---- Calculate area-weighted watershed averages ----
  # Larger valley-bottom polygons contribute proportionally more
  # to the watershed-scale summary.
  rcat_condition <- rcat_hucs |>
    group_by(huc12) |>
    summarise(
      weighted_condition = sum(Condition * Area, na.rm = TRUE) / sum(Area, na.rm = TRUE),
      .groups = "drop"
    )
  
  rcat_lui <- rcat_hucs |>
    group_by(huc12) |>
    summarise(
      weighted_LUI = sum(LUI * Area, na.rm = TRUE) / sum(Area, na.rm = TRUE),
      .groups = "drop"
    )
  
  rcat_fpaccess <- rcat_hucs |>
    group_by(huc12) |>
    summarise(
      weighted_fpaccess = sum(FloodplainAccess * Area, na.rm = TRUE) / sum(Area, na.rm = TRUE),
      .groups = "drop"
    )
  
  rcat_vegdep <- rcat_hucs |>
    group_by(huc12) |>
    summarise(
      weighted_vegdep = sum(RiparianDeparture * Area, na.rm = TRUE) / sum(Area, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Merge RCAT summaries ----
  rcat <- list(rcat_condition, rcat_lui, rcat_fpaccess, rcat_vegdep) |>
    reduce(full_join, by = "huc12")
  
  # ---- Re-scale LUI so larger values indicate better condition ----
  # Original LUI is assumed to range from 0 to 100, where larger values
  # indicate greater land-use intensity. This transformation reverses it.
  rcat <- rcat |>
    mutate(
      weighted_LUI = 1 - (weighted_LUI / 100)
    )
  
  # ---- Calculate empirical percentiles ----
  # Percentiles describe relative standing among watersheds in the study area.
  rcat <- rcat |>
    mutate(
      ConditionPercentile = ecdf(weighted_condition)(weighted_condition),
      LUIPercentile = ecdf(weighted_LUI)(weighted_LUI),
      FPAccessPercentile = ecdf(weighted_fpaccess)(weighted_fpaccess),
      VegDepPercentile = ecdf(weighted_vegdep)(weighted_vegdep)
    )
  
  # ============================================================
  # B. BRAT beaver restoration potential
  # ============================================================
  # ---- Calculate stream-mile-weighted watershed scores ----
  beaver_potential <- brat |>
    mutate(ConsVRest = as.character(ConsVRest)) |>
    mutate(score = category_scores[ConsVRest]) |>
    group_by(huc12) |>
    summarise(
      total_stream_miles = sum(StreamMiles, na.rm = TRUE),
      weighted_sum = sum(StreamMiles * score, na.rm = TRUE),
      restoration_potential = weighted_sum / total_stream_miles,
      .groups = "drop"
    )
  
  # ---- Calculate empirical percentiles ----
  beaver_potential <- beaver_potential |>
    mutate(
      beaver_percentile = ecdf(restoration_potential)(restoration_potential)
    )
  
  # ============================================================
  # C. Climate exposure index from PCA
  # ============================================================
  # ---- Merge stream flow and temperature data ----
  climate_raw <- sfm |>
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
  
  var_specs <- list(
    MA = function() paste0("MA_flow_", change_type, "_Chg_", time_period),
    JJA = function() paste0("JJA_flow_", change_type, "_Chg_", time_period),
    HIQ1_5 = function() paste0("HIQ1_5_flow_", change_type, "_Chg_", time_period),
    CFM = function() paste0("CFM_flow_Abs_Chg_", time_period),
    ST = function() paste0("st_", change_type, "_Chg_", time_period)
  )
  
  col_map <- sapply(names(var_specs), function(v) var_specs[[v]]())
  
  climate_selected <- climate_raw |>
    select(huc12, all_of(col_map))
  
  # ---- Check selected climate data for NAs----
  cat("\n---- NAs for selected climate data ----\n")
  print(colSums(is.na(climate_selected)))
  
  # User should look for:
  # - six columns: HUC12, MA, JJA, HIQ1_5, CFM, ST
  # - CFM should come from the absolute-change column
  # - no selected variable should be entirely NA
  #
  # If one selected variable is all NA, check file names, join keys, or column names.
  
  # ---- Remove incomplete cases ----
  climate_analysis <- climate_selected |>
    filter(complete.cases(across(all_of(pca_vars))))
  
  # ---- Keep numeric variables for PCA ----
  climate_vars <- climate_analysis |>
    select(all_of(pca_vars))
  
  # ---- Correlation matrix ----
  cat("\n---- Correlation matrix ----\n")
  climate_cor <- cor(climate_vars)
  print(round(climate_cor, 3))
  corrplot(climate_cor, method = "color", type = "upper")
  
  # User should look for:
  # - at least some moderate correlations
  # - if nearly all correlations are close to zero, PCA may not summarize well
  
  # ---- KMO test ----
  cat("\n---- Kaiser–Meyer–Olkin test ----\n")
  climate_kmo <- KMO(climate_cor)
  print(climate_kmo)
  
  # Interpretation:
  # - KMO evaluates whether the data are suitable for PCA
  # - A value around 0.60 or higher is usually acceptable
  
  # ---- Bartlett's test ----
  cat("\n---- Bartlett's test ----\n")
  climate_bartlett <- cortest.bartlett(climate_cor, n = nrow(climate_vars))
  print(climate_bartlett)
  
  # Interpretation:
  # - A small p-value, usually < 0.05, supports using PCA
  
  # ---- Perform PCA ----
  climate_pca <- PCA(climate_vars, scale.unit = TRUE, graph = FALSE)
  
  # ---- Inspect explained variance ----
  cat("\n---- Explained variance ----\n")
  print(climate_pca$eig)
  fviz_eig(climate_pca, addlabels = TRUE)
  
  # How to interpret explained variance:
  # - PC1 explains the largest share of variation in the climate variables
  # - PC2 explains the second-largest share
  # - A common rule of thumb is to consider components with eigenvalue > 1
  # - Another common rule is to ask whether the retained components explain a
  # large enough share of total variance to be meaningful
  #
  # What the user should look for:
  # - If PC1 explains a large share of variance and represents a clear,
  # interpretable exposure gradient, PC1 alone may be sufficient
  # (pcs_to_keep = 1)
  # - If PC2 also has eigenvalue > 1 and captures an additional climate signal
  # that is important to the study, then PC1 and PC2 can be combined
  # (pcs_to_keep = c(1, 2))
  
  # ---- Inspect loadings and variable contributions ----
  cat("\n---- Loadings ----\n")
  print(round(climate_pca$var$coord[, 1:2], 2))
  cat("\n---- Variable contributions ----\n")
  print(round(climate_pca$var$contrib[, 1:2], 2))
  
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
  # How to decide whether to reverse a PC:
  # - The sign of a principal component is arbitrary
  # - Reverse a PC (add its number to reverse_pcs) only if higher values of
  # that PC correspond to LOWER exposure
  # - Leave a PC as-is if higher values correspond to greater exposure
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
  
  # ---- Build the climate exposure index from pcs_to_keep and reverse_pcs ----
  # Each kept PC is weighted by its share of explained variance
  # (climate_pca$eig[pc, 2] / 100), and flipped in sign if listed in
  # reverse_pcs. This generalizes the old Option 1 (pcs_to_keep = 1) and
  # Option 2 (pcs_to_keep = c(1, 2)) into a single, parameterized step.
  exposure_index <- rep(0, nrow(climate))
  for (pc in pcs_to_keep) {
    dim_col <- paste0("Dim.", pc)
    weight <- climate_pca$eig[pc, 2] / 100
    sign <- if (pc %in% reverse_pcs) -1 else 1
    exposure_index <- exposure_index + sign * climate[[dim_col]] * weight
  }
  climate$exposure_index <- exposure_index
  
  # ---- Rescale climate exposure to 0-100 ----
  climate$exposure_scaled <- rescale(climate$exposure_index, to = c(0, 100))
  
  # Interpretation:
  # - 0 = lowest relative exposure in the dataset
  # - 100 = highest relative exposure in the dataset
  # - This is a relative ranking within the chosen scenario, not an absolute threshold
  
  # ---- Inspect climate exposure results ----
  hist(climate$exposure_scaled)
  
  # User should look for:
  # - whether the score distribution looks reasonable

  # ============================================================
  # D. RAD classification and management strategies
  # ============================================================
  # ---- Merge watershed-scale datasets ----
  # Climate object uses huc12; RCAT and BRAT also use huc12.
  rad_input <- list(rcat, climate, beaver_potential) |>
    reduce(full_join, by = "huc12")
  
  # ---- Select the core variables used in RAD classification ----
  final_df <- rad_input |>
    select(
      huc12,
      rcat_score = weighted_condition,
      climate_exposure = exposure_index,
      beaver_potential = restoration_potential
    )
  
  # ---- Rescale all three inputs to 0-100 ----
  final_df <- final_df |>
    mutate(
      rcat_scaled = rescale(rcat_score, to = c(0, 100)),
      climate_scaled = rescale(climate_exposure, to = c(0, 100)),
      beaver_scaled = rescale(beaver_potential, to = c(0, 100))
    )
  
  # ---- Classify RCAT condition and climate exposure ----
  # These thresholds are based on 33rd and 67th percentile breakpoints
  # after re-scaling to 0-100.
  final_df <- final_df |>
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
  # Uncomment and edit if you want different cut points.
  # final_df <- final_df |>
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
  final_df <- final_df |>
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
  final_df <- final_df |>
    mutate(
      beaver_category = case_when(
        is.na(beaver_scaled) ~ "Unknown",
        beaver_scaled >= 67 ~ "High",
        beaver_scaled >= 33 ~ "Moderate",
        TRUE ~ "Low"
      )
    )

  # ---- Assign management strategies ----
  final_df <- final_df |>
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
  rad_summary <- final_df |>
    group_by(rad_strategy) |>
    summarise(
      n_watersheds = n(),
      mean_rcat = mean(rcat_scaled, na.rm = TRUE),
      mean_climate = mean(climate_scaled, na.rm = TRUE),
      mean_beaver = mean(beaver_scaled, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("\n---- RAD summary ----\n")
  print(rad_summary)
  
  # User should look for:
  # - whether the distribution of watersheds among RESIST, ACCEPT, and DIRECT
  # is ecologically plausible
  # - whether category means align with expectations
  
  # ---- Join back to huc12s geometry ----
  final_sf <- huc12s |>
    select(huc12, geom) |>
    mutate(huc12 = as.numeric(huc12)) |>
    left_join(
      final_df, by = "huc12"
    )
  
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
  
  final_sf <- final_sf |>
    mutate(management_action = factor(management_action, levels = management_action_order))
  
  p <- ggplot(final_sf) +
    annotation_map_tile(type = "cartolight", zoomin = 0) + 
    geom_sf(aes(fill = management_action), color = "black", linewidth = 0.2, alpha = 0.85) +
    scale_fill_manual(
      values = management_action_colors,
      name = "Management Action",
      na.value = "grey90"
    ) +
    labs(title = paste0(
      sub("^WY_pw_", "", pw_select) |> gsub("_", " ", x = _),
      " RAD Management Strategies ", time_period, " ", change_type, " Change")
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  print(p)
  
  # Export final plot
  ggsave(
    file.path("outputs", paste0(paste0(gsub(" ", "_", pw_select), "_RAD_management_strategies_", time_period, "_", change_type, ".png"))),
    plot = p,
    width = 11,
    height = 8.5,
    units = "in",
    dpi = 500
  )
  
  # Export final RAD output
  st_write(final_sf, file.path("outputs", paste0(id, "_RAD_management_strategies_", time_period, "_", change_type, ".gpkg")), delete_dsn = TRUE)
  
  # ---- Return objects invisibly ----
  invisible(list(
    final_sf = final_sf,
    final_df = final_df,
    rad_summary = rad_summary,
    climate_pca = climate_pca
  ))
}
