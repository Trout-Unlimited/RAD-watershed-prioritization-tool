# ============================================================
# Watershed Prioritization Workflow
# ============================================================
# This script creates watershed-scale indices and management strategies.
#
# Notes:
# - RCAT scores are summarized to HUC12 using area-weighted averages
# - BRAT scores are summarized to HUC12 using area-weighted averages
# - Climate exposure is based on PCA of five variables
# - Users can choose 2040 or 2080
# - Users can choose percent or absolute change for all climate variables
# except CFM, which is always absolute change
# ============================================================

library(dplyr)
library(purrr)
library(sf)
library(FactoMineR)
library(factoextra)
library(psych)
library(corrplot)
library(scales)
library(arrow)

# Arguments:
# - id: the watershed id (one of names(data_list))
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
# 1. Call run_model(id) once with all pca_vars to see
#    the printed diagnostics (correlation matrix, KMO, Bartlett's, eigenvalues,
#    loadings, contributions) for that watershed.
# 2. Inspect the diagnostics and decide: which variables belong in the PCA,
#    which PC(s) to keep, and whether any need to be sign-reversed (see the
#    "How to decide" notes inline below).
# 3. Re-call run_model(id, pca_vars = ..., pcs_to_keep = ...,
#    reverse_pcs = ...) with your chosen settings. This only re-runs that one
#    watershed - it does not touch data_list or any other watershed's results.
run_model <- function(id = NULL,
                      time_period = "2040",
                      change_type = "P",
                      pca_vars = c("JJA", "HIQ1_5", "CFM", "ST"),
                      pcs_to_keep = 1,
                      reverse_pcs = NULL) {
  
  cat("\n\n============================================================\n")
  cat("Processing:", if (is.null(id)) "ALL WATERSHEDS" else id, "\n")
  cat("============================================================\n")
  
  if (is.null(id)) {
    # ---- Use full all hucs ----
    rcat <- rcat
    brat <- brat
  } else {
    # ---- Use one watershed's data ----
    rcat <- data_list[[id]]$rcat
    brat <- data_list[[id]]$brat
  }

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
    MA = function() paste0("MA_", change_type, time_period),
    JJA = function() paste0("MJJA_", change_type, time_period),
    HIQ1_5 = function() paste0("HIQ1_5_", change_type, time_period),
    CFM = function() paste0("CFM_A", time_period),
    ST = function() paste0("ST_", change_type, time_period)
  )
  
  col_map <- sapply(names(var_specs), function(v) var_specs[[v]]())
  
  climate_selected <- climate |>
    select(huc12, all_of(col_map))
  
  # ---- Check the selected climate data ----
  cat("\n---- Preview of selected climate data ----\n")
  print(head(climate_selected))
  
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
  
  # ---- Quick check of PCA inputs ----
  cat("\n---- PCA input variables ----\n")
  print(names(climate_vars))
  print(summary(climate_vars))
  
  # User should look for:
  # - only the variables listed in pca_vars
  # - plausible ranges and signs
  # - enough complete watersheds for PCA
  
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
  fviz_pca_biplot(climate_pca, repel = TRUE)
  
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
  cat("\n---- Climate exposure summary ----\n")
  print(summary(climate$exposure_scaled))
  hist(climate$exposure_scaled)
  
  most_exposed <- climate |>
    arrange(desc(exposure_scaled)) |>
    head(10)
  
  least_exposed <- climate |>
    arrange(exposure_scaled) |>
    head(10)
  
  cat("\n---- Most exposed watersheds ----\n")
  print(most_exposed)
  cat("\n---- Least exposed watersheds ----\n")
  print(least_exposed)
  
  # User should look for:
  # - whether the score distribution looks reasonable
  # - whether the highest- and lowest-ranked watersheds make ecological sense
  
  # ============================================================
  # D. RAD classification and management strategies
  # ============================================================
  # ---- Merge watershed-scale datasets ----
  rad_input <- list(rcat, climate, brat) |>
    reduce(full_join, by = "huc12")
  
  # ---- Select the core variables used in RAD classification ----
  final_df <- rad_input |>
    select(
      huc12,
      rcat_score = Condition,
      climate_exposure = exposure_index,
      beaver_potential = Opportunity
    )
  
  # ---- Rescale all three inputs to 0-100 ----
  final_df <- final_df %>%
    mutate(
      rcat_scaled = rescale(rcat_score, to = c(0, 100)),
      climate_scaled = rescale(climate_exposure, to = c(0, 100)),
      beaver_scaled = rescale(beaver_potential, to = c(0, 100))
    )
  
  
  # ---- Classify RCAT condition and climate exposure ----
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
  
  # ---- RCAT categories for mapping ----
  rcat_categories <- rcat |>
    mutate(
      LUICategory = case_when(
        is.na(LUI) ~ "Unknown",
        LUI >= 0.67 ~ "High",     
        LUI >= 0.33 ~ "Moderate",
        TRUE ~ "Low"
      ),
      VegDepCategory = case_when(
        is.na(RiparianDeparture) ~ "Unknown",
        RiparianDeparture >= 0.67 ~ "High",
        RiparianDeparture >= 0.33 ~ "Moderate",
        TRUE ~ "Low"
      ),
      FPAccessCategory = case_when(
        is.na(FloodplainAccess) ~ "Unknown",
        FloodplainAccess >= 0.67 ~ "High",
        FloodplainAccess >= 0.33 ~ "Moderate",
        TRUE ~ "Low"
      )
    ) |>
    select(huc12, LUICategory, VegDepCategory, FPAccessCategory)
  
  # ---- Join everything back to huc12 geometry ----
  final_sf <- huc12s |>
    select(huc12, geom) |>
    left_join(final_df, by = "huc12") |>
    left_join(rcat, by = "huc12") |>
    left_join(rcat_categories, by = "huc12")
  
  # ---- Plot results ----
  management_action_colors <- c(
    "Conservation with beaver enhancement" = "#00441b",  
    "Conservation with potential beaver" = "#238b44",
    "Traditional conservation" = "#66c178",
    "Assess beaver potential for conservation" = "#c7e9c0",
    "Adaptation with beaver" = "#7a2c04",
    "Mixed adaptation" = "#e55609",
    "Managed adaptation" = "#fdb138",
    "Assess beaver potential for adaptation" = "#fccba1",  
    "Active beaver restoration" = "#012f6b",  
    "Mixed restoration" = "#1f72b1", 
    "Alternative restoration" = "#6bafd8", 
    "Assess beaver potential for restoration" = "#c5daee",  
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
    geom_sf(aes(fill = management_action), color = "black", linewidth = 0.2, alpha = 0.85) +
    scale_fill_manual(
      values = management_action_colors,
      name = "Management Action",
      na.value = "grey90"
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
  
  # ---- Return objects invisibly ----
  invisible(list(
    final_sf = final_sf,
    final_df = final_df,
    rad_summary = rad_summary,
    climate_pca = climate_pca
  ))
}
