# ============================================================================
# --- Get HUC10 Priority Waters ----------------------------------------------
# ============================================================================
source("packages.R")
# ----------------------------------------------------------------------------
#' Get HUC10s for a state or states' priority waters
#'
#' @param states character; one or more two-letter state codes (e.g. "WY", or
#'   c("WY", "MT")). If NULL, returns priority-water HUCs for every state.
#'
#' @return sf object of HUC10 priority waters
getPWhuc10s <- function(states = NULL) {
  
  prio <- arc_open("https://services1.arcgis.com/754BERmVIq3RqSf8/arcgis/rest/services/huc10s_PriorityWaters/FeatureServer/0")
  
  where_clause <- "1=1"
  if (!is.null(states)) {
    quoted_states <- paste0("'", toupper(states), "'")
    where_clause <- paste0("states IN (", paste(quoted_states, collapse = ", "), ")")
  }
  
  suppressMessages(arc_select(prio, where = where_clause)) |> st_make_valid()
}
