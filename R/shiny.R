library(shiny)
library(mapgl)
library(bslib)

ui <- page_sidebar(
  title = "Wyoming Riparian Management Priotitization Tool",
  sidebar = sidebar(),
  maplibreOutput("map", height = "100%")
)

server <- function(input, output, session) {
  
  output$map <- renderMaplibre({
    maplibre(
      bounds = c(-111.94856, 40.28891, -104.17252, 46.49966 )
    ) |>
      add_raster_source(
        id = "satellite-src",
        tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
      ) |>
      add_raster_layer(
        id = "imagery", 
        source = "satellite-src"
      ) |>
      add_pmtiles_source(
        id = "rcat-src", 
        url = "https://raw.githubusercontent.com/Trout-Unlimited/RAD-watershed-prioritization-tool/main/pmtiles/rcat.pmtiles"
      ) |>
      add_pmtiles_source(
        id = "brat-src", 
        url = "https://raw.githubusercontent.com/Trout-Unlimited/RAD-watershed-prioritization-tool/main/pmtiles/brat.pmtiles"
      ) |>
      add_pmtiles_source(
        id = "pw-src", 
        url = "https://raw.githubusercontent.com/Trout-Unlimited/RAD-watershed-prioritization-tool/main/pmtiles/pw.pmtiles"
      ) |>
      add_fill_layer(
        id = "rcat-access",
        source = "rcat-src",
        source_layer = "rcat",
        fill_color = interpolate(
          column = "FloodplainAccess",
          values = c(0, 25, 50, 75, 100),
          stops = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641")
        ),
        visibility = "none"
      ) |>
      add_fill_layer(
        id = "rcat-departure",
        source = "rcat-src",
        source_layer = "rcat",
        fill_color = interpolate(
          column = "RiparianDeparture",
          values = c(0, 25, 50, 75, 100),
          stops = c("#1a9641", "#ffffbf", "#a6d96a", "#fdae61", "#d7191c")
        ),
        visibility = "none"
      ) |>
      add_fill_layer(
        id = "rcat-lui",
        source = "rcat-src",
        source_layer = "rcat",
        fill_color = interpolate(
          column = "LUI",
          values = c(0, 25, 50, 75, 100),
          stops = c("#1a9641","#a6d96a","#ffffbf","#fdae61","#d7191c")
        ),
        visibility = "none"
      ) |>
      add_fill_layer(
        id = "rcat-condition",
        source = "rcat-src",
        source_layer = "rcat",
        fill_color = interpolate(
          column = "Condition",
          values = c(0, 25, 50, 75, 100),
          stops = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641")
        ),
        visibility = "none"
      ) |>
      add_fill_layer(
        id = "brat-risk",
        source = "brat-src",
        source_layer = "brat",
        fill_color = match_expr(
          column = "Risk",
          values = c("Negligible Risk", "Minor Risk", "Some Risk", "Considerable Risk"),
          stops = c("#1a9850",  "#ffffbf", "#fc8d59", "#d73027")
        ),
        visibility = "none"
      ) |>
      add_fill_layer(
        id = "brat-limitation",
        source = "brat-src",
        source_layer = "brat",
        fill_color = match_expr(
          column = "Limitation",
          values = c(
            "Dam Building Possible", 
            "Potential Reservoir or Land Use Change", 
            "Anthropogenically Limited", 
            "Stream Power Limited", 
            "Stream Size Limited", 
            "Slope Limited", 
            "Naturally Vegetation Limited", 
            "Other"
          ),       
          stops = c("#1b9e77", "#e6ab02", "#e7298a", "#d95f02", "#377eb8", "#7570b3", "#66a61e", "#ffff33")
        ),
        visibility = "none"
      ) |>
      add_fill_layer(
        id = "brat-opportunity",
        source = "brat-src",
        source_layer = "brat",
        fill_color = match_expr(
          column = "Opportunity",
          values = c(
            "Encourage Beaver Expansion/Colonization",
            "Conservation/Appropriate for Translocation",
            "Beaver Mimicry",
            "Potential Floodplain/Side Channel Opportunities",
            "Conflict Management",
            "Land Management Change",
            "Natural or Anthropogenic Limitations"
          ),       
          stops = c("#08519c", "#67a9cf", "#d1e5f0", "#f7f7f7","#fddbc7", "#ef8a62", "#b2182b")
        ),
        visibility = "none"
      ) |>
      add_line_layer(
        id = "pw",
        source = "pw-src",
        source_layer = "pw",
        line_color = "black",
        line_width = 2,
        visibility = "visible"
      ) |>
      add_layers_control(position = "bottom-right") |>
      add_continuous_legend(
        legend_title = "Floodplain Accessibility (percentile)",
        values = c("", "", "", "", ""),
        filter_values = c(0, 25, 50, 75, 100),
        colors = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"),
        layer_id = "rcat-access",
        add = TRUE,
        interactive = TRUE,
        filter_column = "FloodplainAccess",
        width = "auto"
      ) |>
      add_continuous_legend(
        legend_title = "Vegetation Departure (percentile)",
        values = c("", "", "", "", ""),
        filter_values = c(0, 100),
        colors = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"),
        layer_id = "rcat-departure",
        add = TRUE,
        interactive = TRUE,
        filter_column = "RiparianDeparture",
        width = "auto"
      ) |>
      add_continuous_legend(
        legend_title = "Land Use Intensity (percentile)",
        values = c("", "", "", "", ""),
        filter_values = c(0, 25, 50, 75, 100),
        colors = c("#1a9641","#a6d96a","#ffffbf","#fdae61","#d7191c"),
        layer_id = "rcat-lui",
        add = TRUE,
        interactive = TRUE,
        filter_column = "LUI",
        width = "auto"
      ) |>
      add_continuous_legend(
        legend_title = "Riparian Condition (percentile)",
        values = c("", "", "", "", ""),
        filter_values = c(0, 25, 50, 75, 100),
        colors = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641"),
        layer_id = "rcat-condition",
        add = TRUE,
        interactive = TRUE,
        filter_column = "Condition",
        width = "auto"
      ) |>
      add_categorical_legend(
        legend_title = "Dam Building Risk",
        values = c("Negligible Risk", "Minor Risk", "Some Risk", "Considerable Risk"),
        colors = c("#1a9850",  "#ffffbf", "#fc8d59", "#d73027"),
        layer_id = "brat-risk",
        add = TRUE,
        interactive = TRUE,
        filter_column = "Risk",
        width = "auto"
      ) |>
      add_categorical_legend(
        legend_title = "Dam Building Limitation",
        values = c(
          "Dam Building Possible", 
          "Potential Reservoir or Land Use Change", 
          "Anthropogenically Limited", 
          "Stream Power Limited", "Stream Size Limited", 
          "Slope Limited", 
          "Naturally Vegetation Limited", 
          "Other"
        ),
        colors = c("#1b9e77", "#e6ab02", "#e7298a", "#d95f02", "#377eb8", "#7570b3", "#66a61e", "#ffff33"),
        layer_id = "brat-limitation",
        add = TRUE,
        interactive = TRUE,
        filter_column = "Limitation",
        width = "auto"
      ) |>
      add_categorical_legend(
        legend_title = "Beaver Restoration Opportunity",
        values = c(
          "Encourage Beaver Expansion/Colonization",
          "Conservation/Appropriate for Translocation",
          "Beaver Mimicry",
          "Potential Floodplain/Side Channel Opportunities",
          "Conflict Management",
          "Land Management Change",
          "Natural or Anthropogenic Limitations"
        ),
        colors = c("#08519c", "#67a9cf", "#d1e5f0", "#f7f7f7","#fddbc7", "#ef8a62", "#b2182b"),
        layer_id = "brat-opportunity",
        add = TRUE,
        interactive = TRUE,
        filter_column = "Opportunity",
        width = "auto"
      )
  })
}

shinyApp(ui, server)
