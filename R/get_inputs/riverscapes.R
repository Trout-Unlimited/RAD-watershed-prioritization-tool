# Download RCAT and BRAT projects from the Riverscapes Data Exchange and 
# compute area-weighted watershed summaries

# load packages and connect to duckdb
source("R/setup.R")

# ---- read in huc12s ----
pw_huc12s <- st_read("data/WY_pw_huc12s.gpkg")

# get huc10s
huc10s <- unique(pw_huc12s$huc10)


# ---- search for projects ----
rcat_projects <- search_projects(list(projectTypeId = "rcat", HUC = huc10s), parallel = TRUE) |>
  group_by(HUC) |>
  slice_max(`Model Version`, n = 1, with_ties = FALSE) |>    # grab latest model version
  ungroup()

brat_projects <- search_projects(list(projectTypeId = "brat", HUC = huc10s), parallel = TRUE) |>
  group_by(HUC) |>
  slice_max(`Model Version`, n = 1, with_ties = FALSE) |>
  ungroup()


# ---- download projects ----
download_projects(
  project_ids = rcat_projects$id,
  download_dir = "data/WY_pw_rcat",
  layers = "rcat.gpkg",
  parallel = TRUE
)

download_projects(
  project_ids = brat_projects$id,
  download_dir = "data/WY_pw_brat_old",
  layers = "brat.gpkg",
  parallel = TRUE
)

# get gpkg paths
rcat_gpkg <- dbGetQuery(con, sprintf(
  "SELECT file FROM glob('%s/WY_pw_rcat*/**/rcat.gpkg')", "data")
)$file

brat_gpkg <- dbGetQuery(con, sprintf(
  "SELECT file FROM glob('%s/WY_pw_brat*/**/brat.gpkg')", "data")
)$file

# check layer names
st_layers(rcat_gpkg[1])
st_layers(brat_gpkg[1])

# ---- UNION ALL of ST_Read calls ----
read_gpkgs <- function(paths, layer, cols) {
  queries <- sprintf(
    "SELECT %s FROM ST_Read('%s', layer = '%s')",
    paste(cols, collapse = ", "),
    paths,
    layer
  )
  paste(queries, collapse = "\n    UNION ALL\n    ")
}

rcat_read <- read_gpkgs(
  rcat_gpkg,
  "vwDgos",
  c("Condition", "LUI", "FloodplainAccess", "RiparianDeparture", "geom")
)

brat_read <- read_gpkgs(
  brat_gpkg,
  "vwDgos",
  c("Opportunity", "geom")
)

# ---- summarize RCAT ----
dbExecute(con, sprintf("
  COPY (
  
  WITH huc12s AS (
    SELECT 
      huc12, 
      geom 
    
    FROM ST_Read('data/WY_pw_huc12s.gpkg')
  ),

  rcat AS (
    SELECT
      Condition,
      LUI,
      FloodplainAccess,
      RiparianDeparture,
      ST_Transform(
        geom,
        'EPSG:4326',
        'EPSG:5070',
        true
      ) AS geom

    FROM (
      %s
    )
  ),

  clipped AS (
    SELECT
      h.huc12,
      ST_Area(
        ST_Intersection(
          r.geom,
          h.geom
        )
      ) AS area,
      r.* EXCLUDE (geom)

    FROM rcat AS r

    JOIN huc12s AS h
      ON ST_Intersects(r.geom, h.geom)
  )

  SELECT
    huc12,

    SUM(
      CASE
        WHEN Condition IS NOT NULL AND Condition >= 0
        THEN Condition * area
      END
    )
    / SUM(
      CASE
        WHEN Condition IS NOT NULL AND Condition >= 0
        THEN area
      END
    ) AS Condition,

    SUM(
      CASE
        WHEN LUI IS NOT NULL AND LUI >= 0
        THEN LUI * area
      END
    )
    / SUM(
      CASE
        WHEN LUI IS NOT NULL AND LUI >= 0
        THEN area
      END
    ) AS LUI,

    SUM(
      CASE
        WHEN FloodplainAccess IS NOT NULL AND FloodplainAccess >= 0
        THEN FloodplainAccess * area
      END
    )
    / SUM(
      CASE
        WHEN FloodplainAccess IS NOT NULL AND FloodplainAccess >= 0
        THEN area
      END
    ) AS FloodplainAccess,

    SUM(
      CASE
        WHEN RiparianDeparture IS NOT NULL AND RiparianDeparture >= 0
        THEN RiparianDeparture * area
      END
    )
    / SUM(
      CASE
        WHEN RiparianDeparture IS NOT NULL AND RiparianDeparture >= 0
        THEN area
      END
    ) AS RiparianDeparture

  FROM clipped

  GROUP BY
    huc12

  ) TO 'data/WY_pw_huc12s_rcat.parquet'
  (FORMAT PARQUET);

", rcat_read))


# ---- summarize BRAT ----
dbExecute(con, sprintf("
  COPY (
  
  WITH huc12s AS (
    SELECT 
      huc12, 
      geom 
    
    FROM ST_Read('data/WY_pw_huc12s.gpkg')
  ),

  brat AS (
    SELECT
      CASE Opportunity
        WHEN 'Encourage Beaver Expansion/Colonization' THEN 6
        WHEN 'Conservation/Appropriate for Translocation' THEN 5
        WHEN 'Beaver Mimicry' THEN 4
        WHEN 'Potential Floodplain/Side Channel Opportunities' THEN 3
        WHEN 'Conflict Management' THEN 2
        WHEN 'Land Management Change' THEN 1
        WHEN 'Natural or Anthropogenic Limitations' THEN 0
        ELSE NULL
      END AS Opportunity,
      ST_Transform(
        geom,
        'EPSG:4326',
        'EPSG:5070',
        true
      ) AS geom

    FROM (
      %s
    )
  ),

  clipped AS (
    SELECT
      h.huc12,
      ST_Area(
        ST_Intersection(
          b.geom,
          h.geom
        )
      ) AS area,
      b.* EXCLUDE (geom)

    FROM brat AS b

    JOIN huc12s AS h
      ON ST_Intersects(b.geom, h.geom)
  )

  SELECT
    huc12,

    SUM(
      CASE
        WHEN Opportunity IS NOT NULL AND Opportunity >= 0
        THEN Opportunity * area
      END
    )
    / SUM(
      CASE
        WHEN Opportunity IS NOT NULL AND Opportunity >= 0
        THEN area
      END
    ) AS Opportunity

  FROM clipped

  GROUP BY
    huc12

  ) TO 'data/WY_pw_huc12s_brat.parquet'
  (FORMAT PARQUET);

", brat_read))


# ---- close database connection ----
dbDisconnect(con, shutdown = TRUE)
