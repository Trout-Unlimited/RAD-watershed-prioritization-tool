# Pull NorWeST and FlowMet data into duckdb and compute length-weighted 
# watershed summaries

# load packages and connect to duckdb
source("R/setup.R")

bbox <- st_as_text(
  st_as_sfc(
    st_bbox(
      st_transform(
        st_read(
          "data/WY_pw_huc12s.gpkg", 
          quiet = TRUE
        ), 4269
      )
    )
  )
)

# ---- NorWeST ----
# gdb path
norwest_url <- c(norwest = "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.NorWeST_PredictedStreams.gdb.zip")

dir.create("data/norwest")
norwest <- file.path(
  "data/norwest", 
  paste0(names(norwest_url), ".gdb.zip"))

options(timeout = 1200)
download.file(norwest_url, norwest)

norwest_path <- paste0("/vsizip/", norwest)

# check norwest layer name and crs
dbGetQuery(con, sprintf("
  SELECT * 
  
  FROM ST_Read_Meta('%s')
", norwest_path))

# check norwest field names
names(
  dbGetQuery(con, sprintf("
    SELECT * 
    
    FROM ST_Read('%s', layer = 'NorWeST_PredictedStreams') LIMIT 0
  ", norwest_path))
)

# summarize norwest
dbExecute(con, sprintf("
  COPY (

  WITH huc12s AS (
    SELECT 
      huc12, 
      geom 
    
    FROM ST_Read('data/WY_pw_huc12s.gpkg')
  ),

  norwest_filtered AS (
    SELECT
      * EXCLUDE (SHAPE),
      ST_Transform(
        SHAPE, 
        'EPSG:4269', 
        'EPSG:5070', 
        true
      ) AS geom
 
    FROM ST_Read(
      '%s', layer = 'NorWeST_PredictedStreams'
    )
 
    WHERE ST_Intersects(
      SHAPE,
      ST_GeomFromText('%s')
    )
  ),

  norwest AS (
    SELECT
      *,
    
      -- calculate percent and absolute changes between historic and mid-century/end-of-century values
      S30_2040D - S1_93_11 AS st_Abs_Chg_2040,
      S32_2080D - S1_93_11 AS st_Abs_Chg_2080,
      (S30_2040D - S1_93_11) / S1_93_11 * 100 AS st_Pct_Chg_2040,
      (S32_2080D - S1_93_11) / S1_93_11 * 100 AS st_Pct_Chg_2080
 
    FROM norwest_filtered
  ),

  clipped AS (
    SELECT
      h.huc12,
      ST_Length(
        ST_Intersection(
          n.geom, 
          h.geom
        )
      ) AS seg_length,
      n.* EXCLUDE (geom)

  FROM norwest AS n
  
  JOIN huc12s AS h
    ON ST_Intersects(
      n.geom, 
      h.geom
    )
  )
  
  SELECT
    huc12, 
    SUM(st_Abs_Chg_2040 * seg_length) 
      / SUM(
        CASE 
          WHEN st_Abs_Chg_2040 IS NOT NULL
          THEN seg_length 
        END
      ) AS st_Abs_Chg_2040,
    SUM(st_Abs_Chg_2080 * seg_length) 
      / SUM(
        CASE 
          WHEN st_Abs_Chg_2080 IS NOT NULL 
          THEN seg_length 
        END
      ) AS st_Abs_Chg_2080,
    SUM(st_Pct_Chg_2040 * seg_length) 
      / SUM(
        CASE 
          WHEN st_Pct_Chg_2040 IS NOT NULL 
          THEN seg_length 
        END
      ) AS st_Pct_Chg_2040,
    SUM(st_Pct_Chg_2080 * seg_length) 
      / SUM(
        CASE 
          WHEN st_Pct_Chg_2080 IS NOT NULL 
          THEN seg_length 
        END
      ) AS st_Pct_Chg_2080
  
  FROM clipped
  
  GROUP BY
    huc12, 

  ) TO 'data/WY_huc12s_norwest.parquet' (FORMAT PARQUET);

", norwest_path, bbox))


# ---- FlowMet ----
# gdb paths
flowmet_urls <- c(
  p40 = "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.Hydro_FlowMet_2040sChgPct.gdb.zip",
  p80 = "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.Hydro_FlowMet_2080sChgPct.gdb.zip",
  a40 = "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.Hydro_FlowMet_2040sChgAbs.gdb.zip",
  a80 = "https://data.fs.usda.gov/geodata/edw/edw_resources/fc/S_USA.Hydro_FlowMet_2080sChgAbs.gdb.zip"
)

dir.create("data/flowmet")
flowmet <- file.path(
  "data/flowmet", 
  paste0(names(flowmet_urls), ".gdb.zip"))

multi_download(flowmet_urls, flowmet, resume = TRUE)

names(flowmet) <- names(flowmet_urls)

p40_path <- paste0("/vsizip/", flowmet["p40"])
p80_path <- paste0("/vsizip/", flowmet["p80"])
a40_path <- paste0("/vsizip/", flowmet["a40"])
a80_path <- paste0("/vsizip/", flowmet["a80"])

# check layer names for a single flowmet gdb
dbGetQuery(con, sprintf("
  SELECT *
  
  FROM ST_Read_Meta('%s')
", p40_path))

# check field names for a single layer
names(dbGetQuery(con, sprintf("
  SELECT *
  
  FROM ST_Read('%s', layer = 'Hydro_FlowMet_2040sChgPct') LIMIT 0
", p40_path)))

# summarize flowmet
dbExecute(con, sprintf("
  COPY (

  WITH huc12s AS (
    SELECT 
      huc12, 
      geom 
    FROM ST_Read('data/WY_pw_huc12s.gpkg')
  ),

flowmet_filtered AS (
    SELECT
      p40.COMID,
      p40.FTYPE,
      p40.TOTDASQKM,
      ST_Transform(
        p40.SHAPE, 
        'EPSG:4269', 
        'EPSG:5070', 
        true
      ) AS geom,
      p40.MA_P2040,
      p40.MJJA_P2040,
      p40.HIQ1_5_P2040
 
    FROM ST_Read(
      '%s', 
      layer = 'Hydro_FlowMet_2040sChgPct'
    ) AS p40
 
    WHERE p40.FTYPE = 460
      AND p40.TOTDASQKM <= 10000
      AND ST_Intersects(
        p40.SHAPE,
        ST_GeomFromText('%s')
      )
  ),
 
  p80_filtered AS (
    SELECT 
      COMID, 
      MA_P2080, 
      MJJA_P2080, 
      HIQ1_5_P2080
 
    FROM ST_Read(
      '%s', 
      layer = 'Hydro_FlowMet_2080sChgPct'
    )
 
    WHERE ST_Intersects(
      SHAPE, 
      ST_GeomFromText('%s')
    )
  ),
 
  a40_filtered AS (
    SELECT 
      COMID, 
      MA_A2040, 
      MJJA_A2040, 
      HIQ1_5_A2040, 
      CFM_A2040
 
    FROM ST_Read(
      '%s', 
      layer = 'Hydro_FlowMet_2040sChgAbs'
    )
 
    WHERE ST_Intersects(
      SHAPE, 
      ST_GeomFromText('%s')
    )
  ),
 
  a80_filtered AS (
    SELECT 
      COMID, 
      MA_A2080, 
      MJJA_A2080,
      HIQ1_5_A2080, 
      CFM_A2080
 
    FROM ST_Read(
      '%s', 
      layer = 'Hydro_FlowMet_2080sChgAbs'
    )
 
    WHERE ST_Intersects(
      SHAPE, 
      ST_GeomFromText('%s')
    )
  ),
 
flowmet AS (
  SELECT
    f.geom,
    f.COMID,
    f.MA_P2040, 
    f.MJJA_P2040, 
    f.HIQ1_5_P2040,                 
    p80.MA_P2080, 
    p80.MJJA_P2080, 
    p80.HIQ1_5_P2080,                  
    a40.MA_A2040, 
    a40.MJJA_A2040, 
    a40.HIQ1_5_A2040, 
    a40.CFM_A2040,   
    a80.MA_A2080, 
    a80.MJJA_A2080, 
    a80.HIQ1_5_A2080, 
    a80.CFM_A2080    
 
  FROM flowmet_filtered AS f
 
  JOIN p80_filtered AS p80 
    ON f.COMID = p80.COMID
 
  JOIN a40_filtered AS a40 
    ON f.COMID = a40.COMID
 
  JOIN a80_filtered AS a80 
    ON f.COMID = a80.COMID
),

  clipped AS (
    SELECT
      h.huc12, 
      ST_Length(
        ST_Intersection(f.geom, h.geom)
      ) AS seg_length,
      f.* EXCLUDE (geom)
    
    FROM flowmet AS f
    
    JOIN huc12s h 
      ON ST_Intersects(f.geom, h.geom)
  )
  
  SELECT
    huc12, 

    -- 2040 percent change
    SUM(MA_P2040 * seg_length) 
      / SUM(
        CASE 
          WHEN MA_P2040 IS NOT NULL 
          THEN seg_length 
        END
      ) AS MA_P2040,
    SUM(MJJA_P2040 * seg_length) 
      / SUM(
        CASE 
          WHEN MJJA_P2040 IS NOT NULL 
          THEN seg_length 
        END
      ) AS MJJA_P2040,
    SUM(HIQ1_5_P2040 * seg_length) 
      / SUM(
        CASE WHEN HIQ1_5_P2040 IS NOT NULL 
        THEN seg_length END
      ) AS HIQ1_5_P2040,
    
    -- 2040 absolute change
    SUM(MA_A2040 * seg_length) 
      / SUM(
        CASE 
          WHEN MA_A2040 IS NOT NULL 
          THEN seg_length 
        END
      ) AS MA_A2040,
    SUM(MJJA_A2040 * seg_length) 
      / SUM(
        CASE 
          WHEN MJJA_A2040 IS NOT NULL 
          THEN seg_length END
      ) AS MJJA_A2040,
    SUM(HIQ1_5_A2040 * seg_length) 
      / SUM(
        CASE 
          WHEN HIQ1_5_A2040 IS NOT NULL 
          THEN seg_length 
        END
      )  AS HIQ1_5_A2040,
    SUM(CFM_A2040 * seg_length) 
      / SUM(
        CASE 
          WHEN CFM_A2040 IS NOT NULL 
          THEN seg_length 
        END
      ) AS CFM_A2040,
    
    -- 2080 percent change
    SUM(MA_P2080 * seg_length) 
      / SUM(
        CASE 
          WHEN MA_P2080 IS NOT NULL 
          THEN seg_length END
        ) AS MA_P2080,
    SUM(MJJA_P2080 * seg_length) 
      / SUM(
        CASE 
          WHEN MJJA_P2080 IS NOT NULL 
          THEN seg_length 
        END
      ) AS MJJA_P2080,
    SUM(HIQ1_5_P2080 * seg_length) 
      / SUM(
        CASE 
          WHEN HIQ1_5_P2080 IS NOT NULL 
          THEN seg_length 
        END
      ) AS HIQ1_5_P2080,
    
    -- 2080 absolute change
    SUM(MA_A2080 * seg_length) 
      / SUM(
        CASE 
          WHEN MA_A2080 IS NOT NULL 
          THEN seg_length END
      ) AS MA_A2080,
    SUM(MJJA_A2080 * seg_length) 
      / SUM(
        CASE 
          WHEN MJJA_A2080 IS NOT NULL 
          THEN seg_length 
        END
      ) AS MJJA_A2080,
    SUM(HIQ1_5_A2080 * seg_length) 
      / SUM(
        CASE 
          WHEN HIQ1_5_A2080 IS NOT NULL 
          THEN seg_length END
        ) AS HIQ1_5_A2080,
    SUM(CFM_A2080 * seg_length) 
      / SUM(
        CASE
          WHEN CFM_A2080 IS NOT NULL 
          THEN seg_length 
        END
      ) AS CFM_A2080
  
  FROM clipped

  GROUP BY
    huc12, 

  ) TO 'data/WY_pw_huc12s_flowmet.parquet' (FORMAT PARQUET);

", p40_path, bbox, p80_path, bbox, a40_path, bbox, a80_path, bbox))

# ---- close database connection ----
dbDisconnect(con, shutdown = TRUE)
