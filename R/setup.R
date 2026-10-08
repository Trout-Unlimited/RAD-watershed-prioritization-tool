library(dplyr)
library(sf)
library(duckdb)
library(DBI)
library(arcgislayers)
library(riverscapesR)
library(curl)
library(callr)

con <- dbConnect(duckdb(shared_home = FALSE))   # create duckdb connection 
dbExecute(con, "INSTALL spatial;")              # install spatial extension
dbExecute(con, "LOAD spatial;")                 # load spatial extension
