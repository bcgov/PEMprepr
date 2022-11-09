# creates a aoi template at a larger scale for use in landscape covariates.
# set to a watershed boundary

#library(sf)
library(bcdata)
import(magrittr)

#library(dplyr)
#library(foreach)

# testing
#in_aoi <- sf::st_read(file.path('temp', "aoi.gpkg"))
#out_path = shape_raw_dir <- file.path('temp')


aoi_snap_landscape <- function(in_aoi){

  watershed <- bcdc_query_geodata("3ee497c4-57d7-47f8-b030-2e0c03f8462a") %>%
    bcdata::filter(INTERSECTS(in_aoi)) %>%
    collect()

  box <- PEMprepr::aoi_snap(watershed, "extend")

 # mapview::mapview(list(box, in_aoi, watershed))

  return(box)

}

