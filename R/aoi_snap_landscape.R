# creates a aoi template at a larger scale for use in landscape covariates.
# set to a watershed boundary

#library(sf)
library(bcdata)
#library(dplyr)
#library(foreach)

# testing
in_aoi <- sf::st_read(file.path('temp', "aoi.gpkg"))
#out_path = shape_raw_dir <- file.path('temp')


aoi_snap_landscape <- function(in_aoi, out_path = shape_raw_dir){

  # Uses date filter which filters cutblock ages less than 20 years, or 7305 days
  watershed <- bcdc_query_geodata("3ee497c4-57d7-47f8-b030-2e0c03f8462a") %>%
    bcdata::filter(INTERSECTS(in_aoi)) %>%
    collect()

  bb <- sf::st_bbox(watershed)

  xmin <- floor(bb$xmin / 100)*100
  xmax <- ceiling(bb["xmax"] / 100) * 100
  ymin <- floor(bb$ymin / 100)*100
  ymax <- ceiling(bb["ymax"] / 100) * 100

  box <- matrix(c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin), ncol = 2, byrow = TRUE)
  box <- sf::st_polygon(list(box))
  box <- sf::st_sfc(box, crs=sf::st_crs(in_aoi))
  box <- sf::st_as_sf(box)

  print("Extent is:")
  print(sf::st_bbox(box))

  return(box)

}

