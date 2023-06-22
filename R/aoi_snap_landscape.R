#' Snap Area of Interest
#'
#' Adjusts the area of interest to the nearest 100m.
#' Note that this package assumes to data is in a metric equal area projection.
#'
#' This is an essential first step.  As subsequent co-variate layers will be generated at multiple resolutions (e.g. 5, 10, 25m^2) and then disaggregate'd back to the lowest resolution.
#' Having the aoi set 100m break-points facilitates this.
#'
#' @param aoi is a sf object (e.g. polygon). The bounding box of the shape will be used to create rectangular shape.
#' @param method Options are _shrink_ or _expand_. _Shrink_ will snap the aoi in to the nearest 100m. _Expand_ will snap the aoi out to the nearest 100m.
#' @return a sf polygon
#' @keywords AOI, polygon
#' @export
#' @examples
#' ## Load sf object
#' aoi_raw <- sf::st_read(dsn = "../data/Block_aoi.gpkg", quiet = TRUE)
#' ## snap aoi to nearest 100m
#' aoi_snap(aoi_raw)
#' ##
#' ## [1] "initial extent is:"
#' ## xmin      ymin      xmax      ymax
#' ## 559691.2 5994955.0  560687.0 5995832.5
#' ## [1] "Expanded extent is:"
#' ## xmin    ymin    xmax    ymax
#' ## 559600 5994900  560700 5995900


aoi_snap <- function(aoi, method=c("expand","shrink")){
  ## testing
  # setwd("e:/workspace/2019/PEM_2020/PEMWorkFlow/")
  # aoi <- sf::st_read("../data/Block_aoi.gpkg")
  bb <- sf::st_bbox(aoi)

  ## Function
  print("initial extent is:")
  print(bb)

  if (method == "expand") {
    ## Generate expanded bbox -- expands to neared 100m
    xmin <- floor(bb$xmin / 100)*100 -5000
    xmax <- ceiling(bb["xmax"] / 100) * 100 +5000
    ymin <- floor((bb$ymin / 100)*100) - 5000
    ymax <- ceiling((bb["ymax"] / 100) * 100) + 5000

  } else if (method == "shrink") {

    xmin <- ceiling(bb$xmin / 100)*100
    xmax <- floor(bb["xmax"] / 100) * 100
    ymin <- ceiling(bb$ymin / 100)*100
    ymax <- floor(bb["ymax"] / 100) * 100

  }

  box <- matrix(c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin), ncol = 2, byrow = TRUE)
  box <- sf::st_polygon(list(box))
  box <- sf::st_sfc(box, crs=sf::st_crs(aoi))
  box <- sf::st_as_sf(box)

  ## Report and Return
  print("Extent is:")
  print(sf::st_bbox(box))
  return(box)

}
