#' Create a raster template from AOI
#'
#' Sets the baseline raster template to align and standardize all raster predictor layers
#' This package assumes to data is in a metric equal area projection, most likely BCAlbers
#'
#' This is an essential first step.  As subsequent co-variate layers will be generated at multiple resolutions (e.g. 5, 10, 25m^2) and then disaggregated back to the lowest resolution.
#' Having the aoi set 100m break-points facilitates this.
#'
#' @param f.ids folder IDs
#' @param res integer vector of desired resolution of the raster (in meters)
#'
#' @return a terra raster
#' @keywords aoi, raster, template
#' @export
#' @examples
#' Put examples here

create_template <- function(f.ids, res = c(5, 10, 15, 20, 25, 30)){

  for(i in 1:length(res)){
    out.file <- check_output(f.ids, obj.name = "aoi.snapped")
  }

  out.file <- check_output(f.ids, obj.name = "aoi.snapped")

  if(redo == TRUE | is.na(out.file)){

    aoi <- sf::st_read(check_output(f.ids, "aoi.orig"))

    bb <- sf::st_bbox(aoi)

    ## Function
    print("Initial extent is:")
    print(bb)

    if (buffer > 0 & method == "expand") {
      ## Generate expanded bbox -- expands to neared 100m
      xmin <- floor((bb$xmin - buffer) / 100)*100
      xmax <- ceiling((bb["xmax" ]+ buffer) / 100) * 100
      ymin <- floor((bb$ymin - buffer) / 100)*100
      ymax <- ceiling((bb["ymax"] + buffer) / 100) * 100


    } else if (method == "expand") {
      ## Generate expanded bbox -- expands to neared 100m
      xmin <- floor(bb$xmin / 100)*100
      xmax <- ceiling(bb["xmax"] / 100) * 100
      ymin <- floor(bb$ymin / 100)*100
      ymax <- ceiling(bb["ymax"] / 100) * 100

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
    print("Snapped extent is:")
    print(sf::st_bbox(box))

    push_output(obj = box, obj.name = "aoi.snapped", f.ids = f.ids)

  }

  return(check_output(f.ids = f.ids, obj.name = "aoi.snapped"))


}
