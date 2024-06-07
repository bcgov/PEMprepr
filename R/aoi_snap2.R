#' Snap Area of Interest
#'
#' Adjusts the area of interest to the nearest 100m.
#' Note that this package assumes to data is in a metric equal area projection.
#'
#' This is an essential first step.  As subsequent co-variate layers will be generated at multiple resolutions (e.g. 5, 10, 25m^2) and then disaggregate'd back to the lowest resolution.
#' Having the AOI set 100m break-points facilitates this.
#'
#' @param f.ids user folder list where all data is stored
#' @param method Options are _shrink_ or _expand_. _Shrink_ will snap the aoi in to the nearest 100m. _Expand_ will snap the AOI out to the nearest 100m.
#' @param buffer adds additional buffer to expand AOI bounding box
#' @return a sf polygon
#' @keywords AOI, polygon
#' @export
#' @examples
#' ## Load sf object
#' ## snap aoi to nearest 100m
#' aoi_snap2(ufl = f.ids, method = "expand")
#' ##
#' ## [1] "initial extent is:"
#' ## xmin      ymin      xmax      ymax
#' ## 559691.2 5994955.0  560687.0 5995832.5
#' ## [1] "Expanded extent is:"
#' ## xmin    ymin    xmax    ymax
#' ## 559600 5994900  560700 5995900


aoi_snap2 <- function(f.ids, method = c("expand","shrink"), buffer = 0, redo = FALSE){

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
