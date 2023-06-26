#' AOI Snap to watershed
#'
#' Creates a watershed based area of interest.  Does this by querying _BCData's_
#' _Freshwater Atlas_
#'
#' @param in_aoi `sf` polygon as the Area of Interest. Needs to be a meters based projection (e.g. Albers, UTM)
#' @param preview Boolean.  `TRUE` will provide a mapview preview of the resulting aoi, the input aoi, and the Freshwater Atlas Watersheds that intersect.
#'
#' @return sf polygon
#'
#' @import magrittr
#' @export
#'
#' @examples
#' aoi_snap_watershed(aoi) ### add more

#in_aoi = aoi

aoi_snap_watershed <- function(in_aoi, preview = TRUE){

  watershed <- bcdata::bcdc_query_geodata("3ee497c4-57d7-47f8-b030-2e0c03f8462a") %>%
    bcdata::filter(INTERSECTS(in_aoi)) %>%
    bcdata::collect()

  box <- PEMprepr::aoi_snap(watershed, "expand")

  if (preview == TRUE) {
      print(mapview::mapview(list(box, in_aoi, watershed)))
  }

  return(box)

}

