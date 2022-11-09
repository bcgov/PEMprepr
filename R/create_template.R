#' Create a raster template from AOI
#'
#' Sets the baseline raster template to align and standardize all raster predictor layers
#' This package assumes to data is in a metric equal area projection most likely BCAlbers
#'
#' This is an essential first step.  As subsequent co-variate layers will be generated at multiple resolutions (e.g. 5, 10, 25m^2) and then disaggregate'd back to the lowest resolution.
#' Having the aoi set 100m break-points facilitates this.
#'
#' @param aoi is a sf or terra::vect object bounding box created expanded in aoi_snap function(e.g. polygon).  Should be a meter based projection
#' @param res desired resolution of the raster (in meters)
#' @param outpath output path.  Note that the results will be placed in a subfolder labelled with the resolution.
#' @return a terra raster.  _NOT CURRENTLY_ saving raster objects
#' @keywords aoi, raster, template, crop, align
#' @export
#' @examples
#'
#' create_template(aoi, 10)

## TESTING ----------
## REMOVE  --
# read in the aoi
# res = 10
# aoi <- terra::vect( "./temp_data/aoi.gpkg")
# setupfolders("CanyonCreek2")

# output = cov_dir
## ------------------


create_template <- function(aoi, res#, dropped saving for now
                            #outpath = c(NULL, "default", "yourpath")
                            ){
  if (class(aoi)[1] == "sf") {
    aoi <- terra::vect(aoi)
  }

  template <- terra::rast(aoi, resolution = res)
  terra::values(template) <- 0

  # outpath <- file.path(outpath, res)
  # if(!exists(outpath)) {dir.create(outpath, recursive = TRUE) }


  # if (is.null(outpath)) {
  # terra::writeRaster(template, file.path(outpath, "template.tif"), overwrite = TRUE)
  # }


  # print(paste("Template raster create and saved as:", file.path(outpath, "template.tif")))
  return(template)
}


## -----------------------------------

