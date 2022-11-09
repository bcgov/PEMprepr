#' Create a raster template from AOI
#'
#' Sets the baseline raster template to align and standardize all raster predictor layers
#' This package assumes to data is in a metric equal area projection most likely BCAlbers
#'
#' This is an essential first step.  As subsequent co-variate layers will be generated at multiple resolutions (e.g. 5, 10, 25m^2) and then disaggregate'd back to the lowest resolution.
#' Having the aoi set 100m break-points facilitates this.
#'
#' @param aoi is a sf object bounding box created expanded in aoi_snap function(e.g. polygon).
#' @param method Options are _shrink_ or _expand_. _Shrink_ will snap the aoi in to the nearest 100m. _Expand_ will snap the aoi out to the nearest 100m.
#' @return a terra raster
#' @keywords aoi, raster, template, crop, align
#' @export
#' @examples

## TESTING ----------
## REMOVE  --
# read in the aoi
# res = 10
# aoi <- terra::vect( "./temp_data/aoi.gpkg")
# setupfolders("CanyonCreek2")

# output = cov_dir
## ------------------


create_template <- function(aoi, res, outpath = "cov_dir"){
  template <- terra::rast(aoi, resolution = c(res,res))
  values(template) <- 1:ncell(template)
  resfolder = paste0(res,"m")

  terra::writeRaster(template, file.path(paste(outpath, resfolder, "template.tif", sep = "/")), overwrite = TRUE)
return(TRUE)
}


## -----------------------------------

