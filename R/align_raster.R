#' Align raster
#'
#' Forces raster to an specified extent
#'
#' To facilitate stacking of rasters, and rasters of varied resolutions rasters
#' must have the same extent. This function ensure that ..... MORE HERE ...
#'
#' @param raster a `terra::rast` object
#' @param rtemplate a `terra::rast` object
#' @param outpath a character string to save location
#'
#' @export
#'
#' @examples
#' add some examples
#'


align_raster <- function(raster,          ## input raster
                         rtemplate,       ## a terra::rast object
                         outpath = ""){   ## export location

  rtemplate <- terra::rast(rtemplate)
  res <- terra::res(rtemplate)[1]

  ## Error check
  ## 1. Template rexolution in x and y are the same


  raster    <-    terra::rast(covariate, resolution = c(res,res))
  # rtemplate <- terra::rast(rtemplate)
  crs(covariate) <- crs(rtemplate)
  covariate <- terra::crop(dem, rtemplate) %>%
    terra::resample(dem, rtemplate)
  #plot(template)
  terra::writeRaster(covariate, file.path(paste(outpath, resfolder, covariate_name, ".tif", sep = "/")), overwrite = TRUE)
  return(TRUE)
}

