#' Align raster
#'
#' Forces raster to an specified extent
#'
#' To facilitate stacking of rasters, and rasters of varied resolutions rasters
#' must have the same extent. This function ensure that ..... MORE HERE ...
#'
#' @param iraster a `terra::rast` object
#' @param rtemplate a `terra::rast` object
#' @param outpath a character string to save location. Note that it will be placed
#' in a subfolder based on the template resolution.
#'
#' @export
#'
#' @examples
#' add some examples
#'


align_raster <- function(iraster,         ## input raster
                         rtemplate,       ## a terra::rast object
                         outpath = c(NULL, "default", "yourpath"),
                         outname = c(NULL, "yourname.tiff")){   ## export location

  if (outpath == "default") {print("Add query default dir")}

  # rtemplate <- terra::rast(rtemplate) ##
  res <- terra::res(rtemplate)[1]

  # iraster    <-    terra::rast(covariate, resolution = res)
  # rtemplate <- terra::rast(rtemplate)
  crs(covariate) <- crs(rtemplate) ### This should be error checked

  out <- terra::crop(dem, rtemplate)
  out <- resample(out, rtemplate)

  #plot(template)

  if (!is.null(outpath)|| !is.null(outname)) { ## CHECK THIS
  terra::writeRaster(covariate, file.path(paste(outpath, resfolder, covariate_name, ".tif", sep = "/")), overwrite = TRUE)
  }
  return(out) ## SpatRaster
}

