#' Create a raster template from AOI
#'
#' Sets the baseline raster template to align and standardize all raster predictor layers
#' This package assumes to data is in a metric equal area projection, most likely BCAlbers
#'
#' This is an essential first step.  As subsequent co-variate layers will be generated at multiple resolutions (e.g. 5, 10, 25m^2) and then disaggregated back to the lowest resolution.
#' Having the aoi set 100m break-points facilitates this.
#'
#' @param aoi_bb is a sf or terra::vect object bounding box created expanded in aoi_snap function(e.g. polygon).  Should be a meter based projection
#' @param res desired resolution of the raster (in meters)
#' @param filename text name of the output file ie: template.tif
#' @param outpath output path.  Note that the results will be placed in a subfolder labelled with the resolution.
#'
#' @return a terra raster and saves in correct filepath
#' @keywords aoi, raster, template
#' @export
#' @examples
#' create_template(aoi_bb = file.path(fid$shape_dir_1010[1], "aoi_snapped.gpkg"), res = 10, outpath = fid$cov_dir_1020[2], filename = "template.tif")

create_template <- function(aoi_bb = file.path(fid$shape_dir_1010[2], "aoi_snapped.gpkg"),
                            res = 25, outpath = fid$cov_dir_1020[2],
                            filename = "template.tif"){

  aoi_bb  <- terra::vect(aoi_bb)

  template <- terra::rast(aoi_bb , resolution = res)
  terra::values(template) <- 0

  outpath <- file.path(outpath, paste0(res, 'm'))
  if(!exists(outpath)) {dir.create(outpath, recursive = TRUE) }


  # write out
  terra::writeRaster(template, file.path(outpath, filename), overwrite = TRUE)

  # print(paste("Template raster create and saved as:", file.path(outpath, "template.tif")))
  return(template)

}
