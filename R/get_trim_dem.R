
#' Get Trim data form Cded
#'
#' @param aoi_bb filepath to aoi required
#' @param res resolution of raster
#' @param out_dir output drive, default is set to fid$cov_dir_1020[2]
#' @importFrom bcmaps cded_raster
#' @return SpatRast DEM from CDED
#' @importFrom sf st_read
#' @importFrom bcmaps cded_raster
#' @importFrom terra rast project writeRaster
#' @export
#'
#' @examples
#' get_trim_dem(aoi_bb = file.path(fid$shape_dir_1010[2],"aoi_snapped.gpkg"), res = 5, out_dir = fid$cov_dir_1020[2]))
get_trim_dem <- function(aoi_bb = file.path(fid$shape_dir_1010[2],"aoi_snapped.gpkg"),
                         res,
                         out_dir = fid$cov_dir_1020[2]){

    aoi <- sf::st_read(aoi_bb)
    trim_raw <- bcmaps::cded_raster(aoi)
    trim <- terra::rast(trim_raw)

    # check it aligns with template
    template_file <- file.path(fid$cov_dir_1020[2],paste0(res, 'm'), "template.tif")
    template <- terra::rast(template_file)
    trim <- terra::project(trim, template)

    # if there is not a folder already created, then create it.

    outpath <- file.path(out_dir, paste0(res, 'm'))
    if(!exists(outpath)) {dir.create(outpath, recursive = TRUE) }


    terra::writeRaster(trim, file.path(  outpath, "dem.tif"), overwrite = TRUE)
    return(trim)
}
