#' Create Area of interest
#'
#' @param aoi_dir location of the raw aoi
#' @param out_dir output location default is
#' @param aoi_vec text string of name of raw aoi, default = "aoi.gpkg"
#' @importFrom sf st_read st_write
#' @return TRUE
#' @export
#'
#' @examples
#' create_aoi(aoi = file.path(fid$shape_dir_0010[2], out_dir = file.path(fid$shape_dir_1010[1],aoi_vec = "aoi.gpkg")

create_aoi <- function(aoi_dir = file.path(fid$shape_dir_0010[2]),
                       out_dir = file.path(fid$shape_dir_1010[1]),
                       aoi_vec = "aoi.gpkg"){       ## input raster
  aoi <- sf::st_read(file.path(aoi_dir, aoi_vec))
  aoi_bb <- aoi_snap(aoi, method = "expand")
  aoi <- sf::st_read(file.path(aoi_dir, aoi_vec))
  aoi_ls <- aoi_snap_watershed(aoi, "expand")
  #sf::st_write(aoi, file.path(out_dir,"aoi.gpkg"), append = FALSE)
  sf::st_write(aoi_bb, file.path(out_dir,"aoi_snapped.gpkg"), append = FALSE)
  sf::st_write(aoi_ls, file.path(out_dir,"aoi_ls_snap.gpkg"), append = FALSE)

  return(TRUE)
}
