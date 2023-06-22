#' Create BGC template
#'
#' @param bec_sf sf object with bec linework
#' @param field field within in sf object which will be used to assign raster value
#' @param template raster object of the size in which to model output raster not including the resoution subfolder
#' @param outpath location in which output to be saved, default is based on fid object. Note resolution of template will be used determine folder in which output is saved
#' @importFrom sf st_drop_geometry
#' @importFrom terra vect rasterize writeRaster res
#' @importFrom dplyr select mutate left_join
#' @return write out bgc raster and return object
#' @export
#'
#' @examples
#' bgc_raster = create_bgc_template( bec_sf, field = "MAP_LABEL", template = r25,outpath = fid$cov_dir_1020[2])

create_bgc_template= function(bec_sf, field = "MAP_LABEL", template = r25,
  outpath = fid$cov_dir_1020[2]
){

bec_code <- bec_sf %>% sf::st_drop_geometry()  %>% dplyr::select(any_of(field)) %>%
  unique()

bec_code <- bec_code %>%
  dplyr::mutate(bgc_unique_code = seq(1, length(bec_code[,1]),1))

bec_sf <- dplyr::left_join(bec_sf, bec_code)

bec_vec <- terra::vect(bec_sf)

# generate a 25m raster

bec_ras25 <- terra::rasterize(bec_vec, r25, field = field)
pixal_size = terra::res(r25)[1]

terra::writeRaster(bec_ras25, file.path(outpath, paste0(pixal_size,"m"), "bec.tif"), overwrite = TRUE)


print(paste("Template raster create and saved as:", file.path(outpath, paste0(pixal_size,"m"), "bec.tif")))
  return(bec_ras25)
}

