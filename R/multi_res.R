#' Multi-resolutions
#'
#' Provides multiple resolutions of the input raster
#' Resamples raster to specified desired resolution using the raster package.
#' NOTE: an attempt was made to use the SAGA resample but I had trouble aligning the rasters. I am sure that gdal can be used but for now funtioning is internal to R packages.
#'
#' For the PEM project we generally start with a high resolution dtm.  Resampling to a set of lower resolution DTMs is used to capture multi-scale influences on the ecological unit.
#' NOTE: to ensure raster stacking of all covariates this operation should be completed only on data that has been cropped to the AOI using aoi_snap()
#'
#' @param input a \code{raster::raster} object (not a file)
#' @param resolution = c(5, 10, 25)  multiple resolutions can be specified
#' @param output location of the out rasters
#' @export
#' @examples
#' ## generate default raster sizes
#' multi_res(r)


multi_res <- function(input, output="CoVars",
                      resolution = c(2.5, 5, 10, 25)) {


  # ##testing
  # setwd("e:/workspace/2019/PEM_2020/data/")
  # input <- raster::raster("dtm_cropped.tif")
  # resolution <- c(2.5, 5, 10, 25)
  # iMetrics <- raster(input)
  # # r <- rgdal::readGDAL(input)
  # rtn <- getwd()
  # # setwd("../data/")
  # output <- "CoVars"
  # # SAGApath <- "C:/SAGA/"

  # OUTPUTS: ------------------------------------------------------------
  ifelse(!dir.exists(file.path(output)),              #if tmpOut Does not Exists
         dir.create(file.path(output), recursive = TRUE), "Directory Already Exisits")        #create tmpOut

  ## Load input and get information from it
  r <- input
  e <- as.vector(raster::extent(r))
  # e
  proj <- raster::crs(r)
  raster::res(r)

  for(i in resolution){
    ##Testing
    # i <- resolution[4]

    ## create a target raster:: to tun the 1m to 2.5m raster -- contrained to the extent
    target <- raster::raster(ncol=10, nrow=10, xmn=e[1], xmx=e[2], ymn=e[3], ymx=e[4]) ## empty raster
    raster::res(target) <- i ## Makes target resolution
    raster::projection(target) <-  raster::crs(r)
    # target

    r2 <- raster::resample(r, target)  ## resamples the DTM to the target specified
    # raster::plot(r2)
    # raster::extent(r2)

    outdir <- paste(output, i, sep = "/")
    ifelse(!dir.exists(file.path(outdir)),              #if tmpOut Does not Exists
           dir.create(file.path(outdir)), "Directory Already Exisits")        #create tmpOut

    ## Name Adjustments
    outname <- input@data@names[1]
    outsuf <- paste0("_", i, ".tif")
    outname <- paste0(outname, outsuf)

    raster::writeRaster(r2, paste(outdir, outname, sep = "/"), overwrite = TRUE)

    r <- r2 ## reassigning raster for resampling -- step-wise resampling instead of resamping all from original.

  }
}


## confirms same extent
# library(raster)
# l <- list.files("CoVars/", pattern = "*.tif", recursive = TRUE, full.names = TRUE)
#
# for(i in l){
# c <- raster(i)
# print(as.vector(extent(c)))
# }
