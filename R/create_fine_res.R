#' Create high resolution versions of rasters
#'
#' Uses `terra::disagg` to push a set of rasters to a target resolution.  Note that input raster resolution need to be divisible by the target resolution.
#'
#' @param inputFileList a character vector specifying the location of all the input rasters .  Input list should only be a raster type (e.g. .tif).  Best practice is to use list.files(full.names = TRUE).
#' @param output destination of the output files. _NOTE_ files names will be
#' @param targetRes desired resolution to convert to.
#' @keywords disaggregate
#' @export
#' @examples
#' l <- list.files("e:/covariates/10")
#' create_fine_res(l, output = "e:/covariates/5", targetRes = 5)


create_fine_res <- function(inputFileList, output = "./10_clean_inputs/covariates/5", targetRes = 5){

  ## Error checking
  ## starting resolution / Target resolution should be a whole number

  ifelse(!dir.exists(file.path(output)),              #if tmpOut Does not Exists
         dir.create(file.path(output), recursive = TRUE), "Directory Already Exisits")        #create tmpOut

  for(i in inputFileList){
    ### testing parms
    # i  <- inputFileList[1]
    # targetRes <- 2.5
    # output = "e:/tmp/2.5/"

    print(paste("Processing:", i))
    # r  <- raster::raster(i)
    r  <- terra::rast(i)
    px <- terra::res(r)[1]
    r  <- terra::disagg(r, px/targetRes)  ## This will throw an error if not an integer


    ## naming the output
    nm <- basename(i)
    nm <- unlist(strsplit(nm, split = "[.]"))
    outName <- paste0(output, "/",     ## path
                      nm[1],           ## name
                      "_", px,         ## org res suffix
                      ".", nm[2])           ## original file format
    # print(outName)
    terra::writeRaster(r, outName, overwrite = TRUE)
  }
}
