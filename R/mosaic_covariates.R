#' Mosaic covariates
#'
#' @param dir Character. Directory where covaraites exist
#' @param output Character. Directory to write mosaiced covaraites
#' @param poly spatVector. Original tile boundaries generated using \code{preprocess_dtm}
#' @param tmp Character. Temp folder directory to be used for mosaicing.
#' Deleted upon completion of function.
#'
#' @export

mosaic_covariates <- function(dir,
                              output,
                              tmp,
                              poly){

  if(!is.character(dir)){

    stop(paste0("`dir` must be type character."), call. = FALSE)

  }

  if(!is.character(output)){

    stop(paste0("`output` must be type character."), call. = FALSE)

  }

  if(!is.character(tmp)){

    stop(paste0("`tmp` must be type character."), call. = FALSE)

  }

  if(!inherits(poly, "SpatVector")){

    stop(paste0("`poly` must be type SpatVector."), call. = FALSE)

  }

  #--- make output and tmp if it doesnt already exist ---#

  dir.create(output, recursive = TRUE)
  dir.create(tmp, recursive = TRUE)

  #--- read in poly vector ---#
  poly <- terra::vect(poly)

  #--- list file directory rasters to mosaic ---#
  f <- list.files(dir, pattern = ".sdat$")

  #--- filter unique metric names in files ---#
  l <- unique(unlist(stringr::str_split(f,pattern = "\\s*([[:digit:]]+)_")))

  l1 <- l[!l %in% "tile_"]

  #--- mosaic metrics ---#
  for(i in 1:length(l1)){

    f <- list.files(dir, pattern = paste0(l1[i],"$"), full.names = TRUE)

    #--- define output mosaic name ---#
    out <- paste0(output,tools::file_path_sans_ext(basename(l1[i])),".tif")

    #--- write mosaiced outputs ---#

    if(!file.exists(out)){

      #--- read and crop overlapping tiles to polygon extent ---#

      walk(.x = f, .f = read_crop, poly = poly, tmp = tmp)

      tmpf <- list.files(tmp, full.names = TRUE, pattern = ".sdat$")

      v <- terra::vrt(tmpf)

      message(paste0("mosaicing -- ",l1[i]))

      terra::writeRaster(v, paste0(output,tools::file_path_sans_ext(basename(l1[i])),".tif"))

      #--- delete tmp directory files ---#
      do.call(file.remove, list(list.files(tmp, full.names = TRUE)))

    } else {

      message("exists")
    }
  }

  #--- delete temp directory ---#

  unlink(tmp, recursive = TRUE)

}
