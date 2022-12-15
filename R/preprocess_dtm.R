#' Pre-process dtm
#'
#' @description pre-process dtm to use in create_covariates()
#'
#' @param dtm file path or SpatRaster. dtm of interest
#' @param template file path or SpatRaster. Template to align dtm
#' @param odir file path. Output directory
#' @param tile Logical. If \code{TRUE}, \code{dtm} will be tiled. Defaults to \code{FALSE}
#' @param ncol Numeric. Number of columns to tile \code{dtm}.
#' @param nrow numeric. Number of rows to tile \code{dtm}.
#' @param tilebuffer Numeric. Distance (in dtm units) to buffer tiles if desired
#' @param writeoutputs Logical. Write outputs to disk if \code{TRUE}
#' @param overwrite Logical. Overwrite previous files
#'
#' @export


preprocess_dtm <- function(dtm,
                           template = NULL,
                           odir,
                           tile = FALSE,
                           ncol = 5,
                           nrow = 10,
                           tilebuffer = NULL,
                           writeoutputs = TRUE,
                           overwrite = TRUE){

  if(is.character(dtm) & file.exists(dtm)){

    dtm <- terra::rast(x = dtm)

  } else if(!inherits(dtm,"SpatRaster")){

    stop(paste0(dtm," needs to be a valid file path or a an image readable with terra::rast()"), call. = FALSE)

  }

  if(!(is.null(template))){
    if(is.character(template) & file.exists(template)){

      template <- terra::rast(x = template)

    } else if(!inherits(template,"SpatRaster")){

      stop(paste0(template," needs to be a valid file path or a an image readable with terra::rast()"), call. = FALSE)

    }
  }

  if(!is.character(odir)){

    stop(paste0("`odir` must be type character."), call. = FALSE)

  }

  if(!is.logical(tile)){

    stop(paste0("`tile` must be type logical"), call. = FALSE)

  }

  if(!is.numeric(ncol)){

    stop(paste0("`ncol` must be type numeric"), call. = FALSE)

  }

  if(!is.numeric(nrow)){

    stop(paste0("`nrow` must be type numeric"), call. = FALSE)

  }

  if(!is.numeric(tilebuffer)){

    stop(paste0("`tilebuffer` must be type numeric"), call. = FALSE)

  }

  if(!is.logical(writeoutputs)){

    stop(paste0("`writeoutputs` must be type logical"), call. = FALSE)

  }

  if(!is.logical(overwrite)){

    stop(paste0("`overwrite` must be type logical"), call. = FALSE)

  }

  #--- check outputs ---#
  odirinputs <- file.path(odir,"inputs","processed")

  if(!file.exists(odirinputs)) dir.create(odirinputs, recursive = TRUE)

  #--- align to template ---#
  if(!is.null(template)){

    message("aligning DEM to template.")
    dtma <- align_raster(iraster = dtm, rtemplate = template)

    if(isTRUE(writeoutputs)){
      terra::writeRaster(dtma, file.path(odirinputs,"dtm_aligned.tif"), overwrite = overwrite)
    }
  }

  #--- tile dtm ---#
  if(!is.null(tilebuffer)){
    message("tileing.")
    ####--- tile dtm with overlap for faster efficient processing ---####
    t <- terra::rast(ncols=ncol, nrows=nrow, ext = terra::ext(dtma), crs = terra::crs(dtma))

    #--- overlapping tiles ---#
    tv <- terra::as.polygons(t)

    #--- check outputs ---#
    dir.create(file.path(odir,"tiles"))

    tv |> terra::writeVector(file.path(odir,"tiles","tile_extent.shp"))

    tp <- terra::as.polygons(t) |> buffer(tilebuffer)

    #--- check outputs ---#
    terra::makeTiles(x = dtma, y = tp, file.path(odir,"tiles","tile_.tif"))
  }

}
