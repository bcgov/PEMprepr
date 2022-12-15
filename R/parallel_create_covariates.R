#' Create covariates in parallel
#'
#' @inheritParams calculate_covariates
#'
#' @param files file paths. Vector of file paths derived using \code{file.path} of tiles to be processed
#' @param cores Numeric. Number of cores to use
#' @param output Character. Path to output directory
#'
#' @export

parallel_create_covariates <- function(files,
                                       layers,
                                       cores,
                                       output,
                                       SAGApath = ""){

  if(!is.character(files)){

    stop(paste0("`files` must be a vector of file paths."), call. = FALSE)

  }

  if(!is.numeric(cores)){

    stop(paste0("`cores` must be type numeric."), call. = FALSE)

  }


  pb <- utils::txtProgressBar(max = length(files)*length(layers), style = 3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  cores = cores
  cl <- snow::makeCluster(spec = cores, type = "SOCK")
  doSNOW::registerDoSNOW(cl)

  loop <- foreach::foreach(f = layers, .packages = c("terra","PEMprepr"), .options.snow = opts) %:%

    foreach::foreach(i = files) %dopar% {

      create_covariates(dtm = i, SAGApath = SAGApath, output = output, layers = f)

    }

  close(pb)

  #--- End parallel ---#
  snow::stopCluster(cl)

}
