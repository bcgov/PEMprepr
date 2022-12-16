#' Create covariates in parallel
#'
#' @inheritParams create_covariates
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

  parallel::clusterExport(cl, 'covariate_file_names')
  parallel::clusterExport(cl, 'recursive_layers_call')

  `%dopar%` <- foreach::`%dopar%`
  `%:%` <- foreach::`%:%`

  create_covariates <- create_covariates


  loop <- foreach::foreach(i = layers, .options.snow = opts) %:%

    foreach::foreach(j = files) %dopar% {

      create_covariates(dtm = j, SAGApath = SAGApath, output = output, layers = i)

    }

  close(pb)

  #--- End parallel ---#
  snow::stopCluster(cl)

}
