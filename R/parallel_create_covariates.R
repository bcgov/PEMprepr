#' Create covariates in parallel
#'
#' @inheritParams create_covariates
#'
#' @param files file paths. Vector of file paths derived using \code{file.path} of files to be processed in parallel.
#' @param cores Numeric. Number of cores to use.
#' @param output Character. Path to output directory.
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

  cl <- snow::makeCluster(spec = cores, type = "SOCK")
  doSNOW::registerDoSNOW(cl)

  snow::clusterExport(cl, 'covariate_file_names')
  snow::clusterExport(cl, 'recursive_layers_call')

  `%dopar%` <- foreach::`%dopar%`
  `%:%` <- foreach::`%:%`

  fx <- create_covariates

  loop <- foreach::foreach(f = layers, .packages = c("terra"), .options.snow = opts) %:%

    foreach::foreach(i = files) %dopar% {

      fx(dtm = i, SAGApath = SAGApath, output = output, layers = f)

    }

  close(pb)

  #--- End parallel ---#
  snow::stopCluster(cl)

}
