#' copy_clean_vectors
#'
#' @param origindir text filepath of raw vectors
#' @param targetdir text filepath of clean vectors
#'
#' @return TRUE
#' @export
#'
#' @examples
#' copy_clean_vectors(origindir = fid$shape_dir_0010[1],targetdir = fid$shape_dir_1010[1])

copy_clean_vectors <- function(origindir = fid$shape_dir_0010[1],
                               targetdir = fid$shape_dir_1010[1]){

  filestocopy <- list.files(path = origindir, pattern = ".gpkg",
                            recursive = TRUE)
  lapply(filestocopy, function(x) file.copy(paste(origindir, x, sep = "/"),
                                            paste(targetdir, x, sep = "/"), recursive = FALSE, copy.mode = TRUE))

  return(TRUE)

  }
