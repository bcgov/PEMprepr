#' Checks for an output file.
#'
#' This function is almost exclusively called internally within other PEMprepr functions. It checks the function output against a list of standardized outputs and write it to a specified place on the disk.
#'
#' @param vari.name the standard name of the variable
#' @param f.ids list of folder IDS
#'
#' @return a list of folder IDs with relative and absolute paths
#' @export
#'
#' @examples
#' check_output(f.ids = f.ids, vari.name = "aoi.snapped")
#' [1] TRUE
check_output <- function(f.ids, obj.name){

  output <- readxl::read_xlsx("inst/extdata/output_list.xlsx") %>%
    dplyr::filter(s.name == obj.name)

  out.file <- stringr::str_c(f.ids[[output$f.ids.path]][2], "/", output$l.name)

  if(file.exists(out.file)){
    return(out.file)
  } else {
    return(NA_character_)
  }

}
