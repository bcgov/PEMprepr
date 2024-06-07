#' Reads/Writes function output to user-defined folder set
#'
#' This function is almost exclusively called internally within other PEMprepr functions. It checks the function output against a list of standardized outputs and write it to a specified place on the disk.
#'
#' @param obj the object to be written
#' @param obj.name the standard name of the variable
#' @param f.ids list of folder IDS
#'
#' @return a list of folder IDs with relative and absolute paths
#' @export
#'
#' @examples
#' pull_output(f.ids = f.ids, obj = box, obj.name = "aoi.snapped")
#' [1] TRUE
push_output <- function(f.ids, obj, obj.name){

  output <- readxl::read_xlsx("inst/extdata/output_list.xlsx") %>%
    dplyr::filter(s.name == obj.name)

  out.file <- stringr::str_c(f.ids[[output$f.ids.path]][2], "/", output$l.name)

  if(!file.exists(out.file)){
    if(output$write.driver == "st_write()"){
      st_write(obj, dsn = out.file, append = F)
    }
    if(output$write.driver == "writeRaster()"){
      terra::writeRaster(obj, filename = out.file, overwrite = T)
    }
  }

  if(file.exists(out.file)){
    return(out.file)
  } else {
    return(NA_character_)
  }

}
