#' Setup PEM folder structure
#'
#' Sets up the default folder structure for all analysis and modelling.
#'
#' Folder structure contains all the raw data, covariates generated, subsequent models, and predicted map rasters. _Note_ If directories have been created, this function can be used to generate a list of the folder_id and associated file paths.
#'
#' CLAIRE NOTES:
#' -Need to ensure that the folder_names.xlsx is properly referenced from the package.
#' -How can we make sure the absolute paths being created are in the appropriate working folder? Is this set in the pipeline? See param "data.dir"
#'
#' @param aoi.name A character string to name the main data folder. This will become the working directory for the entire project
#' @param data.dir A character string specifying the desired data directory for this AOI. This SHOULD NOT BE WITHIN THE PACKAGE.
#' @return a named list of the folders created
#' @export
#' @examples
#' setup_folders2("Kitimat")

setup_folders2 <- function(aoi.name, data.dir){

  ##### Testing?
  if(!inherits(aoi.name, "character") | !inherits(data.dir, "character") | !dir.exists(data.dir)){
    print("Parameters aoi.name and/or data.dir are not valid paths.")
    return()
  }
  #####

  ## Get folder names
  folder.setup <- readxl::read_xlsx("inst/extdata/folder_names.xlsx") %>%
    dplyr::mutate(lvl.0 = aoi.name) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rel.path = stringr::str_flatten(dplyr::c_across(dplyr::starts_with("lvl")), collapse = "/", na.rm = T),
           abs.path = stringr::str_c(data.dir, "/", rel.path)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dir.here = base::dir.exists(abs.path)) %>%
    dplyr::select(c(dir.name.new, rel.path, abs.path, dir.here))

  if(nrow(dplyr::filter(folder.setup, dir.here == FALSE)) > 0){
    base::print("creating folders")
  } else{
    base::print(stringr::str_c("The ", base::as.character(aoi.name), " folder already exists - returning folder names."))
  }

  folder.list <- base::lapply(1:base::nrow(folder.setup),
                        function(i) base::c(folder.setup$rel.path[i],
                                      folder.setup$abs.path[i])) %>%
    stats::setNames(., folder.setup$dir.name.new)


  for(i in 1:base::length(folder.list)){
    if(!base::dir.exists(folder.list[[i]][2])){
      base::dir.create(folder.list[[i]][2])
    }
  }

  return(folder.list)

}


