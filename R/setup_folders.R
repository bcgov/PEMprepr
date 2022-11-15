#' Setup Folder Structure
#'
#' Sets up the default folder structure
#'
#' Folder structure contains all the raw data, covariates generated, and
#' subsequent models, and predicted map rasters. _Note_ if directories have
#' been created this function can be used to generate a list of the folder_id
#' and associated file paths.
#'
#' @param aoi A character string to name the main data folder
#'
#' @return a named list of the folders created
#' @export
#' @examples
#' ## add more
#' setupfolder("AppleValley")
#' list.dir


setup_folders <- function(aoi_name, full_names = TRUE){

  # Testing ---------------
  # full_names <- TRUE
  # aoi_name <- "e:/workRspace/tmp/apple123/apple" ## full path example
  # aoi_name <- "carrot"  ## relative example
  ## END TESTING ----------

  ## convert to full path if not provided
  if (full_names == TRUE) {
    if (is_absolute_path(aoi_name) == FALSE) {
      aoi_name <- paste(as.character(getwd()), aoi_name, sep = "/")
    }
  }

  #base directory
  AOI_dir <- file.path(paste0(aoi_name))
  raw_dir <- file.path(AOI_dir, "00_raw_inputs")
  derived_dir <- file.path(AOI_dir, "10_map_inputs")

  # input and data processing directly

  shape_dir <- file.path(AOI_dir,               "00_raw_inputs", "vector")
  shape_raw_dir <- file.path(AOI_dir,           "00_raw_inputs", "vector", "raw")
  dem_dir <- file.path(AOI_dir,                 "00_raw_inputs", "dem")
  lidar_dir <- file.path(AOI_dir,               "00_raw_inputs", "dem", "lidar")
  trim_dir <- file.path(AOI_dir,                "00_raw_inputs", "dem", "trim")
  sat_dir  <- file.path(AOI_dir,                "00_raw_inputs", "satelite")
  CHM_dir  <- file.path(AOI_dir,                "00_raw_inputs", "chm")

  cov_dir <- file.path(AOI_dir,                 "10_clean_inputs", "covariates")

  # sample filepaths
  out_path <- file.path(AOI_dir,                "20_sample_design", "stage1_studydesign")
  sampling_raw_folder <- file.path(out_path,    "input_raster")
  clhs_outpath <- file.path(out_path,           "clhs_sample_plans")


  training_data <- file.path(AOI_dir,           "10_clean_inputs", "trainingdata")
  training_data_vector <- file.path(AOI_dir,    "10_clean_inputs", "vector")
  training_data_clean <- file.path(AOI_dir,     "10_clean_inputs", "trainingdata", "clean")

  # model building folders
  model_dir <- file.path(AOI_dir,               "30_maps_analysis")
  model_data <- file.path(AOI_dir,              "30_maps_analysis", "models")
  model_f <- file.path(AOI_dir,                 "30_maps_analysis", "models", "forest")


  ## add error control


  ## set up folders if not already exist
  ## also used to create list of default directories
  folder_set_up <- data.frame(AOI_dir, raw_dir,shape_raw_dir, derived_dir,
                              cov_dir, shape_dir, dem_dir,lidar_dir, trim_dir,
                              out_path, sampling_raw_folder, clhs_outpath,
                              training_data, training_data_clean, training_data_vector,
                              model_dir, model_data, model_f,
                              sat_dir, CHM_dir)




  ### Compare if dirs exist ... or if there are additionsal dirs
  folder_id <- as.list(folder_set_up)

  # either way return the folder list.
  ## Retrieve full path name
  tf <- as.data.frame(t(folder_set_up))
  tf$id <- row.names(tf)
  names(tf) <- c("path", "id")
  row.names(tf) <- NULL

  tf$exists <- dir.exists(tf$path)


if(sum(tf$exists) < nrow(tf)) {
    print("creating folders")
    for(fold in folder_set_up){

      ifelse(!dir.exists(fold), dir.create(fold, recursive = TRUE), FALSE)



    }
  print("Folder structure created!")

  } else {

    print(paste("The", as.character(aoi_name), "folder already exists - returning folder names."))

  }


  return(folder_id)


}
#
#
# is_absolute_path <- function(path) {
#   grepl("^(/|[A-Za-z]:|\\\\|~)", path)
# }
#
