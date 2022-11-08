#' Setup Folder Structure
#'
#' Sets up the default folder structure
#'
#' Folder structure contains all the raw data, covariates generated, and
#' subsequent models, and predicted map rasters.
#'
#' @param aoi A character string of the folders
#'
#' @return a dataframe of the folders created
#' @export
#' @examples
#' ## add more
#' setupfolder("AppleValley")


setupfolders <- function(aoi){

  #base directory
  AOI_dir <- file.path(".", paste0(aoi))
  raw_dir <- file.path(AOI_dir, "00_raw_inputs")
  derived_dir <- file.path(AOI_dir, "10_map_inputs")


  # input and data processing directly

  cov_dir <- file.path(AOI_dir, "10_map_inputs", "covariates")
  shape_dir <- file.path(AOI_dir, "00_raw_inputs", "vector")
  shape_raw_dir <- file.path(AOI_dir, "00_raw_inputs", "vector", "raw")
  dem_dir <- file.path(AOI_dir, "00_raw_inputs", "dem")
  lidar_dir <- file.path(AOI_dir, "00_raw_inputs", "dem", "lidar")
  trim_dir <- file.path(AOI_dir, "00_raw_inputs", "dem", "trim")
  sat_dir  <- file.path(AOI_dir, "00_raw_inputs", "satelite")
  CHM_dir  <- file.path(AOI_dir, "00_raw_inputs", "chm")

  # sample filepaths
  out_path <- file.path(AOI_dir, "20_sample_design", "stage1_studydesign")
  sampling_raw_folder <- file.path(out_path, "input_raster")
  clhs_outpath <- file.path(out_path, "clhs_sample_plans")

  training_data <- file.path(AOI_dir, "10_map_inputs", "trainingdata")
  training_data_clean <- file.path(AOI_dir, "10_map_inputs", "trainingdata", "clean")

  # model building folders
  model_dir <- file.path(AOI_dir, "30_maps_analysis")
  model_data <- file.path(AOI_dir, "30_maps_analysis", "models")
  model_f <- file.path(AOI_dir, "30_maps_analysis", "models", "forest")


  ## add error control

  if (!exists(aoi)) {

    # set up folders if not already exist
    folder_set_up <- data.frame(AOI_dir, raw_dir,shape_raw_dir, derived_dir,
                       cov_dir, shape_dir, dem_dir,lidar_dir, trim_dir,
                       out_path, sampling_raw_folder, clhs_outpath,training_data,
                       training_data_clean, model_dir, model_data, model_f,
                       sat_dir, CHM_dir)
    folder_id <- t(folder_set_up)
    names(folder_id) <-c("folder_id", "folder_location")

    for(fold in folder_set_up){

      ifelse(!dir.exists(fold), dir.create(fold, recursive = TRUE), FALSE)

    }
    print("Folder structure created!")
    return(folder_id)

  } else {

    print(paste("The", aoi, "folder already exists - defining folder names."))

  }

}
