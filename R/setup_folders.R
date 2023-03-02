#' Setup PEM folder structure
#'
#' Sets up the default folder structure for all analysis and modelling
#'
#' Folder structure contains all the raw data, covariates generated, and
#' subsequent models, and predicted map rasters. _Note_ if directories have
#' been created this function can be used to generate a list of the folder_id
#' and associated file paths.
#'
#' @param aoi_name A character string to name the main data folder
#'
#' @return a named list of the folders created
#' @export
#' @examples
#' ## add more
#' setupfolder("AppleValley")
#' list.dir


setup_folders <- function(aoi_name){

  # Testing ---------------
  # aoi_name <- "e:/workRspace/tmp/apple123/apple" ## full path example
  #aoi_name <- "carrot1"  ## relative example
  ## END TESTING ----------

  #base directory
  AOI_dir <- file.path(paste0(aoi_name))

  # generate folders
  raw_dir_00 <- file.path(AOI_dir, "00_raw_inputs")
  dem_dir_00 <- file.path(raw_dir_00, "20_dem")
  shape_dir_0010 <- file.path(raw_dir_00, "10_vector")

  lidar_dir_00 <- file.path(dem_dir_00, "lidar")
  trim_dir_00 <- file.path(dem_dir_00 , "trim")

  chm_dir_00  <- file.path(raw_dir_00, "40_chm")
  sat_dir_00  <- file.path(raw_dir_00, "30_satelitte")


  cov_dir_10 <- file.path(AOI_dir, "10_clean_inputs")
  shape_dir_1010 <- file.path(cov_dir_10, "10_vector")
  cov_dir_1020 <- file.path(cov_dir_10, "20_covariates")
  training_data_1030 <- file.path(cov_dir_10, "30_trainingdata")
  # note the raster size folders are generated in other script

  sample_dir_20 <- file.path(AOI_dir, "20_sample_plan")
  # sample filepaths
  out_path_2010 <- file.path(sample_dir_20 , "10_standard_sample")
  sampling_input_2010 <- file.path(out_path_2010, "10_input_raster")
  sampling_input_landscape <- file.path(out_path_2010, "10_input_raster", "landscape_covariates")
  sampling_input_exclusion <- file.path(out_path_2010, "10_input_raster", "exclusion")
  sampling_input_review <- file.path(out_path_2010, "10_input_raster", "review")


  samplingplan_201020 <- file.path(out_path_2010, "20_sampleplan_draft")
  samplingplan_clhs <- file.path(samplingplan_201020, "clhs")
  samplingplan_vrp <- file.path(samplingplan_201020, "vrp")
  samplingplan_review <- file.path(samplingplan_201020,  "review")


  sample_dir_001030 <- file.path( out_path_2010, "30_sampleplan_final")
  sampleplan_final_transect <- file.path(sample_dir_001030 , "transect")
  sampleplan_final_maps<- file.path(sample_dir_001030 , "maps")


  trainpts_201040 <- file.path( out_path_2010, "40_transect_data")
  trainpts_transect <- file.path(trainpts_201040  , "raw_field_data")
  trainpts_maps <- file.path(trainpts_201040 , "clean_field_data")
  trainpts_att <- file.path(trainpts_201040 , "attributed_field_data")
  remoteplan_201020 <- file.path(sample_dir_20, "20_remote_sample")

  trans_review2030 <- file.path(sample_dir_20, "30_transect_review")


  sample_dir_0030 <- file.path(AOI_dir, "30_model")


  ## set up folders if not already exist
  ## also used to create list of default directories
  folder_set_up <- data.frame(AOI_dir, raw_dir_00,  dem_dir_00 , shape_dir_0010 ,
                              lidar_dir_00, trim_dir_00, chm_dir_00, sat_dir_00,
                              cov_dir_10,shape_dir_1010, cov_dir_1020, training_data_1030,
                              sample_dir_20 , out_path_2010, sampling_input_2010, sampling_input_landscape,
                              sampling_input_exclusion, sampling_input_review,
                              samplingplan_201020 ,  samplingplan_clhs,samplingplan_vrp,  samplingplan_review,
                              sample_dir_0030,  sampleplan_final_transect, sampleplan_final_maps,
                              trainpts_201040 ,trainpts_transect, trainpts_maps,
                              remoteplan_201020 ,trans_review2030,trainpts_att)

  # generate absolute paths
  folder_set_up_all <- rbind(folder_set_up, R.utils::getAbsolutePath(folder_set_up))

  folder_id <- as.list(folder_set_up_all)

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
    print("folder structure created")
  } else {

    print(paste("The", as.character(aoi_name), "folder already exists - returning folder names."))

  }

  return(folder_id)

}
