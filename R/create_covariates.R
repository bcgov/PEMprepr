#' Covariate raster generation from input dtm.
#'
#' _This is an update of cov_dtm.R replacing the processing with 02a_ from  BEC_DevExchange_Work_
#'
#' Takes a dtm and via SAGA GIS generates the covariates embeded in this function.
#' This script has been tested with SAGA 7.3 on Windows and Ubuntu 18.
#' Depending on your system the path to `saga_cmd` may need to be specified.
#'
#'
#' @param dtm is a dtm `SpatRaster` object (i.e. loaded via `terra::rast`)
#' @param SAGApath Is the location of SAGA on your system.  On linux systems with SAGA GIS installed Use `SAGApath = ""`
#' @param output Location of where rasters will be saved.
#' @param layers The covariates that will be generated.  A full list of covariates is listed at: ADD
#' @keywords SAGA, covariates, predictors, raster
#' @export
#' @examples
#' ##
#' create_covariates(dtm,                ## the dtm (loaded by raster() )
#'         SAGApath = "C:/SAGA/"         ## specify location of SAGA on your machine
#'         output   = "c:/dtm-derived" ) ## specify output folder



# # get a base raster that is correct size
#  aoi_raw <- system.file("extdata", "aoi.gpkg", package ="PEMprepr")
#  aoi_raw <- sf::st_read(aoi_raw)
#  aoi <- PEMprepr::aoi_snap(aoi_raw, "shrink")
#  t25 <- create_template(aoi, 25)
#  library(bcmaps)
#  trim_raw <- cded_raster(aoi)
#  trim <- terra::rast(trim_raw)
#  dtm <- terra::project(trim, t25)
#
# PEMprepr::create_covariates (dtm, layers = "all",
#                              output =  filelist$cov_dir_1020[[2]],
#                              SAGApath ="C:/SAGA/saga-7.7.0_x64/")



create_covariates <- function(dtm, SAGApath = "",
                              output = "./cv-rasters",
                              layers = "all"){
  ## testing
   #dtm <- terra::rast(system.file("extdata", "DTM.tif", package = "PEMprepr")) # this raster is not correct size

  # # get a base raster that is correct size
  #  aoi_raw <- system.file("extdata", "aoi.gpkg", package ="PEMprepr")
  #  aoi_raw <- sf::st_read(aoi_raw)
  #  aoi <- PEMprepr::aoi_snap(aoi_raw, "shrink")
  #  t25 <- create_template(aoi, 25)
  #  library(bcmaps)
  #  trim_raw <- cded_raster(aoi)
  #  trim <- terra::rast(trim_raw)
  #  dtm <- terra::project(trim, t25)
  #
  #
  # # dtm <- dat2
  #  layers <- "all"
  #  output <-  filelist$cov_dir_1020[[2]]
  #  SAGApath <- "C:/SAGA/saga-7.7.0_x64/"

  ### In future this would be good to set as a lookup table and then have a single
  # sub-function that uses the table parameters

  ####### Options -- All the possible covariates ########
  layer_options <- c("filled_sinks", "sinkroute", "dem_preproc", "slope_aspect_curve",
               "tcatchment", "tca", "scatchment", "twi", "channelsnetwork",
               "overlandflow", "overlandflow2", "multiresflatness", "multiresflatness2",
               "multiresflatness3", "tri", "convergence", "openness",
               "dah", "tpi", "ridgevalley", "mrn", "flowaccumulation",
               "slopelength", "flowaccumulation2", "flowaccumulation3",
               "flowpathlength", "flowpathlength2", "flowpathlength3", "lsfactor",
               "solarrad", "convexity", "vertdistance", "tci_low",
               "swi", "windexp", "texture", "protection", "vrm",
               "mbi", "mscale_tpi", "relposition", "slopecurvatures",
               "steepestslope", "upslopearea")



  ####### flag all to run #######################
  if (layers == "all") {  ## currently gives warning ... but functions as expected.
    layers <- layer_options
  }

  ####### Error handling -- unspecified layers ############
  err.layers <- setdiff(layers, layer_options)

  if (length(err.layers) == 1) {
    stop(paste(err.layers, "is not a valid covariate" ))
  }

  if (length(err.layers) > 1) {
    print(err.layers)
    stop("Specified covariates are not a valid options" )
  }


     ##### Link SAGA to R --------------------------------------------------
  if(Sys.info()['sysname']=="Windows"){
    saga_cmd <- paste0(SAGApath, "saga_cmd.exe")
    fns      <- "\\" ### file name separator
  } else {
    saga_cmd <- "saga_cmd"
    fns      <- "/" ### file name separator

  }  ;
  z<- system(paste(saga_cmd, "-v"), intern = TRUE)  ## prints that SAGA version number -- confirming it works.
  z <- print(z)
  v <- suppressWarnings(as.numeric(unlist(strsplit(z, "[[:punct:][:space:]]+")[1])))
  v <- v[!is.na(v)][1:2]
  v <- as.numeric(paste(v[1], v[2], sep = "."))

  if (v < 7.6) {
    warning("SAGA-GIS is less that 7.6.  Not all covariates will generate.  Upgrade your SAGA, visit https://sourceforge.net/projects/saga-gis/files/")
  }

  # OUTPUTS: ------------------------------------------------------------
  ifelse(!dir.exists(file.path(output)),              #
         dir.create(file.path(output)), print("Directory Already Exists"))        #create tmpOut

  rn <- terra::res(dtm)[1] ## Get the resolution

  saga_tmp_files <- paste(output, paste0(rn,"m"), sep = "/")
  ifelse(!dir.exists(file.path(saga_tmp_files)),              #if tmpOut Does not Exists
         dir.create(file.path(saga_tmp_files)), print("Directory Already Exists"))        #create tmpOut


  ## Convert to Saga format for processing ---------------------------------------
  sDTM <- "dtm.sdat"
  sDTM <- paste(saga_tmp_files, sDTM, sep= "/")
  terra::writeRaster(dtm, sDTM, overwrite = TRUE)


  ############# Covariate File Names #############################################
  ### Create Names for all the covariates ... needed here so that dependancies are
  ### partially met

  ### This is ugly! re-write
  sDTM <- paste(saga_tmp_files, "dtm.sdat", sep= fns)
  sinksroute <- paste(saga_tmp_files, "sinkroute.sgrd",sep = fns)
  sinksfilled <- paste(saga_tmp_files, "filled_sinks.sgrd", sep = fns)
  dem_preproc <- paste(saga_tmp_files,"dem_preproc.sgrd", sep = fns)
  slope <- paste(saga_tmp_files, "slope.sgrd", sep = fns)
  aspect <- paste(saga_tmp_files, "aspect.sgrd", sep = fns)
  gencurve <- paste(saga_tmp_files, "gencurve.sgrd", sep = fns)
  totcurve <- paste(saga_tmp_files, "totcurve.sgrd", sep = fns)
  tcatchment <- paste(saga_tmp_files, "tcatchment.sgrd", sep = fns)
  tca <- paste(saga_tmp_files, "tca1.sgrd", sep = fns)
  flowlength4 <- paste(saga_tmp_files, "flowlength1.sgrd", sep = fns) ## part of tca
  scatchment <- paste(saga_tmp_files, "scatchment.sgrd", sep = fns)
  twi <- paste(saga_tmp_files, "twi.sgrd", sep = fns)
  channelsnetwork <- paste(saga_tmp_files, "cnetwork.sgrd", sep = fns)
  hdistance <-  paste(saga_tmp_files, "hdist.sgrd", sep = fns)
  vdistance  <- paste(saga_tmp_files, "vdist.sgrd", sep = fns)
  hdistancenob <-  paste(saga_tmp_files, "hdistnob.sgrd", sep = fns)
  vdistancenob  <- paste(saga_tmp_files, "vdistnob.sgrd", sep = fns)
  MRVBF <- paste(saga_tmp_files, "mrvbf.sgrd", sep = fns)
  MRRTF <- paste(saga_tmp_files, "mrrtf.sgrd", sep = fns)
  MRVBF2 <- paste(saga_tmp_files, "mrvbf2.sgrd", sep = fns)
  MRRTF2 <- paste(saga_tmp_files, "mrrtf2.sgrd", sep = fns)
  tri <- paste(saga_tmp_files, "tri.sgrd", sep = fns)
  convergence <- paste(saga_tmp_files, "convergence.sgrd", sep = fns)
  opos <- paste(saga_tmp_files, "open_pos.sgrd", sep = fns)
  oneg <- paste(saga_tmp_files, "open_neg.sgrd", sep = fns)
  dAH <- paste(saga_tmp_files, "dah.sgrd", sep = fns)
  tpi <- paste(saga_tmp_files, "tpi.sgrd", sep = fns)
  val_depth = paste(saga_tmp_files, "val_depth.sgrd", sep = fns)
  ridgelevel = paste(saga_tmp_files, "rid_level.sgrd", sep = fns)
  mrncatchment = paste(saga_tmp_files, "mnr_area.sgrd", sep = fns)
  mrnmaxheight = paste(saga_tmp_files, "mnr_mheight.sgrd", sep = fns)
  mrn = paste(saga_tmp_files, "mnr.sgrd", sep = fns)
  flowaccumft = paste(saga_tmp_files, "flow_accum_ft.sgrd", sep = fns)
  meanovcatch = paste(saga_tmp_files, "meanovcatch.sgrd", sep = fns)
  accummaterial = paste(saga_tmp_files, "accummaterial.sgrd", sep = fns)
  slopelength = paste(saga_tmp_files, "slength.sgrd", sep = fns)
  flowaccump = paste(saga_tmp_files, "flow_accum_p.sgrd", sep = fns)
  flowaccumtd = paste(saga_tmp_files, "flow_accum_td.sgrd", sep = fns)
  meanovcatchTD = paste(saga_tmp_files, "meanovcatchTD.sgrd", sep = fns)
  accummaterialTD = paste(saga_tmp_files, "accummaterialTD.sgrd", sep = fns)
  flowpathlenTD = paste(saga_tmp_files, "flowpathlenTD.sgrd", sep = fns)
  flowpathlength = paste(saga_tmp_files, "max_fp_l.sgrd", sep = fns)
  flowpathlength = paste(saga_tmp_files, "max_fp_l1.sgrd", sep = fns)
  FloOwAccum = paste(saga_tmp_files, "slope_lts_fa.sgrd", sep = fns)
  lsfactor = paste(saga_tmp_files, "ls_factor.sgrd", sep = fns)
  DirInsol <- paste(saga_tmp_files, "direinso.sgrd", sep = fns)
  DifInsol <- paste(saga_tmp_files, "diffinso.sgrd", sep = fns)
  convexity = paste(saga_tmp_files, "convexity.sgrd", sep = fns)
  vertdistance = paste(saga_tmp_files, "vert_dis.sgrd", sep = fns)
  tci_low = paste(saga_tmp_files, "tci_low.sgrd", sep = fns)
  catchmentarea = paste(saga_tmp_files, "swi_area.sgrd", sep = fns)
  catchmentslope = paste(saga_tmp_files, "swi_slope.sgrd", sep = fns)
  modcatchmentarea = paste(saga_tmp_files, "swi_area_mod.sgrd", sep = fns)
  topowetindex = paste(saga_tmp_files, "swi_twi.sgrd", sep = fns)
  windexp = paste(saga_tmp_files, "wind_exp_index.sgrd", sep = fns)
  Texture = paste(saga_tmp_files, "texture.sgrd", sep = fns)
  protection = paste(saga_tmp_files, "protection.sgrd", sep = fns)
  vrm = paste(saga_tmp_files, "vrm.sgrd", sep = fns)
  mbi = paste(saga_tmp_files, "mbi.sgrd", sep = fns)
  tpi = paste(saga_tmp_files, "mscale_tpi.sgrd", sep = fns)
  slopeheight = paste(saga_tmp_files, "slope_height.sgrd", sep = fns)
  valleydepth = paste(saga_tmp_files, "valleydepth.sgrd", sep = fns) #don't need this as created above?
  normheight = paste(saga_tmp_files, "norm_height.sgrd", sep = fns)
  standheight = paste(saga_tmp_files, "stand_height.sgrd", sep = fns)
  msposition = paste(saga_tmp_files, "ms_position.sgrd", sep = fns)
  localcurve = paste(saga_tmp_files, "local_curv.sgrd", sep = fns)
  upslopecurve = paste(saga_tmp_files, "upslope_curv.sgrd", sep = fns)
  localupcurve = paste(saga_tmp_files, "local_upslope_curv.sgrd", sep = fns)
  downcurve = paste(saga_tmp_files, "down_curv.sgrd", sep = fns)
  localowncurve = paste(saga_tmp_files, "local_downslope_curv.sgrd", sep = fns)
  steepestslope <- paste(saga_tmp_files, "steepest_slope.sgrd", sep = fns)
  upslopearea = paste(saga_tmp_files, "upslopearea.sgrd", sep = fns)



  ############################### BEGIN PROCESSING ###############################

  ####### >> 1 -- Fill Sinks XXL (Wang and Liu)  -----------------------------

  # Fill sinks in dem to prepare base DEM for other layers:

  #### STEP 1: preprocess DEM

  ## http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_5.html
  ## Module Fill Sinks XXL (Wang & Liu)

  # This module uses an algorithm proposed by Wang & Liu to identify and fill surface depressions in digital elevation models.
  # The method was enhanced to allow the creation of hydrologic sound elevation models, i.e. not only to fill the depression(s) but also to preserve a downward slope along the flow path. If desired, this is accomplished by preserving a minimum slope gradient (and thus elevation difference) between cells.
  # This version of the module is designed to work on large data sets (e.g. LIDAR data), with smaller datasets you might like to check out the fully featured standard version of the module.

  if ("filled_sinks" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_preprocessor 5", "-ELEV" ,
                    sDTM,
                    "-FILLED", sinksfilled,
                    "-MINSLOPE ", 0.1
    )
    system(sysCMD)
  }


  # Generate sink drainage route detection layer to use in preprocess DEM
  #http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_1.html

  if ("sinkroute" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_preprocessor 1",
                    "-ELEVATION" , sDTM,
                    "-SINKROUTE", sinksroute

    )
    system(sysCMD)
  }

  # preproces DEM version 2:  fills sinks (input requires DEM + sink detection layer
  # generated above)/
  #http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_2.html


  if ("dem_preproc" %in% layers) {
    sysCMD <- paste(saga_cmd, "ta_preprocessor 2",
                    "-DEM" , sDTM,
                    "-SINKROUTE", sinksroute,
                    "-DEM_PREPROC", dem_preproc,
                    "-METHOD", 1,
                    "-THRESHOLD", 0

    )
    system(sysCMD)
  }

  ##### >> 2 -- Slope Aspect and Curvature -------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html

  if ("slope_aspect_curve" %in% layers) {
    sysCMD <- paste(saga_cmd, "ta_morphometry 0", "-ELEVATION",
                    sDTM,
                    "-SLOPE", slope,
                    "-ASPECT", aspect,                     # Outputs
                    "-C_GENE", gencurve,
                    "-C_TOTA", totcurve,                # Outputs
                    "-METHOD", 6,
                    "-UNIT_SLOPE", 0,
                    "-UNIT_ASPECT", 0       # Default Parameters
    )
    system(sysCMD)
  }

  ##### >> 3 -- Total Catchment Area --------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_0.html

   if ("tcatchment" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_hydrology 0", "-ELEVATION",
                    sDTM,
                    "-FLOW", tcatchment,                                    # Output
                    "-METHOD", 4                                            # Default Parameters
    )
    system(sysCMD)
  }
  ## Note this is the same as flow Accumulation top down (#19 although less outputs included here that are included in #19


  #####################
  # Still to run - using tCatchement instead for the base tca raster for other inputs
  # This is not working properly but is the equivalent to tca - needs more work

  #  #   Following this method for calculating topographic wetness index:
  # #    https://gracilis.carleton.ca/CUOSGwiki/index.php/Enhanced_Wetness_Modelling_in_SAGA_GIS
  # #    See this paper as well for discussion on different ways to calculate TWI:
  # #   https://link.springer.com/article/10.1186/s40965-019-0066-y

  ##### >> 3a -- Total Catchment Area --------------------------------------

  if ("tca" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_hydrology 1",
                    "-ELEVATION",
                    sDTM,
                    # file.path(gsub("sdat","sgrd", sDTM)),
                    "-FLOW", tca,
                    "-FLOW_LENGTH", flowlength4,
                    "-FLOW_UNIT", 1,
                    "-METHOD", 3
    )
    system(sysCMD)
  }
  ####################

  # try other methods
  #
  # # # no difference when using method (#3) and 2 and 1 :
  #
  #      # flow_accum = paste0(
  #      #   "<tool library='ta_hydrology' tool='1' name='Flow Accumulation (Recursive)'>
  #      #      <input id='ELEVATION'>dem_preproc</input>
  #      #      <output id='FLOW'>tca2</output>
  #      #      <output id='FLOW_LENGTH'>flowlength2</output>
  #      #      <option id='FLOW_UNIT'>1</option>
  #      #      <option id='METHOD'>2</option>
  #      #  </tool>"
  #      # ),
  #
  #      # flow_accum = paste0(
  #      #   "<tool library='ta_hydrology' tool='1' name='Flow Accumulation (Recursive)'>
  #      #      <input id='ELEVATION'>dem_preproc</input>
  #      #      <output id='FLOW'>tca1</output>
  #      #      <output id='FLOW_LENGTH'>flowlength1</output>
  #      #      <option id='FLOW_UNIT'>1</option>
  #      #      <option id='METHOD'>1</option>
  #      #  </tool>"#
  #


  ##### >> 4 -- Flow Width and Specific Catchment Area --------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_19.html

  if ("scatchment" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_hydrology 19", "-DEM", sinksfilled,       # Input from 1
                    "-SCA", scatchment,                                     # Output
                    "-TCA", tcatchment,                                     # Input from 2
                    "-METHOD", 1                                            # Parameters
    )
    system(sysCMD)

  }

  ##### >> 5 -- Topographic Wetness Index --------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_20.html

  if ("twi" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_hydrology 20",
                    "-SLOPE", slope,           # Input from 11
                    "-AREA", scatchment,                                    # Input from 3
                    "-TWI", twi,                                            # Output
                    "-CONV",1,
                    "-METHOD", 1
    )
    system(sysCMD)
  }

  ##### >> 6 -- Channel Network -------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_0.html
  # https://sourceforge.net/projects/saga-gis/files/SAGA%20-%20Documentation/SAGA%20Documents/SagaManual.pdf/download

  if ("channelsnetwork" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_channels 0",
                    "-ELEVATION", sinksfilled,     # Input from 1
                    "-CHNLNTWRK", channelsnetwork,                            # Output
                    "-INIT_GRID", tcatchment,                                 # Input from 2
                    "-INIT_VALUE", 1000000,
                    "-INIT_METHOD", 2,                # Based on SAGA Manual Documentation, p. 119
                    "-DIV_CELLS", 5.0,
                    "-MINLEN", 10.0                        # Default Parameters
    )
    system(sysCMD)
  }


  ##### >> 7 -- Overland Flow Distance to Channel Network -----------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_4.html

  if ("overlandflow" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_channels 4",
                    "-ELEVATION", sinksfilled,        # Input from 1
                    "-CHANNELS", channelsnetwork,     # Input from 4
                    "-DISTANCE", hdistance,
                    "-DISTVERT", vdistance,           # Outputs
                    "-METHOD", 1,
                    "-BOUNDARY", 1                              # Parameters
    )
    system(sysCMD)
  }
  # note distnob created using XML script with no boundary. This shows NA for areas on the edge where
  # metrics cannot be calculated)

  if ("overlandflow2" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_channels 4",
                    "-ELEVATION", sinksfilled,   # Input from 1
                    "-CHANNELS", channelsnetwork,                             # Input from 4
                    "-DISTANCE", hdistancenob,
                    "-DISTVERT", vdistancenob,           # Outputs
                    "-METHOD", 1,
                    "-BOUNDARY", 0                              # Parameters
    )
    system(sysCMD)
  }

  # #     # testing other output method?
  # #     # Used the method = 0 (Deterministic 8 (O'Callaghan & Mark 1984) method)
  # #     # http://www.saga-gis.org/saga_tool_doc/2.3.0/ta_channels_4.html
  # #
  # #      flow_dist = paste0(
  # #        "<tool library='ta_channels' tool='4' name='Overland Flow Distance to Channel Network'>
  # #           <input id='ELEVATION'>dem_preproc</input>
  # #           <input id='CHANNELS'>cnetwork</input>
  # #           <output id='DISTANCE'>hdist0</output>
  # #           <output id='DISTVERT'>vdist0</output>
  # #           <option id='METHOD'>0</option>
  # #           <option id='BOUNDARY'>true</option>
  # #       </tool>"
  # #      ),
  # #



  ##### >> 8 -- MRVBF -----------------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_8.html
  if ("multiresflatness" %in% layers) {

    # use defaul parameters
    sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                    # file.path(gsub("sdat","sgrd", sDTM)),
                    sDTM,
                    "-MRVBF", MRVBF,
                    "-MRRTF", MRRTF,                       # Outputs
                    "-T_SLOPE", 16,
                    "-T_PCTL_V", 0.4,
                    "-T_PCTL_R", 0.35,    # Default Parameters
                    "-P_SLOPE", 4.0,
                    "-P_PCTL", 3.0,
                    "-UPDATE", 0,
                    "-CLASSIFY", 0,
                    "-MAX_RES", 100
    )
    system(sysCMD)
  }
  # Test a Variety of paramter and method versions.
  # Note these need to be converted from XML chain format to standard format

  # tested a number of MRVBF options for the t-slope parameter #ie
  # use dem_preproces for input and lowered the slope parameter from 15 to 10
  #
  if ("multiresflatness2" %in% layers) {

    #  Adjust parameters -  Option 2.
    sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                    # file.path(gsub("sdat","sgrd", sDTM)),
                    sDTM,
                    "-MRVBF", MRVBF2,
                    "-MRRTF", MRRTF2,
                    "-T_SLOPE", 10,
                    "-T_PCTL_V", 0.4,
                    "-T_PCTL_R", 0.35,
                    "-P_SLOPE", 4.0,
                    "-P_PCTL", 3.0,
                    "-UPDATE", 0,
                    "-CLASSIFY", 0,
                    "-MAX_RES", 100
    )
    system(sysCMD)
  }

  ## dropped 2022-11-08 PEMr workshop ... not included in GP's 02a_DEM_SpatialLayer_Prep.R
  # #  Adjust parameters -  Option 3.
  #
  # if ("multiresflatness3" %in% layers) {
  #
  #   MRVBF5 <- "mrvbf5.sgrd"
  #   MRRTF5 <- "mrrtf5.sgrd"
  #
  #   sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
  #                   # file.path(gsub("sdat","sgrd", sDTM)),
  #                   sDTM,
  #                   "-MRVBF", MRVBF5,
  #                   "-MRRTF", MRRTF5,
  #                   "-T_SLOPE", 64,
  #                   "-T_PCTL_V", 0.2,
  #                   "-T_PCTL_R", 0.6,
  #                   "-P_SLOPE", 4.0,
  #                   "-P_PCTL", 3.0,
  #                   "-UPDATE", 0,
  #                   "-CLASSIFY", 0,
  #                   "-MAX_RES", 100
  #   )
  #   system(sysCMD)
  # }
  # tested other options:

  #     mrvbf = paste0(
  #       "<tool library='ta_morphometry' tool='8' name='Multiresolution Index of Valley Bottom Flatness (MRVBF)'>
  #          <input id='DEM'>dem_preproc</input>
  #          <output id='MRVBF'>mrvbf4</output>
  #          <output id='MRRTF'>mrrtf4</output>
  #          <option id='T_SLOPE'>64</option>
  #          <option id='T_PCTL_V'>0.600000</option>
  #          <option id='T_PCTL_R'>0.200000</option>
  #          <option id='P_SLOPE'>4.000000</option>
  #          <option id='P_PCTL'>3.000000</option>
  #          <option id='UPDATE'>false</option>
  #          <option id='CLASSIFY'>false</option>
  #          <option id='MAX_RES'>100.000000</option>
  #      </tool>"
  #     ),
  #
  #



  ##### >> 9 -- Terrain Ruggedness Index ----------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_16.html

  if ("tri" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_morphometry 16",
                    sDTM,
                    "-TRI", tri,  # Output
                    "-MODE", 0,
                    "-RADIUS", 3.0,
                    "-DW_WEIGHTING", 0          # Parameters
    )
    system(sysCMD)
  }

  ##### >> 10 -- Convergence Index -----------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_1.html


  if ("convergence" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_morphometry 1",
                    "-ELEVATION ",
                    sDTM,
                    "-RESULT", convergence,                                 # Output
                    "-METHOD", 1,
                    "-NEIGHBOURS", 1                          # Parameters
    )
    system(sysCMD)
  }

  ##### >> 11 -- openness --------------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_5.html

  if ("openness" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_lighting 5", "-DEM",
                    sDTM,
                    "-POS", opos,
                    "-NEG", oneg,                               # Outputs
                    "-RADIUS", 1000,
                    "-METHOD", 0,
                    "-DLEVEL",  3,
                    "-NDIRS", 8
    )
    system(sysCMD)
  }

  ##### >> 12 -- Diuranal Anisotropic Heating -----------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_12.html

  if ("dah" %in% layers) {
    sysCMD <- paste(saga_cmd, "ta_morphometry 12", "-DEM",
                    sDTM,
                    "-DAH", dAH,                                            # Output
                    "-ALPHA_MAX", 202.5                                     # Default Parameters
    )
    system(sysCMD)
  }

  ##### >> 13 -- Topographic Position Index --------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_18.html

  if ("tpi" %in% layers) {

    sysCMD <- paste(saga_cmd, "ta_morphometry 18", "-DEM",
                    sDTM,
                    "-TPI", tpi,                                            # Output
                    "-STANDARD", 0,
                    "-RADIUS_MIN", 0,
                    "-RADIUS_MAX", 100,   # Default Parameters
                    "-DW_WEIGHTING", 0,
                    "-DW_IDW_POWER", 1,
                    "-DW_IDW_OFFSET", 1,
                    "-DW_BANDWIDTH", 75
    )
    system(sysCMD)
  }

  # re-run - may need to adjust the radius min and radius max???


  #### >> 14 -- Valley Depth -------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_7.html

  if ("ridgevalley" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_channels 7",
                   "-ELEVATION", sinksfilled,            # input DEM
                   "-VALLEY_DEPTH", val_depth,         # output Valley Depth
                   "-RIDGE_LEVEL", ridgelevel,           # output Ridge Level
                   "-THRESHOLD", 1,
                   "-NOUNDERGROUND", 1,
                   "-ORDER", 4
    )
    system(sysCMD)
  }


  #### >> 15 -- Melton Ruggedness Number -------------------------- ## works
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_23.html

    if ("mrn" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_hydrology 23",
                   "-DEM", sinksfilled,                 # input DEM
                   "-AREA", mrncatchment,               # output MRN Catchment
                   "-ZMAX", mrnmaxheight,               # output MRN Max Height
                   "-MRN", mrn                          # output MRN
    )
    system(sysCMD)
  }

  #### >> 16 -- Flow Accumulation (Flow Tracing)  --------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_2.html

  if ("flowaccumulation" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_hydrology 2",
                   "-ELEVATION", sinksfilled,            # input DEM
                   "-FLOW", flowaccumft,                 # output Flow Accumulation
                   "-VAL_MEAN", meanovcatch,             # output Mean over Catchment
                   "-ACCU_TOTAL", accummaterial,         # output Accumulated Material
                   "-FLOW_UNIT", 1,
                   "-METHOD", 1,
                   "-MINDQV", 0
    )
    system(sysCMD)
  }

  #### >> 17 -- Slope Length ---------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_7.html

   if ("slopelength" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_hydrology 7",
                   "-DEM", sinksfilled,             # input DEM
                   "-LENGTH", slopelength            # output Slope Length
    )
    system(sysCMD)
  }


  #### >> 18 -- Flow Accumulation (Parallelizable) -------------------
  ## this tool doesn't seem to exist - SAGA version issue?
  # # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_29.html

  if ("flowaccumulation2" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_hydrology 29",
                   "-DEM", sinksfilled,                  # input DEM
                   "-FLOW", flowaccump,                  # output Flow Accumulation
                   "-METHOD", 2,
                   "-CONVERGENCE", 1.1
    )
    system(sysCMD)
  }

  #### >> 19 -- Flow Accumulation (Top-Down) ---------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_0.html

  if ("flowaccumulation3" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_hydrology 0",
                   "-ELEVATION", sinksfilled,                 # input DEM
                   "-FLOW", flowaccumtd,                      # output Flow Accumulation
                   "-VAL_MEAN", meanovcatchTD,                # output Mean over Catchment
                   "-ACCU_TOTAL", accummaterialTD,            # output Accumulated Material
                   "-FLOW_LENGTH", flowpathlenTD,             # output Flow Path Length
                   "-FLOW_UNIT", 1,
                   "-METHOD", 4,
                   "-LINEAR_DO", 1,
                   "-LINEAR_MIN", 500,
                   "-CONVERGENCE", 1.1
    )
    system(sysCMD)
  }

  #### >> 20 -- Stream Power Index ----------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_21.html

  # Not included as binary output

  # StreamPower = "spower.sgrd"
  # StreamPower = file.path(tmpOut, StreamPower)
  # sysCMD = paste(saga_cmd, "ta_hydrology 21",
  #                "-SLOPE", slope,                    # input Slope
  #                "-AREA", tcatchment,                # input Catchment Area
  #                "-SPI", StreamPower,               # output Stream Power Index
  #                "-CONV", 0
  # )
  # system(sysCMD)


  #### >> 21 -- Maximum Flow Path Length ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_27.html

  if ("flowpathlength" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_hydrology 27",
                   "-ELEVATION", sinksfilled,            # input DEM
                   "-DISTANCE", flowpathlength,          # output Max Flow Path Length
                   "-DIRECTION", 0
    )
    system(sysCMD)
  }


  #### >> 21a -- Maximum Flow Path Length ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_27.html
  #
  if ("flowpathlength2" %in% layers) {

        sysCMD = paste(saga_cmd, "ta_hydrology 27",
                   "-ELEVATION", sinksfilled,            # input DEM
                   "-DISTANCE", flowpathlength,          # output Max Flow Path Length
                   "-DIRECTION", 1
    )
    system(sysCMD)
    #
  }

  #### >> 22 -- Slope Limited Flow Accumulation -------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_26.html

  if ("flowpathlength3" %in% layers) {

     sysCMD = paste(saga_cmd, "ta_hydrology 26",
                   "-DEM", sinksfilled,               # input DEM
                   "-FLOW", flowaccum,                # output Flow Accumulation
                   "-SLOPE_MIN", 0,
                   "-SLOPE_MAX", 5,
                   "-B_FLOW", 0
    )
    system(sysCMD)
  }
  #### >> 23 -- LS Factor -----------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_22.html


  if ("lsfactor" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_hydrology 22",
                   "-SLOPE", slope,                # input Slope
                   "-AREA", tcatchment,            # input Catchment Area
                   "-LS", lsfactor,                # output LS Factor
                   "-CONV", 0,
                   "-METHOD", 0,
                   "-EROSIVITY", 1,
                   "-STABILITY", 0
    )
    system(sysCMD)
  }



  #### >> 24 -- Solar covariates  -----------------------------------------
  # solar direct and diffuse solar radiation
  # adjust min and max limits to 4am and 22 pm to reduce processing time
  #http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_2.html
  #Calculation of potential incoming solar radiation (insolation). Times of sunrise/sunset will only be calculated if time span is set to single day.
  if ("solarrad" %in% layers) {

      sysCMD <- paste(saga_cmd, "ta_lighting 2",
                    "-GRD_DEM",
                    # file.path(gsub("sdat","sgrd", sDTM)),# Input DTM
                    sDTM,
                    "-GRD_DIRECT", DirInsol,
                    "-GRD_DIFFUS", DifInsol,       # Outputs
                    "-SOLARCONST", 1367,
                    "-LOCALSVF", 1,
                    "-SHADOW", 0,      # Parameters
                    "-LOCATION", 1,
                    "-PERIOD", 2,
                    "-DAY", "2018-02-15",
                    "-DAY_STOP", "2019-02-15",
                    "-DAYS_STEP", 30,
                    "-HOUR_RANGE_MIN", 4,
                    "-HOUR_RANGE_MAX", 22,
                    "-HOUR_STEP", 0.5,
                    "-METHOD", 2,
                    "-LUMPED", 70
    )
    system(sysCMD)
  }

  #### >> 25 -- Terrain Surface convexity ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_21.html

   if ("convexity" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_morphometry 21",
                   "-DEM", sinksfilled,                   # input DEM
                   "-CONVEXITY", convexity,               # output Convexity
                   "-KERNEL", 0,
                   "-TYPE", 0,
                   "-EPSILON", 0,
                   "-SCALE", 10,
                   "-METHOD", 1,
                   "-DW_WEIGHTING", 0,
                   "-DW_IDW_POWER", 2,
                   "-DW_BANDWIDTH", 1
    )
    system(sysCMD)
  }

    #### >> 26 -- Vertical Distance to Channel Network ------------- ## froze - maybe just very slow?
    # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_3.html

  if ("vertdistance" %in% layers) {

        sysCMD = paste(saga_cmd, "ta_channels 3",
                   "-ELEVATION", sinksfilled,            # input DEM
                   "-CHANNELS", channelsnetwork,         # input Channel Network
                   "-DISTANCE", vertdistance,            # output
                   "-THRESHOLD", 1,
                   "-NOUNDERGROUND", 1
    )
    system(sysCMD)
  }


  #### >> 27 -- TCI Low -------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_24.html

   if ("tci_low" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_hydrology 24",
                   "-DISTANCE", vertdistance,            # input Vertical Distance to Channel Network
                   "-TWI", twi,                          # input TWI
                   "-TCILOW", tci_low                     # output TCI Low
    )
    system(sysCMD)
  }

  #### >> 28 -- SAGA Wetness Index -------------------------------- ## works but VERY slow (~18 hours)
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_15.html

  if ("swi" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_hydrology 15",
                   "-DEM", sinksfilled,                 # input DEM
                   "-AREA", catchmentarea,              # output Catchment Area
                   "-SLOPE", catchmentslope,            # output Catchment Slope
                   "-AREA_MOD", modcatchmentarea,       # output Modified Catchment Area
                   "-TWI", topowetindex,                # output TWI
                   "-SUCTION", 10,
                   "-AREA_TYPE", 1,
                   "-SLOPE_TYPE", 1,
                   "-SLOPE_MIN", 0,
                   "-SLOPE_OFF", 0.1,
                   "-SLOPE_WEIGHT", 1
    )
    system(sysCMD)
  }

  #### >> 29 -- Wind Exposition Index ------------------------------ ## works but VERY slow
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_27.html

   if ("windexp" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_morphometry 27",
                   "-DEM", sinksfilled,                     # input DEM
                   "-EXPOSITION", windexp,                  # output Wind Exposition Index
                   "-MAXDIST", 300,
                   "-STEP", 15,
                   "-ACCEL", 1.5
    )
    system(sysCMD)
  }

  #### >> 30 -- Terrain Surface Texture -----------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_20.html

  if ("Texture" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_morphometry 20",
                   "-DEM", sinksfilled,                      # input DEM
                   "-TEXTURE", Texture,                      # output Terrain Surface Texture
                   "-EPSILON", 1,
                   "-SCALE", 10,
                   "-METHOD", 1,
                   "-DW_WEIGHTING", 0,
                   "-DW_IDW_POWER", 2,
                   "-DW_BANDWIDTH", 1
    )
    system(sysCMD)
  }

  #### >> 31 -- Morphometric Protection Index ----------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_7.html

  if ("protection" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_morphometry 7",
                   "-DEM", sinksfilled,                        # input DEM
                   "-PROTECTION", protection,                  # output Morphometric Protection Index
                   "-RADIUS", 2000
    )
    system(sysCMD)
  }


  #### >> 32 -- Vector Ruggedness Measure ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_17.html

  if ("vrm" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_morphometry 17",
                   "-DEM", sinksfilled,                      # input DEM
                   "-VRM", vrm,                              # output Vector Ruggedness Measure
                   "-MODE", 1,
                   "-DW_WEIGHTING", 0,
                   "-DW_IDW_POWER", 2,
                   "-DW_BANDWIDTH", 1
    )
    system(sysCMD)
  }

  #### >> 33 -- Mass Balance Index ----------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_10.html

  if ("mbi" %in% layers) {

        sysCMD = paste(saga_cmd, "ta_morphometry 10",
                   "-DEM", sinksfilled,                 # input DEM
                   "-HREL", vertdistance,               # input Vertical Distance to Channel Network
                   "-MBI", mbi,                         # output Mass Balance Index
                   "-TSLOPE", 15,
                   "-TCURVE", 0.01,
                   "-THREL", 15
    )
    system(sysCMD)
  }

  #### >> 34 -- Multi-Scale Topographic Position Index --------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_28.html

  if ("mscale_tpi" %in% layers) {

      sysCMD = paste(saga_cmd, "ta_morphometry 28",
                   "-DEM", sinksfilled,                # input DEM
                   "-TPI", tpi,                        # output tpi
                   "SCALE_MIN", 1,
                   "SCALE_MAX", 8,
                   "SCALE_NUM", 3
    )
    system(sysCMD)
  }

  #### >> 35 -- Relative Heights and Slope Positions ----------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_14.html

  if ("relposition" %in% layers) {

        sysCMD = paste(saga_cmd, "ta_morphometry 14",
                   "-DEM", sinksfilled,                 # input DEM
                   "-HO", slopeheight,                  # output Slope Height
                   "-HU", valleydepth,                  # output Valley Depth
                   "-NH", normheight,                   # output Normalized Height
                   "-SH", standheight,                  # output Standardized Height
                   "-MS", msposition,                   # output Mid-Slope Position
                   "-W", 0.5,
                   "-T", 10,
                   "-E", 2
    )
    system(sysCMD)
  }


  #### >> 36 -- Valley and Ridge Detection (Top Hat Approach) -------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_24.html

  # not very informative (binary outputs)

  # HillHeight = "hill_height.sgrd"
  # HillHeight = file.path(tmpOut, HillHeight)
  # ValleyIndex = "valley_index.sgrd"
  # ValleyIndex = file.path(tmpOut, ValleyIndex)
  # HillIndex = "hill_index.sgrd"
  # HillIndex = file.path(tmpOut, HillIndex)
  # HillslopeIndex = "hillslope_index.sgrd"
  # HillslopeIndex = file.path(tmpOut, HillslopeIndex)
  # sysCMD = paste(saga_cmd, "ta_morphometry 24",
  #                "-DEM", sinksfilled,                 # input DEM
  #                "-HILL", HillHeight,                 # output Hill Height
  #                "-VALLEY_IDX", ValleyIndex,          # output Valley Index
  #                "-HILL_IDX", HillIndex,              # output Hill Index
  #                "-SLOPE_IDX", HillslopeIndex,        # output Hillslope Index
  #                "-RADIUS_VALLEY", 1000,
  #                "-RADIUS_HILL", 1000,
  #                "-THRESHOLD", 100,
  #                "-METHOD", 0
  # )
  # system(sysCMD)

  #### >> 37 -- Upslope and Downslope Curvature ---------------------

  if ("slopecurvatures" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_morphometry 26",
                   "-DEM", sinksfilled,                         # input DEM
                   "-C_LOCAL", localcurve,                      # output Local Curvature
                   "-C_UP", upslopecurve,                       # output Upslope Curvature
                   "-C_UP_LOCAL", localupcurve,                 # output Local Upslope Curvature
                   "-C_DOWN", downcurve,                        # output Downslope Curvature
                   "-C_DOWN_LOCAL", localowncurve,             # output Local Downslope Curvature
                   "-WEIGHTING", 0.5
    )
    system(sysCMD)
  }

  #### >> 38 -- Steepest Slope (Slope Aspect and Curvature) --------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html


  if ("steepestslope" %in% layers) {

     sysCMD <- paste(saga_cmd, "ta_morphometry 0",
                    "-ELEVATION", sinksfilled,                              # input DEM
                    "-SLOPE", steepestslope,                                # output Steepest Slope
                    "-METHOD", 1,                                           # method 1 - steepest slope
                    "-UNIT_SLOPE", 0,
                    "-UNIT_ASPECT", 0
    )
    system(sysCMD)

  }

  # #### >> 39 -- Upslope Area -------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.2/ta_hydrology_4.html

  if ("upslopearea" %in% layers) {

    sysCMD = paste(saga_cmd, "ta_hydrology 4",
                   "-ELEVATION", sinksfilled,           # input DEM
                   "-SINKROUTE", sinksroute,
                   "-AREA", upslopearea,                # output Upslope Area
                   "-METHOD", 2,
                   "-CONVERGE", 1.1
    )
    system(sysCMD)


  }

  ################ Covariate Generation Complete ####################

  #### Convert to GeoTif --------------------------------
  # setwd(rtnwd)

  ## Collect tmp saga file names

  ## TEST paramaters
  # output <- "e:/tmp"

  # tmpFiles <- paste(output, "saga", sep = "/")
  # l <- list.files(path = tmpFiles, pattern = "*.sdat")
  # l <- l[!grepl(".xml", l)] ## removes xmls from the list
  # print(l)

  ## OutFile Suffix Use resolution as suffix for out filename
  # r <- raster::raster(paste(tmpFiles, l[1], sep= "/"))
  # subFolder <- raster::res(r)[1]  ##
  # suf <- paste0("_", subFolder, ".tif")
  # outList <- gsub(".sdat", suf, l)

  ## Loop through files and convert to tif
  # for(i in 1:length(l)){
  #
  #   ## parms for testing
  #   # i <- 1
  #
  #   #actions
  #   r <- l[i]
  #   inFile <- paste(tmpFiles, r, sep = "/")
  #   # print(inFile)
  #   r <- raster::raster(inFile)
  #
  #
  #
  #   outFile <- paste(output, subFolder, outList[i], sep = "/")  ## Names output
  #
  #   ifelse(!dir.exists(file.path(paste(output, subFolder, sep = "/"))),              #if tmpOut Does not Exists
  #          dir.create(file.path(paste(output, subFolder, sep = "/"))),
  #          "Directory Already Exisits")        #create tmpOut
  #
  #   raster::writeRaster(r, outFile, overwrite = TRUE)  ## Saves at 25m resolution
  #
  #
  # }

  # ## Remove tmp saga files
  # unlink(paste(output, "saga", sep = "/"), recursive = TRUE)
}
