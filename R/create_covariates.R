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


create_covariates <- function(dtm, SAGApath = "",
                              output = "./cv-rasters",
                              layers = "all"){
  ## testing
  # dtm <- terra::rast(system.file("extdata", "DTM.tif", package = "PEMprepr"))
  # dtm <- dat2
  # layers <- "Filled_sinks"
  # output <- "~/workspace/tmp/bannas"
  # SAGApath <- ""



  ### In future this would be good to set as a lookup table and then have a single
  # sub-function that uses the table parameters

  ####### Options -- All the possible covariates ########
  layer_options <- c("Filled_sinks", "sinkroute", "dem_preproc", "slope_aspect_curve",
               "tCatchment", "tca", "sCatchment", "twi", "channelsNetwork",
               "Distance2Water", "MultiResFlatness", "MultiResFlatness2",
               "MultiResFlatness3", "TRI", "convergence", "Openness",
               "dah", "TPI", "RidgeValley", "MRN", "FlowAccumulation",
               "SlopeLength", "FlowAccumulation2", "FlowAccumulation3",
               "FlowPathLength", "FlowPathLength2", "FlowPathLength3", "LSFactor",
               "SolarRad", "Convexity", "VertDistance", "TCI_low",
               "SWI", "WindExp", "Texture", "Protection", "VRM",
               "MBI", "mscale_TPI", "RelPosition", "SlopeCurvatures",
               "SteepestSlope", "UpslopeArea")



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



  ############# Set up Environment ########################
  ##
  #   ## create output if it does not exist
  # ifelse(!dir.exists(file.path(output)),              #if tmpOut Does not Exists
  #         dir.create(file.path(output), recursive = TRUE), "Directory Already Exists")        #create tmpOut
  #

  # # Testing
  # # setwd("e:/workspace/2019/PEM_2020")
  # # output = "./cv-rasters"
  # # SAGApath = "C:/SAGA/"

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
  ifelse(!dir.exists(file.path(output)),              #if tmpOut Does not Exists
         dir.create(file.path(output)), print("Directory Already Exists"))        #create tmpOut

  rn <- terra::res(dtm)[1] ## Get the resolution


  saga_tmp_files <- paste(output, rn, sep = "/")
  ifelse(!dir.exists(file.path(saga_tmp_files)),              #if tmpOut Does not Exists
         dir.create(file.path(saga_tmp_files)), print("Directory Already Exists"))        #create tmpOut


  ## Convert to Saga format for processing ---------------------------------------
  ## DON'T CHANGE WD ---
  # rtnwd <- getwd() ## wd to return to
  # setwd(saga_tmp_files)

  sDTM <- "dtm.sdat"
  sDTM <- paste(saga_tmp_files, sDTM, sep= "/")
  # sDTM <- paste0(saga_tmp_files, sDTM)
  terra::writeRaster(dtm, sDTM, overwrite = TRUE)
  # raster::writeRaster(dtm, sDTM, drivername = "GTiff", overwrite = TRUE)  # save SAGA Version using rgdal

  ## Bit of a hack here -- SAGA does not like the output from raster package
  ## save it as gTiff, re-open using rgdal and export as SAGA ...
  # dtm <- rgdal::readGDAL(sDTM)

  # sDTM <- "dtm.sdat"
  # ## If the file exists delete and save over.
  # if(file.exists(sDTM)){
  #   unlink(sDTM)
  #   rgdal::writeGDAL(dtm, sDTM, drivername = "SAGA")  ## TRUE
  # } else {
  #   rgdal::writeGDAL(dtm, sDTM, drivername = "SAGA" )               ## FALSE
  # }
  # ## END HACK ------------------

  ############# Covariate File Names #############################################
  ### Create Names for all the covariates ... needed here so that dependancies are
  ### partially met

  ### This is ugly! re-write
  sDTM <- paste(saga_tmp_files, "dtm.sdat", sep= fns)
  sinksRoute <- paste(saga_tmp_files, "sinkroute.sgrd",sep = fns)
  sinksFilled <- paste(saga_tmp_files, "Filled_sinks.sgrd", sep = fns)
  dem_preproc <- paste(saga_tmp_files,"dem_preproc.sgrd", sep = fns)
  slope <- paste(saga_tmp_files, "slope.sgrd", sep = fns)
  aspect <- paste(saga_tmp_files, "aspect.sgrd", sep = fns)
  gencurve <- paste(saga_tmp_files, "gencurve.sgrd", sep = fns)
  totcurve <- paste(saga_tmp_files, "totcurve.sgrd", sep = fns)
  tCatchment <- paste(saga_tmp_files, "tCatchment.sgrd", sep = fns)
  tca <- paste(saga_tmp_files, "tca1.sgrd", sep = fns, sep = fns)
  flowlength4 <- paste(saga_tmp_files, "flowlength1.sgrd", sep = fns) ## part of tca
  sCatchment <- paste(saga_tmp_files, "sCatchment.sgrd", sep = fns)
  twi <- paste(saga_tmp_files, "twi.sgrd", sep = fns)
  channelsNetwork <- paste(saga_tmp_files, "cnetwork.sgrd", sep = fns)
  hDistance <-  paste(saga_tmp_files, "hdist.sgrd", sep = fns)
  vDistance  <- paste(saga_tmp_files, "vdist.sgrd", sep = fns)
  hDistance <-  paste(saga_tmp_files, "hdistnob.sgrd", sep = fns)
  vDistance  <- paste(saga_tmp_files, "vdistnob.sgrd", sep = fns)
  MRVBF <- paste(saga_tmp_files, "mrvbf.sgrd", sep = fns)
  MRRTF <- paste(saga_tmp_files, "mrrtf.sgrd", sep = fns)
  MRVBF2 <- paste(saga_tmp_files, "mrvbf2.sgrd", sep = fns)
  MRRTF2 <- paste(saga_tmp_files, "mrrtf2.sgrd", sep = fns)
  TRI <- paste(saga_tmp_files, "tri.sgrd", sep = fns)
  convergence <- paste(saga_tmp_files, "convergence.sgrd", sep = fns)
  opos <- paste(saga_tmp_files, "open_pos.sgrd", sep = fns)
  oneg <- paste(saga_tmp_files, "open_neg.sgrd", sep = fns)
  dAH <- paste(saga_tmp_files, "dah.sgrd", sep = fns)
  tpi <- paste(saga_tmp_files, "tpi.sgrd", sep = fns)
  val_depth = paste(saga_tmp_files, "val_depth.sgrd", sep = fns)
  RidgeLevel = paste(saga_tmp_files, "rid_level.sgrd", sep = fns)
  MRNCatchment = paste(saga_tmp_files, "mnr_area.sgrd", sep = fns)
  MRNMaxHeight = paste(saga_tmp_files, "mnr_mheight.sgrd", sep = fns)
  MRN = paste(saga_tmp_files, "mnr.sgrd", sep = fns)
  FlowAccumFT = paste(saga_tmp_files, "flow_accum_ft.sgrd", sep = fns)
  MeanOvCatch = paste(saga_tmp_files, "MeanOvCatch.sgrd", sep = fns)
  AccumMaterial = paste(saga_tmp_files, "AccumMaterial.sgrd", sep = fns)
  SlopeLength = paste(saga_tmp_files, "slength.sgrd", sep = fns)
  FlowAccumP = paste(saga_tmp_files, "flow_accum_p.sgrd", sep = fns)
  FlowAccumTD = paste(saga_tmp_files, "flow_accum_td.sgrd", sep = fns)
  MeanOvCatchTD = paste(saga_tmp_files, "MeanOvCatchTD.sgrd", sep = fns)
  AccumMaterialTD = paste(saga_tmp_files, "AccumMaterialTD.sgrd", sep = fns)
  FlowPathLenTD = paste(saga_tmp_files, "FlowPathLenTD.sgrd", sep = fns)
  FlowPathLength = paste(saga_tmp_files, "max_fp_l.sgrd", sep = fns)
  FlowPathLength = paste(saga_tmp_files, "max_fp_l1.sgrd", sep = fns)
  FloOwAccum = paste(saga_tmp_files, "slope_lts_fa.sgrd", sep = fns)
  LSFactor = paste(saga_tmp_files, "ls_factor.sgrd", sep = fns)
  DirInsol <- paste(saga_tmp_files, "direinso.sgrd", sep = fns)
  DifInsol <- paste(saga_tmp_files, "diffinso.sgrd", sep = fns)
  Convexity = paste(saga_tmp_files, "convexity.sgrd", sep = fns)
  VertDistance = paste(saga_tmp_files, "vert_dis.sgrd", sep = fns)
  TCILow = paste(saga_tmp_files, "tci_low.sgrd", sep = fns)
  CatchmentArea = paste(saga_tmp_files, "swi_area.sgrd", sep = fns)
  CatchmentSlope = paste(saga_tmp_files, "swi_slope.sgrd", sep = fns)
  ModCatchmentArea = paste(saga_tmp_files, "swi_area_mod.sgrd", sep = fns)
  TopoWetIndex = paste(saga_tmp_files, "swi_twi.sgrd", sep = fns)
  WindExp = paste(saga_tmp_files, "wind_exp_index.sgrd", sep = fns)
  Texture = paste(saga_tmp_files, "texture.sgrd", sep = fns)
  Protection = paste(saga_tmp_files, "protection.sgrd", sep = fns)
  VRM = paste(saga_tmp_files, "vrm.sgrd", sep = fns)
  MBI = paste(saga_tmp_files, "mbi.sgrd", sep = fns)
  TPI = paste(saga_tmp_files, "mscale_tpi.sgrd", sep = fns)
  SlopeHeight = paste(saga_tmp_files, "slope_height.sgrd", sep = fns)
  ValleyDepth = paste(saga_tmp_files, "ValleyDepth.sgrd", sep = fns) #don't need this as created above?
  NormHeight = paste(saga_tmp_files, "norm_height.sgrd", sep = fns)
  StandHeight = paste(saga_tmp_files, "stand_height.sgrd", sep = fns)
  MSPosition = paste(saga_tmp_files, "ms_position.sgrd", sep = fns)
  LocalCurve = paste(saga_tmp_files, "local_curv.sgrd", sep = fns)
  UpslopeCurve = paste(saga_tmp_files, "upslope_curv.sgrd", sep = fns)
  LocalUpCurve = paste(saga_tmp_files, "local_upslope_curv.sgrd", sep = fns)
  DownCurve = paste(saga_tmp_files, "down_curv.sgrd", sep = fns)
  LocalDownCurve = paste(saga_tmp_files, "local_downslope_curv.sgrd", sep = fns)
  SteepestSlope <- paste(saga_tmp_files, "steepest_slope.sgrd", sep = fns)
  UpslopeArea = paste(saga_tmp_files, "upslopearea.sgrd", sep = fns)



  ############################### BEGIN PROCESSING ###############################

  ####### >> 1 -- Fill Sinks XXL (Wang and Liu)  -----------------------------

  # Fill sinks in dem to prepare base DEM for other layers:

  #### STEP 1: preprocess DEM

  ## http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_5.html
  ## Module Fill Sinks XXL (Wang & Liu)

  # This module uses an algorithm proposed by Wang & Liu to identify and fill surface depressions in digital elevation models.
  # The method was enhanced to allow the creation of hydrologic sound elevation models, i.e. not only to fill the depression(s) but also to preserve a downward slope along the flow path. If desired, this is accomplished by preserving a minimum slope gradient (and thus elevation difference) between cells.
  # This version of the module is designed to work on large data sets (e.g. LIDAR data), with smaller datasets you might like to check out the fully featured standard version of the module.



  # IMPORTANT - renamed to demf in final output but uses sinksFilled in this script


  if ("Filled_sinks" %in% layers) {

    # sinksFilled <- "Filled_sinks.sgrd"
    # sinksFilled <- paste(saga_tmp_files, sinksFilled, sep = "/")
    sysCMD <- paste(saga_cmd, "ta_preprocessor 5", "-ELEV" ,
                    #file.path(gsub("sdat","sgrd", sDTM)),
                    sDTM,
                    "-FILLED", sinksFilled,
                    "-MINSLOPE ", 0.1
    )
    system(sysCMD)
  }

  # sinksFilled <- file.path(gsub("sdat","sgrd", sDTM))



  # Generate sink drainage route detection layer to use in preprocess DEM

  # original script uses ta_preprocessor tool 1 - sink drainage and rout
  #http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_1.html
  if ("sinkroute" %in% layers) {
    # sinksRoute <- paste(saga_tmp_files,
    #                     "sinkroute.sgrd",
    #                     sep = "/")
    # sinksRoute <- file.path(paste0(output, "/saga/", sinksRoute))  ##

    sysCMD <- paste(saga_cmd, "ta_preprocessor 1",
                    "-ELEVATION" , sDTM, #file.path(gsub("sdat","sgrd", sDTM)),
                    "-SINKROUTE", sinksRoute

    )
    system(sysCMD)
  }
  # preproces DEM version 2:  fills sinks (input requires DEM + sink detection layer
  # generated above)/
  #http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_2.html


  if ("dem_preproc" %in% layers) {
    # dem_preproc <- paste(saga_tmp_files,
    #                      "dem_preproc.sgrd",
    #                      sep = "/")
    #dem_preproc <- file.path(paste0(output, "/saga/", dem_preproc))

    sysCMD <- paste(saga_cmd, "ta_preprocessor 2",
                    "-DEM" , sDTM, #file.path(gsub("sdat","sgrd", sDTM)),
                    "-SINKROUTE", sinksRoute,
                    "-DEM_PREPROC", dem_preproc,
                    "-METHOD", 1,
                    "-THRESHOLD", 0

    )
    system(sysCMD)
  }

  ##### >> 2 -- Slope Aspect and Curvature -------------------------------

  if ("slope_aspect_curve" %in% layers) {
    # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html
    # slope <- paste(saga_tmp_files, "slope.sgrd", sep = "/")
    # # slope = file.path(paste0(output, "/saga/", slope))
    # aspect <- paste(saga_tmp_files, "aspect.sgrd", sep = "/")
    # # aspect = file.path(tmpOut, aspect)
    # gencurve <- paste(saga_tmp_files, "gencurve.sgrd", sep = "/")
    # # gencurve = file.path(tmpOut, gencurve)
    # totcurve <- paste(saga_tmp_files, "totcurve.sgrd", sep = "/")
    # # totcurve = file.path(tmpOut, totcurve)


    sysCMD <- paste(saga_cmd, "ta_morphometry 0", "-ELEVATION",
                    # file.path(gsub("sdat","sgrd", sDTM)),     # Input DTM
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

  # STILL TO DO : need to re-run gen curve and tot curve
  # slope and aspect are correct - gen curve and tot curve need to the rerun



  ##### >> 3 -- Total Catchment Area --------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_0.html
  if ("tCatchment" %in% layers) {

    # tCatchment <- paste(saga_tmp_files, "tCatchment.sgrd", sep = "/")
    # tCatchment = file.path(tmpOut, tCatchment)
    sysCMD <- paste(saga_cmd, "ta_hydrology 0", "-ELEVATION",
                    # file.path(gsub("sdat","sgrd", sDTM)),
                    sDTM,
                    "-FLOW", tCatchment,                                    # Output
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

    # tca <- paste(saga_tmp_files, "tca1.sgrd", sep = "/")
    # tca <- file.path(tmpOut, tca)
    # flowlength4 <- "flowlength1.sgrd"
    # flowlength4 <- file.path(tmpOut, flowlength4)

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

  # this is a new parameter - not calculated for Deception
  if ("sCatchment" %in% layers) {
    # sCatchment <- paste(saga_tmp_files, "sCatchment.sgrd", sep = "/")
    # sCatchment  = file.path(tmpOut, sCatchment)

    sysCMD <- paste(saga_cmd, "ta_hydrology 19", "-DEM", sinksFilled,       # Input from 1
                    "-SCA", sCatchment,                                     # Output
                    "-TCA", tCatchment,                                     # Input from 2
                    "-METHOD", 1                                            # Parameters
    )
    system(sysCMD)

  }

  ##### >> 5 -- Topographic Wetness Index --------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_20.html

  if ("twi" %in% layers) {
    # twi <- paste(saga_tmp_files, "twi.sgrd", sep = "/")
    # twi = file.path(tmpOut, twi)
    # sCatchment = file.path(tmpOut, sCatchment) # or tca?


    sysCMD <- paste(saga_cmd, "ta_hydrology 20",
                    "-SLOPE", slope,           # Input from 11
                    "-AREA", sCatchment,                                    # Input from 3
                    "-TWI", twi,                                            # Output
                    "-CONV",1,
                    "-METHOD", 1
    )
    system(sysCMD)
  }

  ##### >> 6 -- Channel Network -------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_0.html
  # https://sourceforge.net/projects/saga-gis/files/SAGA%20-%20Documentation/SAGA%20Documents/SagaManual.pdf/download

  if ("channelsNetwork" %in% layers) {
    # channelsNetwork <- paste(saga_tmp_files, "cnetwork.sgrd", sep = "/")
    # channelsNetwork = file.path(tmpOut, channelsNetwork)

    sysCMD <- paste(saga_cmd, "ta_channels 0",
                    "-ELEVATION", sinksFilled,     # Input from 1
                    "-CHNLNTWRK", channelsNetwork,                            # Output
                    "-INIT_GRID", tCatchment,                                 # Input from 2
                    "-INIT_VALUE", 1000000,
                    "-INIT_METHOD", 2,                # Based on SAGA Manual Documentation, p. 119
                    "-DIV_CELLS", 5.0,
                    "-MINLEN", 10.0                        # Default Parameters
    )
    system(sysCMD)
  }



  ##### >> 7 -- Overland Flow Distance to Channel Network -----------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_4.html
  if ("Distance2Water" %in% layers) {
    # hDistance <-  paste(saga_tmp_files, "hdist.sgrd", sep = "/")
    # vDistance  <- paste(saga_tmp_files, "vdist.sgrd", sep = "/")
    # hDistance = file.path(tmpOut, hDistance)
    # vDistance = file.path(tmpOut, vDistance)

    sysCMD <- paste(saga_cmd, "ta_channels 4",
                    "-ELEVATION", sinksFilled,        # Input from 1
                    "-CHANNELS", channelsNetwork,     # Input from 4
                    "-DISTANCE", hDistance,
                    "-DISTVERT", vDistance,           # Outputs
                    "-METHOD", 1,
                    "-BOUNDARY", 1                              # Parameters
    )
    system(sysCMD)
  }
  # note distnob created using XML script with no boundary. This shows NA for areas on the edge where
  # metrics cannot be calculated)

  if ("Distance2Water2" %in% layers) {
    # hDistance <-  paste(saga_tmp_files, "hdistnob.sgrd", sep = "/")
    # vDistance  <- paste(saga_tmp_files, "vdistnob.sgrd", sep = "/")
    # hDistance = file.path(tmpOut, hDistance)
    # vDistance = file.path(tmpOut, vDistance)

    sysCMD <- paste(saga_cmd, "ta_channels 4",
                    "-ELEVATION", sinksFilled,   # Input from 1
                    "-CHANNELS", channelsNetwork,                             # Input from 4
                    "-DISTANCE", hDistance,
                    "-DISTVERT", vDistance,           # Outputs
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
  if ("MultiResFlatness" %in% layers) {

    # MRVBF <- paste(saga_tmp_files, "mrvbf.sgrd", sep = "/")
    # MRRTF <- paste(saga_tmp_files, "mrrtf.sgrd", sep = "/")

    # MRVBF = file.path(tmpOut, MRVBF)
    # MRRTF  = file.path(tmpOut, MRRTF)

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
  if ("MultiResFlatness2" %in% layers) {

    # MRVBF2 <- paste(saga_tmp_files, "mrvbf2.sgrd", sep = "/")
    # MRRTF2 <- paste(saga_tmp_files, "mrrtf2.sgrd", sep = "/")

    # MRVBF2 = file.path(tmpOut, MRVBF2)
    # MRRTF2  = file.path(tmpOut, MRRTF2)

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
  # if ("MultiResFlatness3" %in% layers) {
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

  if ("TRI" %in% layers) {

    # TRI <- paste(saga_tmp_files, "tri.sgrd", sep = "/")
    # TRI  = file.path(tmpOut, TRI)
    sysCMD <- paste(saga_cmd, "ta_morphometry 16",
                    # "-DEM", file.path(gsub("sdat","sgrd", sDTM)),
                    sDTM,
                    "-TRI", TRI,  # Output
                    "-MODE", 0,
                    "-RADIUS", 3.0,
                    "-DW_WEIGHTING", 0          # Parameters
    )
    system(sysCMD)
  }

  ##### >> 10 -- Convergence Index -----------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_1.html


  if ("convergence" %in% layers) {

    # convergence <- paste(saga_tmp_files, "convergence.sgrd", sep = "/")
    # convergence  = file.path(tmpOut, convergence)
    sysCMD <- paste(saga_cmd, "ta_morphometry 1",
                    "-ELEVATION ",
                    # file.path(gsub("sdat","sgrd", sDTM)),      # Input DTM
                    sDTM,
                    "-RESULT", convergence,                                 # Output
                    "-METHOD", 1,
                    "-NEIGHBOURS", 1                          # Parameters
    )
    system(sysCMD)
  }

  ##### >> 11 -- Openness --------------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_5.html

  if ("Openness" %in% layers) {

    # opos <- paste(saga_tmp_files, "open_pos.sgrd", sep = "/")
    # # opos = file.path(tmpOut, opos)
    # oneg <- paste(saga_tmp_files, "open_neg.sgrd", sep = "/")
    # oneg = file.path(tmpOut, oneg)
    sysCMD <- paste(saga_cmd, "ta_lighting 5", "-DEM",
                    # file.path(gsub("sdat","sgrd", sDTM)),
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
    # dAH <- paste(saga_tmp_files, "dah.sgrd", sep = "/")
    sysCMD <- paste(saga_cmd, "ta_morphometry 12", "-DEM",
                    # file.path(gsub("sdat","sgrd", sDTM)), # Input DTM
                    sDTM,
                    "-DAH", dAH,                                            # Output
                    "-ALPHA_MAX", 202.5                                     # Default Parameters
    )
    system(sysCMD)
  }

  ##### >> 13 -- Topographic Position Index --------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_18.html
  if ("TPI" %in% layers) {
    # tpi <- paste(saga_tmp_files, "tpi.sgrd", sep = "/")
    # tpi= file.path(tmpOut, tpi)

    sysCMD <- paste(saga_cmd, "ta_morphometry 18", "-DEM",
                    # file.path(gsub("sdat","sgrd", sDTM)),# Input DTM
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
  if ("RidgeValley" %in% layers) {

    # val_depth = paste(saga_tmp_files, "val_depth.sgrd", sep = "/")
    # # val_depth = file.path(tmpOut, val_depth)
    #
    # RidgeLevel = paste(saga_tmp_files, "rid_level.sgrd", sep = "/")
    # # RidgeLevel = file.path(tmpOut, RidgeLevel)

    sysCMD = paste(saga_cmd, "ta_channels 7",
                   "-ELEVATION", sinksFilled,            # input DEM
                   "-VALLEY_DEPTH", val_depth,         # output Valley Depth
                   "-RIDGE_LEVEL", RidgeLevel,           # output Ridge Level
                   "-THRESHOLD", 1,
                   "-NOUNDERGROUND", 1,
                   "-ORDER", 4
    )
    system(sysCMD)
  }



  #### >> 15 -- Melton Ruggedness Number -------------------------- ## works
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_23.html
  if ("MRN" %in% layers) {

    # MRNCatchment = paste(saga_tmp_files, "mnr_area.sgrd", sep = "/")
    # # MRNCatchment = file.path(tmpOut, MRNCatchment)
    # MRNMaxHeight = paste(saga_tmp_files, "mnr_mheight.sgrd", sep = "/")
    # # MRNMaxHeight = file.path(tmpOut, MRNMaxHeight)
    # MRN = paste(saga_tmp_files, "mnr.sgrd", sep = "/")
    # # MRN = file.path(tmpOut, MRN)

    sysCMD = paste(saga_cmd, "ta_hydrology 23",
                   "-DEM", sinksFilled,                 # input DEM
                   "-AREA", MRNCatchment,               # output MRN Catchment
                   "-ZMAX", MRNMaxHeight,               # output MRN Max Height
                   "-MRN", MRN                          # output MRN
    )
    system(sysCMD)
  }

  #### >> 16 -- Flow Accumulation (Flow Tracing)  --------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_2.html

  if ("FlowAccumulation" %in% layers) {

    # FlowAccumFT = paste(saga_tmp_files, "flow_accum_ft.sgrd", sep = "/")
    # # FlowAccumFT = file.path(tmpOut, FlowAccumFT)
    # MeanOvCatch = paste(saga_tmp_files, "MeanOvCatch.sgrd", sep = "/")
    # # MeanOvCatch = file.path(tmpOut, MeanOvCatch)
    # AccumMaterial = paste(saga_tmp_files, "AccumMaterial.sgrd", sep = "/")
    #
    # # AccumMaterial = file.path(tmpOut, AccumMaterial)
    sysCMD = paste(saga_cmd, "ta_hydrology 2",
                   "-ELEVATION", sinksFilled,            # input DEM
                   "-FLOW", FlowAccumFT,                 # output Flow Accumulation
                   "-VAL_MEAN", MeanOvCatch,             # output Mean over Catchment
                   "-ACCU_TOTAL", AccumMaterial,         # output Accumulated Material
                   "-FLOW_UNIT", 1,
                   "-METHOD", 1,
                   "-MINDQV", 0
    )
    system(sysCMD)
  }

  #### >> 17 -- Slope Length --------------------------------------- ####
  ## works
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_7.html
  if ("SlopeLength" %in% layers) {

    # SlopeLength = paste(saga_tmp_files, "slength.sgrd", sep = "/")
    # SlopeLength = file.path(tmpOut, SlopeLength)
    sysCMD = paste(saga_cmd, "ta_hydrology 7",
                   "-DEM", sinksFilled,             # input DEM
                   "-LENGTH", SlopeLength            # output Slope Length
    )
    system(sysCMD)
  }


  #### >> 18 -- Flow Accumulation (Parallelizable) -------------------
  ## this tool doesn't seem to exist - SAGA version issue?
  # # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_29.html
  if ("FlowAccumulation2" %in% layers) {

    # FlowAccumP = paste(saga_tmp_files, "flow_accum_p.sgrd", sep = "/")
    # FlowAccumP = file.path(tmpOut, FlowAccumP)
    sysCMD = paste(saga_cmd, "ta_hydrology 29",
                   "-DEM", sinksFilled,                  # input DEM
                   "-FLOW", FlowAccumP,                  # output Flow Accumulation
                   "-METHOD", 2,
                   "-CONVERGENCE", 1.1
    )
    system(sysCMD)
  }

  #### >> 19 -- Flow Accumulation (Top-Down) ---------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_0.html
  #
  if ("FlowAccumulation3" %in% layers) {

    # FlowAccumTD = paste(saga_tmp_files, "flow_accum_td.sgrd", sep = "/")
    # # FlowAccumTD = file.path(tmpOut, FlowAccumTD)
    # MeanOvCatchTD = paste(saga_tmp_files, "MeanOvCatchTD.sgrd", sep = "/")
    # # MeanOvCatchTD = file.path(tmpOut, MeanOvCatchTD)
    # AccumMaterialTD = paste(saga_tmp_files, "AccumMaterialTD.sgrd", sep = "/")
    # # AccumMaterialTD = file.path(tmpOut, AccumMaterialTD)
    # FlowPathLenTD = paste(saga_tmp_files, "FlowPathLenTD.sgrd", sep = "/")

    # FlowPathLenTD = file.path(tmpOut, FlowPathLenTD)
    sysCMD = paste(saga_cmd, "ta_hydrology 0",
                   "-ELEVATION", sinksFilled,                 # input DEM
                   "-FLOW", FlowAccumTD,                      # output Flow Accumulation
                   "-VAL_MEAN", MeanOvCatchTD,                # output Mean over Catchment
                   "-ACCU_TOTAL", AccumMaterialTD,            # output Accumulated Material
                   "-FLOW_LENGTH", FlowPathLenTD,             # output Flow Path Length
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
  #                "-AREA", tCatchment,                # input Catchment Area
  #                "-SPI", StreamPower,               # output Stream Power Index
  #                "-CONV", 0
  # )
  # system(sysCMD)


  #### >> 21 -- Maximum Flow Path Length ---------------------------
  ## works
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_27.html
  if ("FlowPathLength" %in% layers) {

    # FlowPathLength = paste(saga_tmp_files, "max_fp_l.sgrd", sep = "/")
    # FlowPathLength = file.path(tmpOut, FlowPathLength)
    sysCMD = paste(saga_cmd, "ta_hydrology 27",
                   "-ELEVATION", sinksFilled,            # input DEM
                   "-DISTANCE", FlowPathLength,          # output Max Flow Path Length
                   "-DIRECTION", 0
    )
    system(sysCMD)
  }


  #### >> 21a -- Maximum Flow Path Length ---------------------------
  ## works
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_27.html
  #
  if ("FlowPathLength2" %in% layers) {

    # FlowPathLength = paste(saga_tmp_files, "max_fp_l1.sgrd", sep = "/")
    # FlowPathLength = file.path(tmpOut, FlowPathLength)
    sysCMD = paste(saga_cmd, "ta_hydrology 27",
                   "-ELEVATION", sinksFilled,            # input DEM
                   "-DISTANCE", FlowPathLength,          # output Max Flow Path Length
                   "-DIRECTION", 1
    )
    system(sysCMD)
    #
  }

  #### >> 22 -- Slope Limited Flow Accumulation -------------------
  ## works
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_26.html
  #
  # NEW VARIABLE - not generated for Deception
  #
  if ("FlowPathLength3" %in% layers) {

    # FlowAccum = paste(saga_tmp_files, "slope_lts_fa.sgrd", sep = "/")
    # FlowAccum = file.path(tmpOut, FlowAccum)
    sysCMD = paste(saga_cmd, "ta_hydrology 26",
                   "-DEM", sinksFilled,               # input DEM
                   "-FLOW", FlowAccum,                # output Flow Accumulation
                   "-SLOPE_MIN", 0,
                   "-SLOPE_MAX", 5,
                   "-B_FLOW", 0
    )
    system(sysCMD)
  }
  #### >> 23 -- LS Factor -----------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_22.html


  if ("LSFactor" %in% layers) {

    # LSFactor = paste(saga_tmp_files, "ls_factor.sgrd", sep = "/")
    # LSFactor = file.path(tmpOut, LSFactor)
    sysCMD = paste(saga_cmd, "ta_hydrology 22",
                   "-SLOPE", slope,                # input Slope
                   "-AREA", tCatchment,            # input Catchment Area
                   "-LS", LSFactor,                # output LS Factor
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
  if ("SolarRad" %in% layers) {

    # DirInsol <- paste(saga_tmp_files, "direinso.sgrd", sep = "/")
    # DifInsol <- paste(saga_tmp_files, "diffinso.sgrd", sep = "/")

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

  #### >> 25 -- Terrain Surface Convexity ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_21.html
  if ("Convexity" %in% layers) {

    # Convexity = paste(saga_tmp_files, "convexity.sgrd", sep = "/")
    # Convexity = file.path(tmpOut, Convexity)
    sysCMD = paste(saga_cmd, "ta_morphometry 21",
                   "-DEM", sinksFilled,                   # input DEM
                   "-CONVEXITY", Convexity,               # output Convexity
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




  if ("VertDistance" %in% layers) {

    #### >> 26 -- Vertical Distance to Channel Network ------------- ## froze - maybe just very slow?
    # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_3.html
    # VertDistance = paste(saga_tmp_files, "vert_dis.sgrd", sep = "/")
    # VertDistance = file.path(tmpOut, VertDistance)
    # channelsNetwork = file.path(tmpOut, channelsNetwork)
    # sinksFilled
    sysCMD = paste(saga_cmd, "ta_channels 3",
                   "-ELEVATION", sinksFilled,            # input DEM
                   "-CHANNELS", channelsNetwork,         # input Channel Network
                   "-DISTANCE", VertDistance,            # output
                   "-THRESHOLD", 1,
                   "-NOUNDERGROUND", 1
    )
    system(sysCMD)
  }


  #### >> 27 -- TCI Low -------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_24.html
  if ("TCI_low" %in% layers) {

    # TCILow = paste(saga_tmp_files, "tci_low.sgrd", sep = "/")
    # TCILow = file.path(tmpOut, TCILow)
    sysCMD = paste(saga_cmd, "ta_hydrology 24",
                   "-DISTANCE", VertDistance,            # input Vertical Distance to Channel Network
                   "-TWI", twi,                          # input TWI
                   "-TCILOW", TCILow                     # output TCI Low
    )
    system(sysCMD)
  }



  #### >> 28 -- SAGA Wetness Index -------------------------------- ## works but VERY slow (~18 hours)
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_15.html
  if ("SWI" %in% layers) {

    # CatchmentArea = paste(saga_tmp_files, "swi_area.sgrd", sep = "/")
    # # CatchmentArea = file.path(tmpOut, CatchmentArea)
    # CatchmentSlope = paste(saga_tmp_files, "swi_slope.sgrd", sep = "/")
    # # CatchmentSlope = file.path(tmpOut, CatchmentSlope)
    # ModCatchmentArea = paste(saga_tmp_files, "swi_area_mod.sgrd", sep = "/")
    # # ModCatchmentArea = file.path(tmpOut, ModCatchmentArea)
    # TopoWetIndex = paste(saga_tmp_files, "swi_twi.sgrd", sep = "/")
    # # TopoWetIndex = file.path(tmpOut, TopoWetIndex)
    sysCMD = paste(saga_cmd, "ta_hydrology 15",
                   "-DEM", sinksFilled,                 # input DEM
                   "-AREA", CatchmentArea,              # output Catchment Area
                   "-SLOPE", CatchmentSlope,            # output Catchment Slope
                   "-AREA_MOD", ModCatchmentArea,       # output Modified Catchment Area
                   "-TWI", TopoWetIndex,                # output TWI
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
  if ("WindExp" %in% layers) {

    # WindExp = paste(saga_tmp_files, "wind_exp_index.sgrd", sep = "/")
    # WindExp = file.path(tmpOut, WindExp)
    sysCMD = paste(saga_cmd, "ta_morphometry 27",
                   "-DEM", sinksFilled,                     # input DEM
                   "-EXPOSITION", WindExp,                  # output Wind Exposition Index
                   "-MAXDIST", 300,
                   "-STEP", 15,
                   "-ACCEL", 1.5
    )
    system(sysCMD)
  }


  if ("Texture" %in% layers) {

    #### >> 30 -- Terrain Surface Texture -----------------------------
    # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_20.html

    # Texture = paste(saga_tmp_files, "texture.sgrd", sep = "/")
    # Texture = file.path(tmpOut, Texture)
    sysCMD = paste(saga_cmd, "ta_morphometry 20",
                   "-DEM", sinksFilled,                      # input DEM
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


  if ("Protection" %in% layers) {

    #### >> 31 -- Morphometric Protection Index ----------------------
    # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_7.html
    # Protection = paste(saga_tmp_files, "protection.sgrd", sep = "/")
    # Protection = file.path(tmpOut, Protection)
    sysCMD = paste(saga_cmd, "ta_morphometry 7",
                   "-DEM", sinksFilled,                        # input DEM
                   "-PROTECTION", Protection,                  # output Morphometric Protection Index
                   "-RADIUS", 2000
    )
    system(sysCMD)
  }



  if ("VRM" %in% layers) {

    #### >> 32 -- Vector Ruggedness Measure ---------------------------
    # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_17.html

    # VRM = paste(saga_tmp_files, "vrm.sgrd", sep = "/")
    # VRM = file.path(tmpOut, VRM)
    sysCMD = paste(saga_cmd, "ta_morphometry 17",
                   "-DEM", sinksFilled,                      # input DEM
                   "-VRM", VRM,                              # output Vector Ruggedness Measure
                   "-MODE", 1,
                   "-DW_WEIGHTING", 0,
                   "-DW_IDW_POWER", 2,
                   "-DW_BANDWIDTH", 1
    )
    system(sysCMD)
  }



  if ("MBI" %in% layers) {


    #### >> 33 -- Mass Balance Index ----------------------------------
    # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_10.html
    # MBI = paste(saga_tmp_files, "mbi.sgrd", sep = "/")
    # MBI = file.path(tmpOut, MBI)
    sysCMD = paste(saga_cmd, "ta_morphometry 10",
                   "-DEM", sinksFilled,                 # input DEM
                   "-HREL", VertDistance,               # input Vertical Distance to Channel Network
                   "-MBI", MBI,                         # output Mass Balance Index
                   "-TSLOPE", 15,
                   "-TCURVE", 0.01,
                   "-THREL", 15
    )
    system(sysCMD)
  }


  if ("mscale_TPI" %in% layers) {


    #### >> 34 -- Multi-Scale Topographic Position Index --------------
    # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_28.html
    # TPI = paste(saga_tmp_files, "mscale_tpi.sgrd", sep = "/")
    # TPI = file.path(tmpOut, TPI)
    sysCMD = paste(saga_cmd, "ta_morphometry 28",
                   "-DEM", sinksFilled,                # input DEM
                   "-TPI", TPI,                        # output TPI
                   "SCALE_MIN", 1,
                   "SCALE_MAX", 8,
                   "SCALE_NUM", 3
    )
    system(sysCMD)
  }


  if ("RelPosition" %in% layers) {


    #### >> 35 -- Relative Heights and Slope Positions ----------------
    # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_14.html
    # SlopeHeight = paste(saga_tmp_files, "slope_height.sgrd", sep = "/")
    # # SlopeHeight = file.path(tmpOut, SlopeHeight)
    # ValleyDepth = paste(saga_tmp_files, "ValleyDepth.sgrd", sep = "/") #don't need this as created above?
    # # ValleyDepth = file.path(tmpOut, ValleyDepth)
    # NormHeight = paste(saga_tmp_files, "norm_height.sgrd", sep = "/")
    # # NormHeight = file.path(tmpOut, NormHeight)
    # StandHeight = paste(saga_tmp_files, "stand_height.sgrd", sep = "/")
    # # StandHeight = file.path(tmpOut, StandHeight)
    # MSPosition = paste(saga_tmp_files, "ms_position.sgrd", sep = "/")
    # # MSPosition = file.path(tmpOut, MSPosition)
    sysCMD = paste(saga_cmd, "ta_morphometry 14",
                   "-DEM", sinksFilled,                 # input DEM
                   "-HO", SlopeHeight,                  # output Slope Height
                   "-HU", ValleyDepth,                  # output Valley Depth
                   "-NH", NormHeight,                   # output Normalized Height
                   "-SH", StandHeight,                  # output Standardized Height
                   "-MS", MSPosition,                   # output Mid-Slope Position
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
  #                "-DEM", sinksFilled,                 # input DEM
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





  if ("SlopeCurvatures" %in% layers) {

    #### >> 37 -- Upslope and Downslope Curvature ---------------------
    # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_26.html
    # LocalCurve = paste(saga_tmp_files, "local_curv.sgrd", sep = "/")
    # # LocalCurve = file.path(tmpOut, LocalCurve)
    # UpslopeCurve = paste(saga_tmp_files, "upslope_curv.sgrd", sep = "/")
    # # UpslopeCurve = file.path(tmpOut, UpslopeCurve)
    # LocalUpCurve = paste(saga_tmp_files, "local_upslope_curv.sgrd", sep = "/")
    # # LocalUpCurve = file.path(tmpOut, LocalUpCurve)
    # DownCurve = paste(saga_tmp_files, "down_curv.sgrd", sep = "/")
    # # DownCurve = file.path(tmpOut, DownCurve)
    # LocalDownCurve = paste(saga_tmp_files, "local_downslope_curv.sgrd", sep = "/")
    # # LocalDownCurve = file.path(tmpOut, LocalDownCurve)
    sysCMD = paste(saga_cmd, "ta_morphometry 26",
                   "-DEM", sinksFilled,                         # input DEM
                   "-C_LOCAL", LocalCurve,                      # output Local Curvature
                   "-C_UP", UpslopeCurve,                       # output Upslope Curvature
                   "-C_UP_LOCAL", LocalUpCurve,                 # output Local Upslope Curvature
                   "-C_DOWN", DownCurve,                        # output Downslope Curvature
                   "-C_DOWN_LOCAL", LocalDownCurve,             # output Local Downslope Curvature
                   "-WEIGHTING", 0.5
    )
    system(sysCMD)
  }



  if ("SteepestSlope" %in% layers) {

    #### >> 38 -- Steepest Slope (Slope Aspect and Curvature) --------
    # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html
    # SteepestSlope <- paste(saga_tmp_files, "steepest_slope.sgrd", sep = "/")
    # SteepestSlope = file.path(tmpOut, SteepestSlope)
    sysCMD <- paste(saga_cmd, "ta_morphometry 0",
                    "-ELEVATION", sinksFilled,                              # input DEM
                    "-SLOPE", SteepestSlope,                                # output Steepest Slope
                    "-METHOD", 1,                                           # method 1 - steepest slope
                    "-UNIT_SLOPE", 0,
                    "-UNIT_ASPECT", 0
    )
    system(sysCMD)

  }




  if ("UpslopeArea" %in% layers) {

    # #### >> 39 -- Upslope Area -------------------------------------

    ## not included in deception run

    # http://www.saga-gis.org/saga_tool_doc/7.6.2/ta_hydrology_4.html
    # UpslopeArea = paste(saga_tmp_files, "upslopearea.sgrd", sep = "/")
    # UpslopeArea = file.path(tmpOut, UpslopeArea)
    sysCMD = paste(saga_cmd, "ta_hydrology 4",
                   "-ELEVATION", sinksFilled,           # input DEM
                   "-AREA", UpslopeArea,                # output Upslope Area
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
