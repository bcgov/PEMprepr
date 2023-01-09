#' Covariate raster generation from input dtm.
#'
#' _This is an update of cov_dtm.R replacing the processing with 02a_ from  BEC_DevExchange_Work_
#'
#' Takes a dtm and via SAGA GIS generates the covariates embeded in this function.
#' This script has been tested with SAGA 8.4.1 on Windows.
#' Depending on your system the path to `saga_cmd` may need to be specified.
#'
#'
#' @param dtm file.path. Path to raster file.
#' @param SAGApath file.path. SAGA directory on system.
#' On linux systems with SAGA GIS installed Use `SAGApath = ""`
#' @param output file.path. Directory where covariates will be written. If files already exist they will NOT be overwritten.
#' @param layers character vector. Covariates to be created. Default is \code(`all`)
#'  - call \code(`PEMprepr::layer_options`) for full list.
#' @keywords SAGA, covariates, predictors, raster
#' @examples
#' \dontrun{
#' #--- load in dtm and write to tempfile ---#
#' aoi_raw <- system.file("extdata", "aoi.gpkg", package ="PEMprepr")
#' aoi_raw <- sf::st_read(aoi_raw)
#' aoi <- PEMprepr::aoi_snap(aoi_raw, "shrink")
#' t25 <- create_template(aoi, 25)
#' trim_raw <- cded_raster(aoi)
#' trim <- terra::rast(trim_raw)
#' dtm <- terra::project(trim, t25)
#' tmp <- tempfile(fileext = ".tif")
#' writeRaster(dtm, tmp)
#'
#' #--- global vars ---#
#' dir <- "" #desired base output folder
#' SAGApath <- "" #change SAGA path to your own
#'
#' #--- create all SAGA covariates ---#
#' create_covariates(dtm = tmp, layers = "all", output = dir, SAGApath = SAGApath)
#' }
#'
#' @export

create_covariates <- function(dtm,
                              SAGApath = "",
                              output = "./cv-rasters",
                              layers = "all"){

  data("layer_options", envir=environment())
  data("moddir", envir=environment())
  data("artifacts", envir=environment())

  #--- error handling ---#
  #--- dtm ---#
  if(!is.character(dtm) || !file.exists(dtm)){

    stop(paste0("`dtm` must be the path to an existing file."), call. = FALSE)

  }

  #--- load dtm ---#
  dtmr <- terra::rast(x = dtm)

  #--- SAGA ---#

  if(!dir.exists(SAGApath)){

    stop(paste0("`SAGApath` must be the path to an existing SAGA directory"), call. = FALSE)

  }

  if(Sys.info()['sysname']=="Windows"){
    saga_cmd <- file.path(SAGApath, "saga_cmd.exe")
    fns      <- "\\" ### file name separator
  } else {
    saga_cmd <- "saga_cmd"
    fns      <- "/" ### file name separator

  }  ;
  z <- system(paste(saga_cmd, "-v"), intern = TRUE)  ## prints that SAGA version number -- confirming it works.
  z <- print(z)
  v <- suppressWarnings(as.numeric(unlist(strsplit(z, "[[:punct:][:space:]]+")[1])))
  v <- v[!is.na(v)][1:2]
  v <- as.numeric(paste(v[1], v[2], sep = "."))

  if (v < 7.6) {
    warning("SAGA-GIS is less that 7.6.  Not all covariates will generate.  Upgrade your SAGA, visit https://sourceforge.net/projects/saga-gis/files/")
  }

  #--- output ---#

  if(!is.character(output)){

    stop(paste0("`output` must be a file path."), call. = FALSE)

  }

  #--- layers ---#

  if(!is.character(layers)){

    stop(paste0("`layers` must be type character."), call. = FALSE)

  }

  if (isTRUE(layers == "all")) {
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

  #--- begin procesing ---#

  #--- extract dtm basename for naming convention ---#
  #--- helps to ensure tiles are matched properly during parallel processing ---#

  nm <- tools::file_path_sans_ext(basename(dtm))

  #--- check input layers for recursive requirements ---#
  layers <- recursive_layers_call(layers = layers, moddir = moddir, artifact = artifacts)

  #--- get resolution of dtm ---#
  rn <- terra::res(x = dtmr)[1]

  # OUTPUTS: ------------------------------------------------------------

  #--- check outputs ---#
  outputdir <- file.path(output,rn,"modules")

  if(any(!dir.exists(file.path(outputdir,layers)))){

    purrr::walk(file.path(outputdir,layers), dir.create, recursive = TRUE)

  }

  #--- check outputs ---#
  saga_tmp_files <- file.path(output,rn,"SAGA")
  if(!dir.exists(file.path(saga_tmp_files))){

    dir.create(saga_tmp_files, recursive = TRUE)

  }

  ## Convert to Saga format for processing ---------------------------------------
  sDTM <- file.path(saga_tmp_files,paste0(nm,".sdat"))

  if(!file.exists(sDTM)){

    terra::writeRaster(dtmr, sDTM, overwrite = TRUE)

    message(paste0(basename(sDTM))," written to temp folder.")

  } else {

    message(paste0(basename(sDTM))," already exists.")

  }

  sDTM <- file.path(saga_tmp_files,paste0(nm,".sgrd"))

  #--- covariate file names  call lyr$<NAME> to call from utilities function ---#

  lyr <- covariate_file_names(outputdir = outputdir, nm = nm)

  ############################### BEGIN PROCESSING ###############################

  ####### >> 1 -- Fill Sinks XXL (Wang and Liu)  -----------------------------

  # Fill sinks in dem to prepare base DEM for other layers:

  #### STEP 1: preprocess DEM

  ## http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_5.html
  ## Module Fill Sinks XXL (Wang & Liu)

  # This module uses an algorithm proposed by Wang & Liu to identify and fill surface depressions in digital elevation models.
  # The method was enhanced to allow the creation of hydrologic sound elevation models, i.e. not only to fill the depression(s) but also to preserve a downward slope along the flow path. If desired, this is accomplished by preserving a minimum slope gradient (and thus elevation difference) between cells.
  # This version of the module is designed to work on large data sets (e.g. LIDAR data), with smaller datasets you might like to check out the fully featured standard version of the module.

  if ("sinksfilled" %in% layers) {

    if(!file.exists(lyr$sinksfilled)){

      sysCMD <- paste(saga_cmd, "ta_preprocessor 5", "-ELEV" ,
                      sDTM,
                      "-FILLED", lyr$sinksfilled,
                      "-MINSLOPE ", 0.1
      )
      system(sysCMD)

    }

  }


  # Generate sink drainage route detection layer to use in preprocess DEM
  #http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_1.html

  if ("sinkroute" %in% layers) {

    if(!file.exists(lyr$sinkroute)){

      sysCMD <- paste(saga_cmd, "ta_preprocessor 1",
                      "-ELEVATION" , sDTM,
                      "-SINKROUTE", lyr$sinkroute

      )
      system(sysCMD)

    }
  }

  # preproces DEM version 2:  fills sinks (input requires DEM + sink detection layer
  # generated above)/
  #http://www.saga-gis.org/saga_tool_doc/2.2.2/ta_preprocessor_2.html


  if ("dem_preproc" %in% layers) {

    if(!file.exists(lyr$dem_preproc)){

      sysCMD <- paste(saga_cmd, "ta_preprocessor 2",
                      "-DEM" , sDTM,
                      "-SINKROUTE", lyr$sinkroute,
                      "-DEM_PREPROC", lyr$dem_preproc,
                      "-METHOD", 1,
                      "-THRESHOLD", 0

      )
      system(sysCMD)
    }
  }

  ##### >> 2 -- Slope Aspect and Curvature -------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html

  if ("slope_aspect_curve" %in% layers) {

    ls <- c(lyr$slope, lyr$aspect, lyr$gencurve, lyr$totcurve)

    if(all(!file.exists(ls))){

      sysCMD <- paste(saga_cmd, "ta_morphometry 0", "-ELEVATION",
                      sDTM,
                      "-SLOPE", lyr$slope,
                      "-ASPECT", lyr$aspect,                     # Outputs
                      "-C_GENE", lyr$gencurve,
                      "-C_TOTA", lyr$totcurve,                # Outputs
                      "-METHOD", 6,
                      "-UNIT_SLOPE", 0,
                      "-UNIT_ASPECT", 0       # Default Parameters
      )
      system(sysCMD)
    }
  }

  ##### >> 3 -- Total Catchment Area --------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_0.html

  if ("tcatchment" %in% layers) {

    if(!file.exists(lyr$tcatchment)){

      sysCMD <- paste(saga_cmd, "ta_hydrology 0", "-ELEVATION",
                      sDTM,
                      "-FLOW", lyr$tcatchment,                                    # Output
                      "-METHOD", 4                                            # Default Parameters
      )
      system(sysCMD)
    }
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

    ls <- c(lyr$tca, lyr$flowlength4)

    if(all(!file.exists(ls))){

      sysCMD <- paste(saga_cmd, "ta_hydrology 1",
                      "-ELEVATION",
                      sDTM,
                      # file.path(gsub("sdat","sgrd", sDTM)),
                      "-FLOW", lyr$tca,
                      "-FLOW_LENGTH", lyr$flowlength4,
                      "-FLOW_UNIT", 1,
                      "-METHOD", 3
      )
      system(sysCMD)
    }
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

    if(!file.exists(lyr$scatchment)){

      sysCMD <- paste(saga_cmd, "ta_hydrology 19", "-DEM", lyr$sinksfilled,       # Input from 1
                      "-SCA", lyr$scatchment,                                     # Output
                      "-TCA", lyr$tcatchment,                                     # Input from 2
                      "-METHOD", 1                                            # Parameters
      )
      system(sysCMD)
    }

  }

  ##### >> 5 -- Topographic Wetness Index --------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_hydrology_20.html

  if ("twi" %in% layers) {

    if(!file.exists(lyr$twi)){

      sysCMD <- paste(saga_cmd, "ta_hydrology 20",
                      "-SLOPE", lyr$slope,           # Input from 11
                      "-AREA", lyr$scatchment,                                    # Input from 3
                      "-TWI", lyr$twi,                                            # Output
                      "-CONV",1,
                      "-METHOD", 1
      )
      system(sysCMD)
    }
  }

  ##### >> 6 -- Channel Network -------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_0.html
  # https://sourceforge.net/projects/saga-gis/files/SAGA%20-%20Documentation/SAGA%20Documents/SagaManual.pdf/download

  if ("channelsnetwork" %in% layers) {

    if(!file.exists(lyr$channelsnetwork)){

      sysCMD <- paste(saga_cmd, "ta_channels 0",
                      "-ELEVATION", lyr$sinksfilled,     # Input from 1
                      "-CHNLNTWRK", lyr$channelsnetwork,                            # Output
                      "-INIT_GRID", lyr$tcatchment,                                 # Input from 2
                      "-INIT_VALUE", 1000000,
                      "-INIT_METHOD", 2,                # Based on SAGA Manual Documentation, p. 119
                      "-DIV_CELLS", 5.0,
                      "-MINLEN", 10.0                        # Default Parameters
      )
      system(sysCMD)
    }
  }


  ##### >> 7 -- Overland Flow Distance to Channel Network -----------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_channels_4.html

  if ("overlandflow" %in% layers) {

    ls <- c(lyr$hdistance, lyr$vdistance)

    if(all(!file.exists(ls))){

      sysCMD <- paste(saga_cmd, "ta_channels 4",
                      "-ELEVATION", lyr$sinksfilled,        # Input from 1
                      "-CHANNELS", lyr$channelsnetwork,     # Input from 4
                      "-DISTANCE", lyr$hdistance,
                      "-DISTVERT", lyr$vdistance,           # Outputs
                      "-METHOD", 1,
                      "-BOUNDARY", 1                              # Parameters
      )
      system(sysCMD)
    }
  }
  # note distnob created using XML script with no boundary. This shows NA for areas on the edge where
  # metrics cannot be calculated)

  if ("overlandflow2" %in% layers) {

    ls <- c(lyr$hdistancenob, lyr$vdistancenob)

    if(all(!file.exists(ls))){

      sysCMD <- paste(saga_cmd, "ta_channels 4",
                      "-ELEVATION", lyr$sinksfilled,   # Input from 1
                      "-CHANNELS", lyr$channelsnetwork,                             # Input from 4
                      "-DISTANCE", lyr$hdistancenob,
                      "-DISTVERT", lyr$vdistancenob,           # Outputs
                      "-METHOD", 1,
                      "-BOUNDARY", 0                              # Parameters
      )
      system(sysCMD)
    }
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

    ls <- c(lyr$MRVBF, lyr$MRRTF)

    if(all(!file.exists(ls))){

      # use defaul parameters
      sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                      # file.path(gsub("sdat","sgrd", sDTM)),
                      sDTM,
                      "-MRVBF", lyr$MRVBF,
                      "-MRRTF", lyr$MRRTF,                       # Outputs
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
  }
  # Test a Variety of paramter and method versions.
  # Note these need to be converted from XML chain format to standard format

  # tested a number of MRVBF options for the t-slope parameter #ie
  # use dem_preproces for input and lowered the slope parameter from 15 to 10
  #
  if ("multiresflatness2" %in% layers) {

    ls <- c(lyr$MRVBF2, lyr$MRRTF2)

    if(all(!file.exists(ls))){

      #  Adjust parameters -  Option 2.
      sysCMD <- paste(saga_cmd, "ta_morphometry 8", "-DEM",
                      # file.path(gsub("sdat","sgrd", sDTM)),
                      sDTM,
                      "-MRVBF", lyr$MRVBF2,
                      "-MRRTF", lyr$MRRTF2,
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

    if(!file.exists(lyr$tri)){

      sysCMD <- paste(saga_cmd, "ta_morphometry 16",
                      "-DEM", sDTM,
                      "-TRI", lyr$tri,  # Output
                      "-MODE", 0,
                      "-RADIUS", 3.0,
                      "-DW_WEIGHTING", 0          # Parameters
      )
      system(sysCMD)
    }
  }

  ##### >> 10 -- Convergence Index -----------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_1.html


  if ("convergence" %in% layers) {

    if(!file.exists(lyr$convergence)){

      sysCMD <- paste(saga_cmd, "ta_morphometry 1",
                      "-ELEVATION ", sDTM,
                      "-RESULT", lyr$convergence,                                 # Output
                      "-METHOD", 1,
                      "-NEIGHBOURS", 1                          # Parameters
      )
      system(sysCMD)
    }
  }

  ##### >> 11 -- openness --------------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_5.html

  if ("openness" %in% layers) {

    ls <- c(lyr$opos, lyr$oneg)

    if(all(!file.exists(ls))){

      sysCMD <- paste(saga_cmd, "ta_lighting 5", "-DEM",
                      sDTM,
                      "-POS", lyr$opos,
                      "-NEG", lyr$oneg,                               # Outputs
                      "-RADIUS", 1000,
                      "-METHOD", 0,
                      "-DLEVEL",  3,
                      "-NDIRS", 8
      )
      system(sysCMD)
    }
  }

  ##### >> 12 -- Diuranal Anisotropic Heating -----------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_12.html

  if ("dah" %in% layers) {

    if(!file.exists(lyr$dAH)){

      sysCMD <- paste(saga_cmd, "ta_morphometry 12", "-DEM",
                      sDTM,
                      "-DAH", lyr$dAH,                                            # Output
                      "-ALPHA_MAX", 202.5                                     # Default Parameters
      )
      system(sysCMD)

    }
  }

  ##### >> 13 -- Topographic Position Index --------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_18.html

  if ("tpi" %in% layers) {

    if(!file.exists(lyr$tpi)){

      sysCMD <- paste(saga_cmd, "ta_morphometry 18",
                      "-DEM", sDTM,
                      "-TPI", lyr$tpi,                                            # Output
                      "-STANDARD", 0,
                      "-RADIUS_MIN", 0,
                      "-RADIUS_MAX", 100,   # Default Parameters
                      "-DW_WEIGHTING", 0,
                      "-DW_IDW_POWER", 1,
                      #"-DW_IDW_OFFSET", "1", # NO LONGER A PARAMETER IN SAGA v8.4.1
                      "-DW_BANDWIDTH", 75
      )
      system(sysCMD)
    }
  }

  # re-run - may need to adjust the radius min and radius max???


  #### >> 14 -- Valley Depth -------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_7.html

  if ("ridgevalley" %in% layers) {

    ls <- c(lyr$val_depth, lyr$ridgelevel)

    if(all(!file.exists(ls))){

      sysCMD = paste(saga_cmd, "ta_channels 7",
                     "-ELEVATION", lyr$sinksfilled,            # input DEM
                     "-VALLEY_DEPTH", lyr$val_depth,         # output Valley Depth
                     "-RIDGE_LEVEL", lyr$ridgelevel,           # output Ridge Level
                     "-THRESHOLD", 1,
                     "-NOUNDERGROUND", 1,
                     "-ORDER", 4
      )
      system(sysCMD)
    }
  }


  #### >> 15 -- Melton Ruggedness Number -------------------------- ## works
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_23.html

  if ("mrn" %in% layers) {

    ls <- c(lyr$mrncatchment, lyr$mrnmaxheight, lyr$mrn)

    if(all(!file.exists(ls))){

      sysCMD = paste(saga_cmd, "ta_hydrology 23",
                     "-DEM", lyr$sinksfilled,                 # input DEM
                     "-AREA", lyr$mrncatchment,               # output MRN Catchment
                     "-ZMAX", lyr$mrnmaxheight,               # output MRN Max Height
                     "-MRN", lyr$mrn                          # output MRN
      )
      system(sysCMD)
    }
  }

  #### >> 16 -- Flow Accumulation (Flow Tracing)  --------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_2.html

  if ("flowaccumulation" %in% layers) {

    ls <- c(lyr$flowaccumft, lyr$meanovcatch, lyr$accummaterial)

    if(all(!file.exists(ls))){

      sysCMD = paste(saga_cmd, "ta_hydrology 2",
                     "-ELEVATION", lyr$sinksfilled,            # input DEM
                     "-FLOW", lyr$flowaccumft,                 # output Flow Accumulation
                     "-VAL_MEAN", lyr$meanovcatch,             # output Mean over Catchment
                     "-ACCU_TOTAL", lyr$accummaterial,         # output Accumulated Material
                     "-FLOW_UNIT", 1,
                     "-METHOD", 1,
                     "-MINDQV", 0
      )
      system(sysCMD)
    }
  }

  #### >> 17 -- Slope Length ---------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_7.html

  if ("slopelength" %in% layers) {

    if(!file.exists(lyr$slopelength)){

      sysCMD = paste(saga_cmd, "ta_hydrology 7",
                     "-DEM", lyr$sinksfilled,             # input DEM
                     "-LENGTH", lyr$slopelength            # output Slope Length
      )
      system(sysCMD)
    }
  }


  #### >> 18 -- Flow Accumulation (Parallelizable) -------------------
  ## this tool doesn't seem to exist - SAGA version issue?
  # # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_29.html

  if ("flowaccumulation2" %in% layers) {

    if(!file.exists(lyr$flowaccump)){

      sysCMD = paste(saga_cmd, "ta_hydrology 29",
                     "-DEM", lyr$sinksfilled,                  # input DEM
                     "-FLOW", lyr$flowaccump,                  # output Flow Accumulation
                     "-METHOD", 2,
                     "-CONVERGENCE", 1.1
      )
      system(sysCMD)
    }
  }

  #### >> 19 -- Flow Accumulation (Top-Down) ---------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_0.html

  if ("flowaccumulation3" %in% layers) {

    ls <- c(lyr$flowaccumtd, lyr$meanovcatchTD, lyr$accummaterialTD, lyr$flowpathlenTD)

    if(all(!file.exists(ls))){

      sysCMD = paste(saga_cmd, "ta_hydrology 0",
                     "-ELEVATION", lyr$sinksfilled,                 # input DEM
                     "-FLOW", lyr$flowaccumtd,                      # output Flow Accumulation
                     "-VAL_MEAN", lyr$meanovcatchTD,                # output Mean over Catchment
                     "-ACCU_TOTAL", lyr$accummaterialTD,            # output Accumulated Material
                     "-FLOW_LENGTH", lyr$flowpathlenTD,             # output Flow Path Length
                     "-FLOW_UNIT", 1,
                     "-METHOD", 4,
                     "-LINEAR_DO", 1,
                     "-LINEAR_MIN", 500,
                     "-CONVERGENCE", 1.1
      )
      system(sysCMD)
    }
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

    if(!file.exists(lyr$flowpathlength)){

      sysCMD = paste(saga_cmd, "ta_hydrology 27",
                     "-ELEVATION", lyr$sinksfilled,            # input DEM
                     "-DISTANCE", lyr$flowpathlength,          # output Max Flow Path Length
                     "-DIRECTION", 0
      )
      system(sysCMD)
    }
  }


  #### >> 21a -- Maximum Flow Path Length ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_27.html
  #
  if ("flowpathlength2" %in% layers) {

    if(!file.exists(lyr$flowpathlength2)){

      sysCMD = paste(saga_cmd, "ta_hydrology 27",
                     "-ELEVATION", lyr$sinksfilled,            # input DEM
                     "-DISTANCE", lyr$flowpathlength2,          # output Max Flow Path Length
                     "-DIRECTION", 1
      )
      system(sysCMD)
    }
  }

  #### >> 22 -- Slope Limited Flow Accumulation -------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_26.html

  if ("flowpathlength3" %in% layers) {

    if(!file.exists(lyr$flowpathlength3)){

      sysCMD = paste(saga_cmd, "ta_hydrology 26",
                     "-DEM", lyr$sinksfilled,               # input DEM
                     "-FLOW", lyr$flowpathlength3,                # output Flow Accumulation
                     "-SLOPE_MIN", 0,
                     "-SLOPE_MAX", 5,
                     "-B_FLOW", 0
      )
      system(sysCMD)
    }
  }
  #### >> 23 -- LS Factor -----------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_22.html


  if ("lsfactor" %in% layers) {

    if(!file.exists(lyr$lsfactor)){

      sysCMD = paste(saga_cmd, "ta_hydrology 22",
                     "-SLOPE", lyr$slope,                # input Slope
                     "-AREA", lyr$tcatchment,            # input Catchment Area
                     "-LS", lyr$lsfactor,                # output LS Factor
                     "-CONV", 0,
                     "-METHOD", 0,
                     "-EROSIVITY", 1,
                     "-STABILITY", 0
      )
      system(sysCMD)
    }
  }



  #### >> 24 -- Solar covariates  -----------------------------------------
  # solar direct and diffuse solar radiation
  # adjust min and max limits to 4am and 22 pm to reduce processing time
  #http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_lighting_2.html
  #Calculation of potential incoming solar radiation (insolation). Times of sunrise/sunset will only be calculated if time span is set to single day.
  if ("solarrad" %in% layers) {

    ls <- c(lyr$DirInsol, lyr$DifInsol)

    if(all(!file.exists(ls))){

      sysCMD <- paste(saga_cmd, "ta_lighting 2",
                      "-GRD_DEM",
                      # file.path(gsub("sdat","sgrd", sDTM)),# Input DTM
                      sDTM,
                      "-GRD_DIRECT", lyr$DirInsol,
                      "-GRD_DIFFUS", lyr$DifInsol,       # Outputs
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
  }

  #### >> 25 -- Terrain Surface convexity ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_21.html

  if ("convexity" %in% layers) {

    if(!file.exists(lyr$convexity)){

      sysCMD = paste(saga_cmd, "ta_morphometry 21",
                     "-DEM", lyr$sinksfilled,                   # input DEM
                     "-CONVEXITY", lyr$convexity,               # output Convexity
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
  }

  #### >> 26 -- Vertical Distance to Channel Network ------------- ## froze - maybe just very slow?
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_channels_3.html

  if ("vertdistance" %in% layers) {

    if(!file.exists(lyr$vertdistance)){

      sysCMD = paste(saga_cmd, "ta_channels 3",
                     "-ELEVATION", lyr$sinksfilled,            # input DEM
                     "-CHANNELS", lyr$channelsnetwork,         # input Channel Network
                     "-DISTANCE", lyr$vertdistance,            # output
                     "-THRESHOLD", 1,
                     "-NOUNDERGROUND", 1
      )
      system(sysCMD)
    }
  }


  #### >> 27 -- TCI Low -------------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_24.html

  if ("tci_low" %in% layers) {

    if(!file.exists(lyr$tci_low)){

      sysCMD = paste(saga_cmd, "ta_hydrology 24",
                     "-DISTANCE", lyr$vertdistance,            # input Vertical Distance to Channel Network
                     "-TWI", lyr$twi,                          # input TWI
                     "-TCILOW", lyr$tci_low                     # output TCI Low
      )
      system(sysCMD)
    }
  }

  #### >> 28 -- SAGA Wetness Index -------------------------------- ## works but VERY slow (~18 hours)
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_hydrology_15.html

  if ("swi" %in% layers) {

    ls <- c(lyr$catchmentarea, lyr$catchmentslope, lyr$modcatchmentarea, lyr$topowetindex)

    if(all(!file.exists(ls))){

      sysCMD = paste(saga_cmd, "ta_hydrology 15",
                     "-DEM", lyr$sinksfilled,                 # input DEM
                     "-AREA", lyr$catchmentarea,              # output Catchment Area
                     "-SLOPE", lyr$catchmentslope,            # output Catchment Slope
                     "-AREA_MOD",lyr$modcatchmentarea,       # output Modified Catchment Area
                     "-TWI", lyr$topowetindex,                # output TWI
                     "-SUCTION", 10,
                     "-AREA_TYPE", 1,
                     "-SLOPE_TYPE", 1,
                     "-SLOPE_MIN", 0,
                     "-SLOPE_OFF", 0.1,
                     "-SLOPE_WEIGHT", 1
      )
      system(sysCMD)
    }
  }

  #### >> 29 -- Wind Exposition Index ------------------------------ ## works but VERY slow
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_27.html

  if ("windexp" %in% layers) {

    if(!file.exists(lyr$windexp)){

      sysCMD = paste(saga_cmd, "ta_morphometry 27",
                     "-DEM", lyr$sinksfilled,                     # input DEM
                     "-EXPOSITION", lyr$windexp,                  # output Wind Exposition Index
                     "-MAXDIST", 300,
                     "-STEP", 15,
                     "-ACCEL", 1.5
      )
      system(sysCMD)
    }
  }

  #### >> 30 -- Terrain Surface Texture -----------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_20.html

  if ("texture" %in% layers) {

    if(!file.exists(lyr$texture)){

      sysCMD = paste(saga_cmd, "ta_morphometry 20",
                     "-DEM", lyr$sinksfilled,                      # input DEM
                     "-TEXTURE", lyr$texture,                      # output Terrain Surface Texture
                     "-EPSILON", 1,
                     "-SCALE", 10,
                     "-METHOD", 1,
                     "-DW_WEIGHTING", 0,
                     "-DW_IDW_POWER", 2,
                     "-DW_BANDWIDTH", 1
      )
      system(sysCMD)
    }
  }

  #### >> 31 -- Morphometric Protection Index ----------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_7.html

  if ("protection" %in% layers) {

    if(!file.exists(lyr$protection)){

      sysCMD = paste(saga_cmd, "ta_morphometry 7",
                     "-DEM", lyr$sinksfilled,                        # input DEM
                     "-PROTECTION", lyr$protection,                  # output Morphometric Protection Index
                     "-RADIUS", 2000
      )
      system(sysCMD)
    }
  }


  #### >> 32 -- Vector Ruggedness Measure ---------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_17.html

  if ("vrm" %in% layers) {

    if(!file.exists(lyr$vrm)){

      sysCMD = paste(saga_cmd, "ta_morphometry 17",
                     "-DEM", lyr$sinksfilled,                      # input DEM
                     "-VRM", lyr$vrm,                              # output Vector Ruggedness Measure
                     "-MODE", 1,
                     "-DW_WEIGHTING", 0,
                     "-DW_IDW_POWER", 2,
                     "-DW_BANDWIDTH", 1
      )
      system(sysCMD)
    }
  }

  #### >> 33 -- Mass Balance Index ----------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_10.html

  if ("mbi" %in% layers) {

    if(!file.exists(lyr$mbi)){

      sysCMD = paste(saga_cmd, "ta_morphometry 10",
                     "-DEM", lyr$sinksfilled,                 # input DEM
                     "-HREL", lyr$vertdistance,               # input Vertical Distance to Channel Network
                     "-MBI", lyr$mbi,                         # output Mass Balance Index
                     "-TSLOPE", 15,
                     "-TCURVE", 0.01,
                     "-THREL", 15
      )
      system(sysCMD)
    }
  }

  #### >> 34 -- Multi-Scale Topographic Position Index --------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_28.html

  if ("mscale_tpi" %in% layers) {

    if(!file.exists(lyr$mscale_tpi)){

      sysCMD = paste(saga_cmd, "ta_morphometry 28",
                     "-DEM", lyr$sinksfilled,                # input DEM
                     "-TPI", lyr$mscale_tpi,                        # output tpi
                     "-SCALE_MIN", 1,
                     "-SCALE_MAX", 8,
                     "-SCALE_NUM", 3
      )
      system(sysCMD)
    }
  }

  #### >> 35 -- Relative Heights and Slope Positions ----------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.0/ta_morphometry_14.html

  if ("relposition" %in% layers) {

    ls <- c(lyr$slopeheight, lyr$valleydepth, lyr$normheight, lyr$standheight, lyr$msposition)

    if(all(!file.exists(ls))){

      sysCMD = paste(saga_cmd, "ta_morphometry 14",
                     "-DEM", lyr$sinksfilled,                 # input DEM
                     "-HO", lyr$slopeheight,                  # output Slope Height
                     "-HU", lyr$valleydepth,                  # output Valley Depth
                     "-NH", lyr$normheight,                   # output Normalized Height
                     "-SH", lyr$standheight,                  # output Standardized Height
                     "-MS", lyr$msposition,                   # output Mid-Slope Position
                     "-W", 0.5,
                     "-T", 10,
                     "-E", 2
      )
      system(sysCMD)
    }
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

    ls <- c(lyr$localcurve, lyr$upslopecurve, lyr$localupcurve, lyr$downcurve, lyr$localdowncurve)

    if(all(!file.exists(ls))){

      sysCMD = paste(saga_cmd, "ta_morphometry 26",
                     "-DEM", lyr$sinksfilled,                         # input DEM
                     "-C_LOCAL", lyr$localcurve,                      # output Local Curvature
                     "-C_UP", lyr$upslopecurve,                       # output Upslope Curvature
                     "-C_UP_LOCAL", lyr$localupcurve,                 # output Local Upslope Curvature
                     "-C_DOWN", lyr$downcurve,                        # output Downslope Curvature
                     "-C_DOWN_LOCAL", lyr$localdowncurve,             # output Local Downslope Curvature
                     "-WEIGHTING", 0.5
      )
      system(sysCMD)
    }
  }

  #### >> 38 -- Steepest Slope (Slope Aspect and Curvature) --------
  # http://www.saga-gis.org/saga_tool_doc/7.2.0/ta_morphometry_0.html


  if ("steepestslope" %in% layers) {

    if(!file.exists(lyr$steepestslope)){

      sysCMD <- paste(saga_cmd, "ta_morphometry 0",
                      "-ELEVATION", lyr$sinksfilled,                              # input DEM
                      "-SLOPE", lyr$steepestslope,                                # output Steepest Slope
                      "-METHOD", 1,                                           # method 1 - steepest slope
                      "-UNIT_SLOPE", 0,
                      "-UNIT_ASPECT", 0
      )
      system(sysCMD)
    }

  }

  # #### >> 39 -- Upslope Area -------------------------------------
  # http://www.saga-gis.org/saga_tool_doc/7.6.2/ta_hydrology_4.html

  if ("upslopearea" %in% layers) {

    if(!file.exists(lyr$upslopearea)){

      sysCMD = paste(saga_cmd, "ta_hydrology 4",
                     "-ELEVATION", lyr$sinksfilled,           # input DEM
                     "-SINKROUTE", lyr$sinkroute,
                     "-AREA", lyr$upslopearea,                # output Upslope Area
                     "-METHOD", 2,
                     "-CONVERGE", 1.1
      )
      system(sysCMD)
    }


  }

}
