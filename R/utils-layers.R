#' Layers
#'
#' @param layers Character vector. SAGA modules to call
#' @param moddir Internal Data. Cartesian coordinates of each strata
#' @param artifact Internal Data. Determines proportion of cells to plot
#' @family Layers
#' @name Layers
#' @return Vector of recursive layers to call.
NULL

#' Recursive layers call
#' @family Layers
#' @rdname Layers
#' @keywords internal
#' @export

recursive_layers_call <- function(layers, moddir = moddir, artifact = artifacts){

  #--- which modules are being called ---#

  moddirs <- moddir %>%
    dplyr::filter(module %in% layers)

  mods <- moddirs$module

  #--- determine recursive inputs of desired modules ---#

  while(any(moddirs[,2:3] != "")){

    p1 <- unique(moddirs$par1)

    p1 <- p1[!p1 %in% mods]

    p2 <- unique(moddirs$par2)

    p2 <- p2[!p2 %in% mods]

    v1 <- c(p1,p2)

    v1 <- v1[v1 != ""]

    moddirs <- moddir %>%
      dplyr::filter(module %in% v1)

    mods <- c(mods,moddirs$module)
  }

  #--- ordered vector of modules (and their recursive inputs) to call ---#

  layers_to_call <- mods[order(match(mods, moddir$module))]


  #--- if artifact is provided report the modules that produce artifacts if they are requested ---#

  if(!is.null(artifact)){

    artifactdirs <- artifact %>%
      dplyr::filter(artifacts == TRUE) %>%
      dplyr::select(metric)

    artifact_layers <- layers_to_call[layers_to_call %in% artifactdirs$metric]

    if(length(artifact_layers) != 0){

      warning(paste0(artifact_layers, collapse = ", ")," -- produce artifacts during tiled processing.", call. = FALSE)

    }

  }

  #--- modules needed to be called based on inputs ---#

  return(layers_to_call)

}

#' Crop tiles
#' @family Layers
#' @rdname Layers
#' @keywords internal
#' @export
#'
read_crop <- function(f, poly, tmp){

  #--- extract tile index to match with index in polygons - works up to 1000 tiles ---#
  index <- stringr::str_extract(f, pattern = "_([1-9]|[1-9][0-9]|[1-9][0-9][0-9]|1000)_") %>%
    stringr::str_replace_all(., pattern = "_", replacement = "") %>%
    as.numeric()

  #--- write rasters to tmp folder ---#
  terra::writeRaster(terra::rast(f) %>% terra::crop(y = poly[index,]), paste0(tmp,basename(f)), overwrite = TRUE)

}

#' Covariate file paths
#' @family Layers
#' @rdname Layers
#' @keywords internal
#' @export
#'
covariate_file_names <- function(outputdir, nm){

  sinkroute <- paste0(outputdir,"/sinkroute/",nm,"_sinkroute.sgrd")
  sinksfilled <- paste0(outputdir,"/sinksfilled/",nm,"_sinksfilled.sgrd")
  dem_preproc <- paste0(outputdir,"/dem_preproc/",nm,"_dem_preproc.sgrd")
  slope <- paste0(outputdir,"/slope_aspect_curve/",nm,"_slope.sgrd")
  aspect <- paste0(outputdir,"/slope_aspect_curve/",nm,"_aspect.sgrd")
  gencurve <- paste0(outputdir,"/slope_aspect_curve/",nm,"_gencurve.sgrd")
  totcurve <- paste0(outputdir,"/slope_aspect_curve/",nm,"_totcurve.sgrd")
  tcatchment <- paste0(outputdir,"/tcatchment/",nm,"_tcatchment.sgrd")
  tca <- paste0(outputdir,"/tca/",nm,"_tca1.sgrd")
  flowlength4 <- paste0(outputdir,"/tca/",nm,"_flowlength1.sgrd") ## part of tca
  scatchment <- paste0(outputdir,"/scatchment/",nm,"_scatchment.sgrd")
  twi <- paste0(outputdir,"/twi/",nm,"_twi.sgrd")
  channelsnetwork <- paste0(outputdir,"/channelsnetwork/",nm,"_cnetwork.sgrd")
  hdistance <-  paste0(outputdir,"/overlandflow/",nm,"_hdist.sgrd")
  vdistance  <- paste0(outputdir,"/overlandflow/",nm,"_vdist.sgrd")
  hdistancenob <-  paste0(outputdir,"/overlandflow2/",nm,"_hdistnob.sgrd")
  vdistancenob  <- paste0(outputdir,"/overlandflow2/",nm,"_vdistnob.sgrd")
  MRVBF <- paste0(outputdir,"/multiresflatness/",nm,"_mrvbf.sgrd")
  MRRTF <- paste0(outputdir,"/multiresflatness/",nm,"_mrrtf.sgrd")
  MRVBF2 <- paste0(outputdir,"/multiresflatness2/",nm,"_mrvbf2.sgrd")
  MRRTF2 <- paste0(outputdir,"/multiresflatness2/",nm,"_mrrtf2.sgrd")
  tri <- paste0(outputdir,"/tri/",nm,"_tri.sgrd")
  convergence <- paste0(outputdir,"/convergence/",nm,"_convergence.sgrd")
  opos <- paste0(outputdir,"/openness/",nm,"_open_pos.sgrd")
  oneg <- paste0(outputdir,"/openness/",nm,"_open_neg.sgrd")
  dAH <- paste0(outputdir,"/dah/",nm,"_dah.sgrd")
  tpi <- paste0(outputdir,"/tpi/",nm,"_tpi.sgrd")
  val_depth <- paste0(outputdir,"/ridgevalley/",nm,"_val_depth.sgrd")
  ridgelevel <- paste0(outputdir,"/ridgevalley/",nm,"_rid_level.sgrd")
  mrncatchment <- paste0(outputdir,"/mrn/",nm,"_mrn_area.sgrd")
  mrnmaxheight <- paste0(outputdir,"/mrn/",nm,"_mrn_mheight.sgrd")
  mrn <- paste0(outputdir,"/mrn/",nm,"_mrn.sgrd")
  flowaccumft <- paste0(outputdir,"/flowaccumulation/",nm,"_flow_accum_ft.sgrd")
  meanovcatch <- paste0(outputdir,"/flowaccumulation/",nm,"_meanovcatch.sgrd")
  accummaterial <- paste0(outputdir,"/flowaccumulation/",nm,"_accummaterial.sgrd")
  slopelength <- paste0(outputdir,"/slopelength/",nm,"_slength.sgrd")
  flowaccump <- paste0(outputdir,"/flowaccumulation2/",nm,"_flow_accum_p.sgrd")
  flowaccumtd <- paste0(outputdir,"/flowaccumulation3/",nm,"_flow_accum_td.sgrd")
  meanovcatchTD <- paste0(outputdir,"/flowaccumulation3/",nm,"_meanovcatchTD.sgrd")
  accummaterialTD <- paste0(outputdir,"/flowaccumulation3/",nm,"_accummaterialTD.sgrd")
  flowpathlenTD <- paste0(outputdir,"/flowaccumulation3/",nm,"_flowpathlenTD.sgrd")
  flowpathlength <- paste0(outputdir,"/flowpathlength/",nm,"_max_fp_l.sgrd")
  flowpathlength2 <- paste0(outputdir,"/flowpathlength2/",nm,"_max_fp_l2.sgrd")
  flowpathlength3 <- paste0(outputdir,"/flowpathlength3/",nm,"_max_fp_l3.sgrd")
  #FloOwAccum <- paste0(outputdir,"/slope_lts_fa/",nm,"_slope_lts_fa.sgrd") isnt called in create_covariates()
  lsfactor <- paste0(outputdir,"/lsfactor/",nm,"_ls_factor.sgrd")
  DirInsol <- paste0(outputdir,"/solarrad/",nm,"_direinso.sgrd")
  DifInsol <- paste0(outputdir,"/solarrad/",nm,"_diffinso.sgrd")
  convexity <- paste0(outputdir,"/convexity/",nm,"_convexity.sgrd")
  vertdistance <- paste0(outputdir,"/vertdistance/",nm,"_vert_dis.sgrd")
  tci_low <- paste0(outputdir,"/tci_low/",nm,"_tci_low.sgrd")
  catchmentarea <- paste0(outputdir,"/swi/",nm,"_swi_area.sgrd")
  catchmentslope <- paste0(outputdir,"/swi/",nm,"_swi_slope.sgrd")
  modcatchmentarea <- paste0(outputdir,"/swi/",nm,"_swi_area_mod.sgrd")
  topowetindex <- paste0(outputdir,"/swi/",nm,"_swi_twi.sgrd")
  windexp <- paste0(outputdir,"/windexp/",nm,"_wind_exp_index.sgrd")
  texture <- paste0(outputdir,"/texture/",nm,"_texture.sgrd")
  protection <- paste0(outputdir,"/protection/",nm,"_protection.sgrd")
  vrm <- paste0(outputdir,"/vrm/",nm,"_vrm.sgrd")
  mbi <- paste0(outputdir,"/mbi/",nm,"_mbi.sgrd")
  mscale_tpi <- paste0(outputdir,"/mscale_tpi/",nm,"_mscale_tpi.sgrd")
  slopeheight <- paste0(outputdir,"/relposition/",nm,"_slope_height.sgrd")
  valleydepth <- paste0(outputdir,"/relposition/",nm,"_valleydepth.sgrd") #don't need this as created above?
  normheight <- paste0(outputdir,"/relposition/",nm,"_norm_height.sgrd")
  standheight <- paste0(outputdir,"/relposition/",nm,"_stand_height.sgrd")
  msposition <- paste0(outputdir,"/relposition/",nm,"_ms_position.sgrd")
  localcurve <- paste0(outputdir,"/slopecurvatures/",nm,"_local_curv.sgrd")
  upslopecurve <- paste0(outputdir,"/slopecurvatures/",nm,"_upslope_curv.sgrd")
  localupcurve <- paste0(outputdir,"/slopecurvatures/",nm,"_local_upslope_curv.sgrd")
  downcurve <- paste0(outputdir,"/slopecurvatures/",nm,"_down_curv.sgrd")
  localdowncurve <- paste0(outputdir,"/slopecurvatures/",nm,"_local_downslope_curv.sgrd")
  steepestslope <- paste0(outputdir,"/steepestslope/",nm,"_steepest_slope.sgrd")
  upslopearea <- paste0(outputdir,"/upslopearea/",nm,"_upslopearea.sgrd")

  environment()

}
