## Original GP notes at bottome
## ISSUES
## - TEM download not working
## - options call ... change to global env.
## - review look for water ... overwriting issue?
## ... water is as far as I got.
## make it BEC label?



#' Retrieves essential base vectors
#'
#' Collects vector data from the [BC Data Catelog](https://catalogue.data.gov.bc.ca) for a spcified area of interest.
#'
#' @details
#' This script downloads the relevant spatial data for BEC zones, VRI, TEM,
#' waterbodies and the road network used in stage 1 of PEM processing.
#' Data is downloaded directly from the
#' [BC Data Catalogue](https://catalogue.data.gov.bc.ca/dataset?download_audience=Public)
#' using the [bcdata](https://github.com/bcgov/bcdata) package.
#'
#' Datasets retrieved include:
#' - BEC - [Biogeoclimatic Ecosystems Classification](https://catalogue.data.gov.bc.ca/dataset/bec-map) are used to define and/or select specific "subzones" within a defined study area. Note this can be also pointed to different file where more updated version is available.
#' - Vegetation Resource Inventory (VRI) - This layer includes a variety of vegetation measure, including cutblock age, TEM data and ..... Detailed data standards can be found [here](https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/data-management-and-access/vri-data-standards).
#' - Freshwater Atlas -  The atlas is separated into the different types of waterbodies (lakes, rivers, wetlands, streams, man made, etc.), which requires a seperate download per type. Alternatively a single combined layer can be downloaded by is limited to linear data type. A parameter within the function can be set to "polygon" (the default option) or "linear".
#' - Road network THIS STILL NEEEDS WORK
#' - Road network - In previous works, the [raw road network](https://catalogue.data.gov.bc.ca/dataset/digital-road-atlas-dra-master-partially-attributed-roads) was found to be too detailed. The raw road network is filtered to only keep named roads, and then the up to date [Forest Service Road (FSR) layer](https://catalogue.data.gov.bc.ca/dataset/forest-tenure-road-segment-lines) is downloaded and merged with the filtered road network to produce a really good representation of where roads are actually located on the landscape.
#' - Fire - Fire and fire intensity are used used to assist in identifying areas where a high cost is applied to reduce sampling. This includes:
#'     -[current](https://catalogue.data.gov.bc.ca/dataset/fire-perimeters-current)
#'     -[historical](https://catalogue.data.gov.bc.ca/dataset/fire-perimeters-historical) polygons.
#'     -[severity](https://catalogue.data.gov.bc.ca/dataset/c58a54e5-76b7-4921-94a7-b5998484e697).
#' - Accessing the [consolidated cutblock layer](https://catalogue.data.gov.bc.ca/dataset/harvested-areas-of-bc-consolidated-cutblocks-) using the bcdata package. The FTen layer is also used to identify very recent cutblocks.
#'
#' Note the detailed
#' Tenure (private ownership) 	WHSE_CADASTRE.PMBC_PARCEL_FABRIC_POLY_FA_SVW	BCGW
#'
#' Detailed roads layers. In some cases regionally specific roads are available through other collaborative projects (cumulative effects). These are not directly available through the BCGW.

#' @param in_aoi   `sf` polygon
#' @param out_path character string that points the output location
#'
#' @export
#' @import sf
#' @import bcdata
#' @examples
#' create_base_vectors <- function(in_aoi = your_polygon, out_path = "00_raw_inputs/vector")
#'
create_base_vectors <- function(in_aoi, out_path = "00_raw_inputs/vector"){


  # # testing
  # #in_aoi <- st_read(file.path('temp', "aoi.gpkg"))
  # #out_path = shape_raw_dir <- file.path('temp')
  #
  #
  if(missing(in_aoi)) stop("'aoi' is missing with no default")

  # Second, detect object type and convert where necessary
  if(!inherits(in_aoi, c("sf", "sfc")))
    stop("'aoi' is not an sf or sfc object.")

  if(is.null(out_path)) {
    stop("\rout_path is an invalid file path string")
  }

  # Detect the CRS of the sf object
  if(is.na(sf::st_crs(in_aoi)))
    stop("CRS is not assigned. Use sf::st_crs() to assign a valid CRS to in_aoi")

  if(sf::st_is_longlat(in_aoi)) {
    cat("Input CRS is Lat/Long format. Transforming to EPSG 3005 (BC Albers) for processing\n")
    epsg <- 3005L
    in_crs <- sf::st_crs(in_aoi)
    in_aoi <- sf::st_transform(in_aoi, epsg) %>% sf::st_set_agr("constant")
  } else {
    in_crs <- sf::st_crs(in_aoi)
    epsg <- in_crs$epsg
    in_aoi <- sf::st_set_agr(in_aoi, "constant")
    if(!is.numeric(epsg))
      stop("There was a problem retrieving the EPSG code from the in_aoi. Is it assigned properly?")
  }

  # Get largest bbox for downloading layers using the pemgeneratr function to match the raster inputs
  #box <- sf::st_bbox(in_aoi)
  epsg <- st_crs(in_aoi)

  # Adjust max download size based on AOI
  ## PROBLEMATIC -- should not be done globally -----------------
  options(bcdata.max_geom_pred_size = as.numeric(st_area(in_aoi)) + 10)
  #
  #
  get_BEC(in_aoi, out_path) ##
}





## get_BEC ----------------------------
get_BEC <- function(in_aoi, out_path) {

  # # 1) BEC Biogeographical linework
  bec <- bcdc_query_geodata("f358a53b-ffde-4830-a325-a5a03ff672c3") %>%
    bcdata::filter(INTERSECTS(in_aoi)) %>%
    collect() %>%
    {if(nrow(.) > 0) st_intersection(., in_aoi) else .}

  st_write(bec, file.path(out_path, "bec.gpkg"), delete_dsn = TRUE,
           delete_layer = TRUE)


}

  # # 2) Download VRI
  # vri <- bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") %>%
  #   bcdata::filter(INTERSECTS(in_aoi)) %>%
  #   bcdata::select(c("BCLCS_LEVEL_2","BCLCS_LEVEL_4","PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1")) %>% # Treed sites
  #   collect() %>%
  #   {if(nrow(.) > 0) st_intersection(., in_aoi) else .}
  #
  # st_write(vri, file.path(out_path, "vri.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #
  #
  # # post process VRI data into classes:
  # # Depending on the study area we want to focus on sampling in older areas - class 4 (60-80) or 5 (80 + )
  # # To do this we will define 2 vri age classess to exclude from the sampling area
  # # class 1 and 2 (0 - 40 yrs)
  # # class 1-3 (0 - 60 years)
  #
  # #  vri <- st_read(file.path(out_path, "vri.gpkg")) %>%
  # #    dplyr::select(c("BCLCS_LEVEL_2","BCLCS_LEVEL_4","PROJ_AGE_CLASS_CD_1", "SPECIES_CD_1"))
  #
  #     vri_class2 <- vri %>%
  #       dplyr::mutate(age_class = as.numeric(PROJ_AGE_CLASS_CD_1)) %>%
  #       dplyr::filter(age_class < 3)
  #     st_write(vri_class2, file.path(out_path, "vri_class1_2.gpkg"))
  #
  #     vri_class3 <- vri %>%
  #       dplyr::mutate(age_class = as.numeric(PROJ_AGE_CLASS_CD_1)) %>%
  #       dplyr::filter(age_class == 3)
  #     st_write(vri_class3, file.path(out_path, "vri_class3.gpkg"))
  #
  #
  # ######### this section is currently in test phase (related to removal fo deciduous areas in Date Creek after these were found to be highly selected and challanging to sample)
  #
  # # vri - deciduous leading - this needs to be appled for some
  #
  # # STILL TO DO - for areas with deciduous leading (AT, EP) Aspen and paper burch these should be seperated using the code "SPECIES_CD_1" == AT|EP.
  # # ie important in Date Creek
  #
  # vri_decid <- vri %>%
  #   dplyr::filter(SPECIES_CD_1 %in% c("AT","EP")) # note might need to adjust for some areas of interest
  #
  # st_write(vri_decid, file.path(out_path, "vri_decid.gpkg"))
  #
  # ######### end of test section
  #
  # # 3) Download recent cutblocks (within last 20 years)
  # message("\rDownloading cutblock layers")
  #
  # # Uses date filter which filters cutblock ages less than 20 years, or 7305 days
  # cutblocks <- bcdc_query_geodata("b1b647a6-f271-42e0-9cd0-89ec24bce9f7") %>%
  #   bcdata::filter(INTERSECTS(in_aoi)) %>%
  #   collect()
  #
  # cutblocks <- cutblocks %>%
  #   {if(nrow(.) > 0) st_intersection(., in_aoi) else .} %>%
  #   dplyr::filter(as.numeric(format(Sys.time(), "%Y")) - HARVEST_YEAR <= 20)
  #
  # st_write(cutblocks, file.path(out_path, "cutblocks.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #
  # # 4) ften  - Download latest harvest layer
  #
  # ften <- bcdc_query_geodata("cff7b8f7-6897-444f-8c53-4bb93c7e9f8b") %>%
  #   bcdata::filter(INTERSECTS(in_aoi)) %>%
  #   bcdata::select("HARVEST_AUTH_STATUS_CODE", "ISSUE_DATE", "CURRENT_EXPIRY_DATE_CALC", "LIFE_CYCLE_STATUS_CODE", "FILE_STATUS_CODE") %>%# Treed sites
  #   collect()
  #
  #
  # ften <- ften %>%
  #   filter(ISSUE_DATE >2000) # might need to adjust this to dynamic
  #
  # ften <- ften %>%
  #   dplyr::select("HARVEST_AUTH_STATUS_CODE", "ISSUE_DATE", "CURRENT_EXPIRY_DATE_CALC",
  #                 "LIFE_CYCLE_STATUS_CODE", "FILE_STATUS_CODE","FEATURE_AREA")
  #
  # st_write(ften, file.path(out_path, "ften.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #
  # cutblocks_ften <- bind_rows(cutblocks, ften)
  # cutblocks_ften <- bind_rows( ften, cutblocks)
  # st_write(cutblocks_ften, file.path(out_path, "cutblocks_ften.gpkg"),delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #
  #
  ### WORKS TO HERE ----------------------------------------------
  ### BREAKS BELOW ... FIX NEEDED --------------------------------
  # 5) TEM
  # message("\rDownloading TEM layers")
  # tem <- bcdc_query_geodata("0a83163b-a62f-4ce6-a9a1-21c228b0c0a3") %>%
  #     bcdata::filter(INTERSECTS(in_aoi)) %>%
  #     collect() %>%
  #    {if(nrow(.) > 0) st_intersection(., in_aoi) else .}
  #
  #
  # st_write(tem, file.path(out_path, "tem.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #

  # # 6) Water (Lakes, Rivers, Wetlands)
  #
  # # Use foreach in parallel to efficiently download multiple water layers
  # water_records <- c("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", # lakes
  #                    "f7dac054-efbf-402f-ab62-6fc4b32a619e", # rivers
  #                    "93b413d8-1840-4770-9629-641d74bd1cc6") # wetlands
  #
  # # cl <- parallel::makeCluster(parallel::detectCores())
  # # doParallel::registerDoParallel(cl)
  #
  #
  # for (i in waterbodies) {
  # # waterbodies <- foreach(i = water_records, .combine = rbind,
  # #                        .packages = c("tidyverse", "bcdata", "sf")) %dopar%
  #   # {
  #   waterbodies <- bcdc_query_geodata(i, crs = epsg) %>% # lakes
  #       bcdata::filter(INTERSECTS(in_aoi)) %>%
  #       collect() %>%
  #       {if(nrow(.) > 0) st_intersection(., in_aoi) else NULL}
  #   # }
  # # parallel::stopCluster(cl)
  #
  # # filter a subset of columns
  #
  # waterbodies_sf <- waterbodies %>%
  #   dplyr::select(id, WATERBODY_TYPE, AREA_HA)
  #
  #
  # ## THIS LOOKS LIKE IT WILL OVER-WRITE the previous look ... is filename unique???????? --------------
  # st_write(waterbodies_sf, file.path(out_path, "water.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  # }
  # 7) Download road network
  # The main road network layer has too many roads in it. Filter it down to only
  # include named roads and combine those with actual mapped FSR's

  # message("\rDownloading Road network")
  # roads <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
  #   bcdata::filter(
  #     BBOX(st_bbox(in_aoi))) %>%#,
  #   #ROAD_NAME_ID > 0) %>%
  #   collect() %>%
  #   dplyr::select(id, ROAD_NAME_FULL, FEATURE_LENGTH_M) %>%
  #   dplyr::rename(NAME = ROAD_NAME_FULL) %>%
  #   {if(nrow(.) > 0) {
  #     st_intersection(., in_aoi) %>%
  #       st_cast("MULTILINESTRING")
  #   } else .}
  #
  # fsr <- bcdc_query_geodata("9e5bfa62-2339-445e-bf67-81657180c682") %>%
  #   bcdata::filter(
  #     BBOX(st_bbox(in_aoi)),
  #     LIFE_CYCLE_STATUS_CODE == "ACTIVE") %>%
  #   collect() %>%
  #   dplyr::select(id, MAP_LABEL, FEATURE_LENGTH_M) %>%
  #   dplyr::rename(NAME = MAP_LABEL) %>%
  #   {if(nrow(.) > 0) {
  #     st_intersection(., in_aoi) %>%
  #       st_cast("MULTILINESTRING")
  #   } else .}
  #
  # road_merge <- rbind(roads, fsr)
  #
  # st_write(road_merge, file.path(out_path, "road_network.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #

  # 8 Major Towns

  # towns <- bcdc_query_geodata("b678c432-c5c1-4341-88db-0d6befa0c7f8") %>%
  #   collect()
  #
  # st_write(towns, file.path(out_path, "major_towns_bc.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #
  #
  # # 9 fire polygons
  # fire_records <- c("cdfc2d7b-c046-4bf0-90ac-4897232619e1",
  #                   "22c7cb44-1463-48f7-8e47-88857f207702")
  #
  # cl <- parallel::makeCluster(parallel::detectCores())
  # # doParallel::registerDoParallel(cl)
  #
  # fires <- foreach(i = fire_records, .combine = rbind,
  #                  .packages = c("tidyverse", "bcdata", "sf")) %dopar%
  #   {
  #     bcdc_query_geodata(i) %>%
  #       bcdata::filter(INTERSECTS(in_aoi)) %>%
  #       collect() %>%
  #       {if(nrow(.) > 0) st_intersection(., in_aoi) %>%
  #           dplyr::select(id, FIRE_NUMBER, VERSION_NUMBER, FIRE_YEAR,
  #                         FIRE_SIZE_HECTARES, LOAD_DATE) %>%
  #           dplyr::filter(as.numeric(format(Sys.time(), "%Y")) - FIRE_YEAR <= 20)}
  #   }
  #
  # # parallel::stopCluster(cl)
  #
  # st_write(fires, file.path(out_path, "fire.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #
  # # 10. fire severity
  # fire_int <- bcdc_query_geodata("c58a54e5-76b7-4921-94a7-b5998484e697") %>%
  #   bcdata::filter(INTERSECTS(in_aoi)) %>%
  #   bcdata::select(c("FIRE_YEAR", "BURN_SEVERITY_RATING")) %>% # Treed sites
  #   collect()
  #
  # st_write(fire_int, file.path(out_path, "fire_int.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #
  # # 11. BC parks
  #
  # parks <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7") %>%
  #   bcdata::filter(INTERSECTS(in_aoi)) %>%
  #   collect() %>%
  #   {if(nrow(.) > 0) st_intersection(., in_aoi) else .}
  #
  # st_write(parks, file.path(out_path, "parks.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #
  # # 12. National parks (if an option)
  # national_parks <- bcdc_query_geodata("88e61a14-19a0-46ab-bdae-f68401d3d0fb") %>%
  #   bcdata::filter(INTERSECTS(in_aoi)) %>%
  #   collect()%>%
  #   {if(nrow(.) > 0) st_intersection(., in_aoi) else .}
  #
  # st_write(national_parks, file.path(out_path, "natparks.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #
  # # 13. transmission lines
  # #bcdc_search("transmission")
  # trans_line <-  bcdc_query_geodata("384d551b-dee1-4df8-8148-b3fcf865096a") %>%
  #   bcdata::filter(INTERSECTS(in_aoi)) %>%
  #   collect()%>%
  #   {if(nrow(.) > 0) st_intersection(., in_aoi) else .}
  #
  # st_write(trans_line, file.path(out_path, "translines.gpkg"), delete_dsn = TRUE,
  #          delete_layer = TRUE)
  #
  #
  #




# ## Overview
#
# This script downloads the relevant spatial data for BEC zones, VRI, TEM, waterbodies and the road network used in stage 1 of PEM processing. Data is downloaded directly from the [BC Data Catalogue](https://catalogue.data.gov.bc.ca/dataset?download_audience=Public) using the [bcdata](https://github.com/bcgov/bcdata) package.
#
# ### Datasets to download
#
# * BEC - [Biogeoclimatic Ecosystems Classification](https://catalogue.data.gov.bc.ca/dataset/bec-map) are used to define and/or select specific "subzones" within a defined study area. Note this can be also pointed to different file where more updated version is available.
#
# * Vegetation Resource Inventory (VRI) - This layer includes a variety of vegetation measure, including cutblock age, TEM data and ..... Detailed data standards can be found [here](https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/data-management-and-access/vri-data-standards).
#
# * Freshwater Atlas -  The atlas is separated into the different types of waterbodies (lakes, rivers, wetlands, streams, man made, etc.), which requires a seperate download per type. Alternatively a single combined layer can be downloaded by is limited to linear data type. A parameter within the function can be set to "polygon" (the default option) or "linear".
#
# * Road network THIS STILL NEEEDS WORK
#
# * Road network - In previous works, the [raw road network](https://catalogue.data.gov.bc.ca/dataset/digital-road-atlas-dra-master-partially-attributed-roads) was found to be too detailed. The raw road network is filtered to only keep named roads, and then the up to date [Forest Service Road (FSR) layer](https://catalogue.data.gov.bc.ca/dataset/forest-tenure-road-segment-lines) is downloaded and merged with the filtered road network to produce a really good representation of where roads are actually located on the landscape.
#
# * Fire - Fire and fire intensity are used used to assist in identifying areas where a high cost is applied to reduce sampling. This includes:
# -[current](https://catalogue.data.gov.bc.ca/dataset/fire-perimeters-current)
# -[historical](https://catalogue.data.gov.bc.ca/dataset/fire-perimeters-historical) polygons.
# -[severity](https://catalogue.data.gov.bc.ca/dataset/c58a54e5-76b7-4921-94a7-b5998484e697).
#
# * Accessing the [consolidated cutblock layer](https://catalogue.data.gov.bc.ca/dataset/harvested-areas-of-bc-consolidated-cutblocks-) using the bcdata package. The FTen layer is also used to identify very recent cutblocks ()
#
# Note the detailed
# Tenure (private ownership) 	WHSE_CADASTRE.PMBC_PARCEL_FABRIC_POLY_FA_SVW	BCGW
# Detailed roads layers. In some cases regionally specific roads are available through other collaborative projects (cummulative effects). These are not firectly available through the BCGW.


# inputs: aoi_snapped (shapefile)
# output filenames:





# library(sf)
# library(bcdata)
# library(dplyr)
# library(foreach)


