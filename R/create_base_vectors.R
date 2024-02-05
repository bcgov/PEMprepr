#' Retrieves essential base vectors
#'
#' Collects vector data from the [BC Data Catalog](https://catalogue.data.gov.bc.ca) for a specified area of interest.
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

#' @param in_aoi   character string that points the location of the AOI. Default is the snapped AOI from previous steps, `file.path(fid$shape_dir_1010[2], "aoi_snapped.gpkg"`
#' @param out_path character string that points the output location
#'
#' @export
#' @import sf
#' @import bcdata
#' @import tidyverse
#' @examples
#'

test_create_base_vectors <- function(in_aoi = file.path(fid$shape_dir_1010[2], "aoi_snapped.gpkg"),
                                out_path = file.path(fid$shape_dir_0010[2])){

  ## Check that AOI is valid
  if(missing(in_aoi)) stop("'aoi' is missing with no default")

  ## Check out if output path is valid
  if(is.null(out_path)) stop("\rout_path is an invalid file path string")

  ## Read in AOI
  in_aoi <- st_read(in_aoi)

  ## Detect object type and convert where necessary
  if(!inherits(in_aoi, c("sf", "sfc"))) stop("'aoi' is not an sf or sfc object.")

  ## Detect the CRS of the sf object
  if(is.na(sf::st_crs(in_aoi)) | !is.numeric(sf::st_crs(in_aoi)$epsg)) stop("CRS is not assigned or not readable. Use sf::st_crs() to assign a valid CRS to in_aoi. Preferred crs is B.C. Albers (ESPG 2005)")

  ## Re-project AOI if needed
  if(sf::st_crs(in_aoi)$epsg != 3005L) {
    in_aoi %<>%
      sf::st_transform(x = ., crs = 3005L) %>%
      sf::st_set_agr("constant")
  }

  # Adjust max download size based on AOI
  ## PROBLEMATIC -- should not be done globally -----------------
  ## see: https://r-pkgs.org/code.html#sec-code-r-landscape section 7.6.1
  # withr::local_options(
  #   options(bcdata.max_geom_pred_size = as.numeric(st_area(in_aoi)) + 10)
  # )


  ## CALL all the subfunctions --------------
  test_get_BEC(in_aoi, out_path)     ## works
  test_get_VRI(in_aoi, out_path)     ## works
  test_get_harvest(in_aoi, out_path) ## works
  # test_get_TEM(in_aoi, out_path)     ## works
  # test_get_water(in_aoi, out_path)   ## works
  # test_get_roads(in_aoi, out_path)   ## works
  # test_get_towns(in_aoi, out_path)   ## works
  # test_get_fires(in_aoi, out_path)   ## works, but test on area with recent fire
  # test_get_fire_severity(in_aoi, out_path)      ## works
  # test_get_parks(in_aoi, out_path)              ## works
  # test_get_transmission_lines(in_aoi, out_path) ## works

  return(print(paste("Layers have been downloaded and saved to", out_path)))

  }


## 1) test_get_BEC ----------------------------
### 1) BEC Zones, Subzones, and Variants
test_get_BEC <- function(in_aoi, out_path) {

  message("\rDownloading BEC layers")

  ## URL for warehouse download
  url = "https://catalogue.data.gov.bc.ca/dataset/f358a53b-ffde-4830-a325-a5a03ff672c3"

  ## Name relevant columns to extract
  rel_cols = c("MAP_LABEL") ## Code for the subzone/variant/phases

  ## Name output geopackage
  out_name = "bec.gpkg"

  ## Pull the data from the warehouse
  bcdata::bcdc_query_geodata(record = url, crs = sf::st_crs(in_aoi)$epsg) %>% ## Query dataset
    bcdata::filter(INTERSECTS(in_aoi)) %>% ## Pull all polygons which intersect with the provided AOI
    bcdata::collect() %>% ## Download specified dataset
    dplyr::select(all_of(rel_cols)) %>% ## Select relevant cols
    {if(nrow(.) > 0) st_intersection(., in_aoi) else .} %>% ## Crop to AOI extent
    st_write(file.path(out_path, out_name), append = FALSE) ## Write to output file path

}

## 2) Download VRI -----------------------
### 2)
test_get_VRI <- function(in_aoi, out_path) {

  message("\rDownloading VRI layers")

  ## URL for warehouse download
  url = "https://catalogue.data.gov.bc.ca/dataset/2ebb35d8-c82f-4a17-9c96-612ac3532d55"

  ## Name relevant columns to extract
  rel_cols = c("BCLCS_LEVEL_2", ## Treed/non treed code
               "BCLCS_LEVEL_4", ## Treed/non treed code
               "PROJ_AGE_CLASS_CD_1", ## Age class for dominant species (1-9)
               "SPECIES_CD_1") ## Species code for dominant species

  ## Name output geopackage
  out_name = "vri.gpkg"

  ## Pull the data from the warehouse
  bcdata::bcdc_query_geodata(record = url, crs = sf::st_crs(in_aoi)$epsg) %>% ## Query dataset
    bcdata::filter(INTERSECTS(in_aoi)) %>% ## Pull all polygons which intersect with the provided AOI
    bcdata::collect() %>% ## Download specified dataset
    dplyr::select(all_of(rel_cols)) %>% ## Select relevant cols
    {if(nrow(.) > 0) st_intersection(., in_aoi) else .} %>% ## Crop to AOI extent
    st_write(file.path(out_path, out_name), append = FALSE) ## Write to output file path

  # SPECIAL FUNCTIONS: post process VRI data into classes:
  # Depending on the study area we want to focus on sampling in older areas - class 4 (60-80) or 5 (80 + )
  # To do this we will define 2 vri age classess to exclude from the sampling area
  # class 1 and 2 (0 - 40 yrs)
  # class 1-3 (0 - 60 years)

  vri <- st_read(file.path(out_path, out_name))

  vri %>%
    dplyr::mutate(age_class = as.numeric(PROJ_AGE_CLASS_CD_1)) %>%
    dplyr::filter(age_class < 3) %>%
    st_write(file.path(out_path, "vri_class1_2.gpkg"), append = FALSE)

  vri %>%
    dplyr::mutate(age_class = as.numeric(PROJ_AGE_CLASS_CD_1)) %>%
    dplyr::filter(age_class == 3) %>%
    st_write(file.path(out_path, "vri_class3.gpkg"), append = FALSE)


  #
  #
  # ######### this section is currently in test phase (related to removal fo deciduous areas in Date Creek after these were found to be highly selected and challanging to sample)
  #
  # # vri - deciduous leading - this needs to be appled for some
  #
  # # STILL TO DO - for areas with deciduous leading (AT, EP) Aspen and paper burch these should be seperated using the code "SPECIES_CD_1" == AT|EP.
  # # ie important in Date Creek

  decid_codes = c("AC", "AT", "AX", "DG", "EA", "EP", "MB") ### https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/tree-seed/tree-seed-centre/seed-testing/codes

  vri %>%
    dplyr::filter(SPECIES_CD_1 %in% decid_codes) %>% # note might need to adjust for some areas of interest
    st_write(file.path(out_path, "vri_decid.gpkg"), append = FALSE)

}


## 3) Get harvest history and FTEN --------------------------------
test_get_harvest <- function(in_aoi, out_path) {

  # 3) Download recent cutblocks (within last 20 years)
  message("\rDownloading cutblock layers")

  ## URL for warehouse download
  url = "https://catalogue.data.gov.bc.ca/dataset/b1b647a6-f271-42e0-9cd0-89ec24bce9f7"

  ## Name relevant columns to extract
  rel_cols = c("HARVEST_YEAR")

  ## Name output geopackage
  out_name = "cutblocks.gpkg"

  # Uses date filter which filters cutblock ages less than 20 years, or 7305 days

  # Uses date filter which filters cutblock ages less than 20 years, or 7305 days
  bcdata::bcdc_query_geodata(record = url, crs = sf::st_crs(in_aoi)$epsg) %>% ## Query dataset
    bcdata::filter(INTERSECTS(in_aoi) & HARVEST_YEAR >= 2000) %>% ## Pull all polygons which intersect with the provided AOI + special filter
    bcdata::collect() %>% ## Download specified dataset
    dplyr::select(all_of(rel_cols)) %>% ## Select relevant cols
    {if(nrow(.) > 0) st_intersection(., in_aoi) else .} %>% ## Crop to AOI extent
    st_write(file.path(out_path, out_name), append = FALSE) ## Write to output file path

  # 4) ften  - Download latest harvest layer

  ## URL for warehouse download
  url = "https://catalogue.data.gov.bc.ca/dataset/cff7b8f7-6897-444f-8c53-4bb93c7e9f8b"

  ## Name relevant columns to extract
  rel_cols = c("HARVEST_AUTH_STATUS_CODE",
               "ISSUE_DATE",
               "CURRENT_EXPIRY_DATE_CALC",
               "LIFE_CYCLE_STATUS_CODE",
               "FILE_STATUS_CODE",
               "FEATURE_AREA")

  ## Name output geopackage
  out_name = "ften.gpkg"

  bcdata::bcdc_query_geodata(record = url, crs = sf::st_crs(in_aoi)$epsg) %>% ## Query dataset
    bcdata::filter(INTERSECTS(in_aoi) & ISSUE_DATE > as.Date("2000-01-01")) %>% ## Pull all polygons which intersect with the provided AOI + special filter
    bcdata::collect() %>% ## Download specified dataset
    dplyr::select(all_of(rel_cols)) %>% ## Select relevant cols
    {if(nrow(.) > 0) st_intersection(., in_aoi) else .} %>% ## Crop to AOI extent
    st_write(file.path(out_path, out_name), append = FALSE) ## Write to output file path


dplyr::bind_rows(st_read(file.path(out_path, "cutblocks.gpkg")), st_read(file.path(out_path, "ften.gpkg"))) %>%
  st_write(file.path(out_path, "cutblocks_ften.gpkg"), append = FALSE)

}



## 5) TEM -----------------------------
test_get_TEM <- function(in_aoi, out_path) {

  message("\rDownloading TEM layers")

  ## URL for warehouse download
  url = "https://catalogue.data.gov.bc.ca/dataset/0a83163b-a62f-4ce6-a9a1-21c228b0c0a3"

  bcdata::bcdc_query_geodata(url) %>%
    bcdata::filter(INTERSECTS(in_aoi)) %>%
    bcdata::collect() %>%
    {if(nrow(.) > 0) st_intersection(., in_aoi) else .} %>%
    st_write(file.path(out_path, "tem.gpkg"), append = FALSE)

}

## 6) Water (Lakes, Rivers, Wetlands) --------------------------------------
test_get_water <- function(in_aoi, out_path) {

  message("\rDownloading lake, river, and wetland layers")
  # Use foreach in parallel to efficiently download multiple water layers
  water_records <- c("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", # lakes
                     "f7dac054-efbf-402f-ab62-6fc4b32a619e", # rivers
                     "93b413d8-1840-4770-9629-641d74bd1cc6") # wetlands

  for (i in 1:length(water_records)) {

    waterbodies <- bcdata::bcdc_query_geodata(water_records[i]) %>%
      bcdata::filter(INTERSECTS(in_aoi)) %>%
      bcdata::collect()

    if(nrow(waterbodies) > 0) {
        waterbodies <- sf::st_intersection(waterbodies, in_aoi)

        waterbodies_sf <- waterbodies %>%
          dplyr::select(id, WATERBODY_TYPE, AREA_HA)

        if (i == 1) {
          all_water <- waterbodies_sf } else {
            all_water <- rbind(all_water, waterbodies_sf)
          }
    }
  }
  st_write(all_water, file.path(out_path, "water.gpkg"), append = FALSE)
}


# 7) Download road network --------------------
test_get_roads <- function(in_aoi, out_path) {
  # The main road network layer has too many roads in it. Filter it down to only
  # include named roads and combine those with actual mapped FSR's

  message("\rDownloading Road network")
  roads <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
    bcdata::filter(BBOX(local(st_bbox(in_aoi)))) %>% # slightly larger extent
    bcdata::select(id, ROAD_NAME_FULL, ROAD_CLASS, ROAD_SURFACE, FEATURE_LENGTH_M) %>%
    collect() %>%
    dplyr::select(id, ROAD_NAME_FULL,ROAD_SURFACE, ROAD_CLASS,FEATURE_LENGTH_M) %>%
       {if(nrow(.) > 0) {
      st_intersection(., in_aoi) %>%
       st_cast("MULTILINESTRING")
    } else .}

  fsr <- bcdc_query_geodata("9e5bfa62-2339-445e-bf67-81657180c682") %>%
    bcdata::filter(
      BBOX(local(st_bbox(in_aoi)))) %>%
    collect() %>%
    dplyr::select(id, FILE_TYPE_DESCRIPTION, FEATURE_LENGTH_M) %>%
    dplyr::rename(ROAD_CLASS = FILE_TYPE_DESCRIPTION) %>%
    dplyr::mutate(ROAD_CLASS = dplyr::case_when(
      ROAD_CLASS == "Forest Service Road" ~ "resource",
      ROAD_CLASS == "Road Permit" ~ "unclassifed")) %>%
    dplyr::mutate(ROAD_SURFACE = dplyr::case_when(
    ROAD_CLASS == "resource" ~ "loose",
    ROAD_CLASS == "unclassifed" ~ "rough")) %>%
    {if(nrow(.) > 0) {
      st_intersection(., in_aoi) %>%
        st_cast("MULTILINESTRING")
    } else .}

  road_merge <- dplyr::bind_rows(roads, fsr)

  st_write(road_merge, file.path(out_path, "road_network.gpkg"), append = FALSE)
}



# 8 Major Towns ---------------------------------
test_get_towns <- function(in_aoi, out_path) {
  message("\rDownloading major towns")

  towns <- bcdc_query_geodata("b678c432-c5c1-4341-88db-0d6befa0c7f8") %>%
    bcdata::collect()

  st_write(towns, file.path(out_path, "major_towns_bc.gpkg"), append = FALSE)
}


# 9) Fire polygons  ---------------------------------
test_get_fires <- function(in_aoi, out_path) {
  message("\rDownloading recent fire disturbance (<20 years)")

  fire_records <- c("cdfc2d7b-c046-4bf0-90ac-4897232619e1",
                    "22c7cb44-1463-48f7-8e47-88857f207702")

  fires_all <- NA ## placeholder

  for (i in 1:length(fire_records)) {
      fires <- bcdata::bcdc_query_geodata(fire_records[i]) %>%
        bcdata::filter(INTERSECTS(in_aoi)) %>%
        collect() %>%
        {if(nrow(.) > 0) st_intersection(., in_aoi) else .}

      # filter for recent fires
      if(nrow(fires) > 0) {
        fires <- sf::st_intersection(fires, in_aoi) %>%
          dplyr::select(id, FIRE_NUMBER, VERSION_NUMBER, FIRE_YEAR,
                      FIRE_SIZE_HECTARES, LOAD_DATE) %>%
          dplyr::filter(as.numeric(format(Sys.time(), "%Y")) - FIRE_YEAR <= 20) }


      if(nrow(fires) > 0) {
        ## bind results of loops
        if (i == 1) {
            fires_all <- fires } else { ## i > 1
              if(all(is.na(fires_all))) {fires_all <- fires } else {fires_all <- rbind(fires_all, fires)}
              }
        } #else {print("No fires in layer queried") }

     # rm(fires)
    } ## end loop

  if (all(is.na(fires_all)) || nrow(fires_all) == 0) {
    print("No recent fire disturbance in area of interest") } else {
    sf::st_write(fires_all, file.path(out_path, "fire.gpkg"), append = FALSE)
  }
}

# 10. fire severity -------------------------------------
test_get_fire_severity <- function(in_aoi, out_path) {
  message("\rDownloading burn severity layer")

  fire_int <- bcdc_query_geodata("c58a54e5-76b7-4921-94a7-b5998484e697") %>%
    bcdata::filter(INTERSECTS(in_aoi)) %>%
    bcdata::select(c("FIRE_YEAR", "BURN_SEVERITY_RATING")) %>% # Treed sites
    collect()

  if(nrow(fire_int) > 0) {
    st_write(fire_int, file.path(out_path, "fire_int.gpkg"), append = FALSE)
  } else {
    print("No burn severity in AOI")
  }
}


# 11) BC parks and National parks-----------------
test_get_parks <- function(in_aoi, out_path) {
  message("\rDownloading Parks")

  parks <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7") %>%
    bcdata::filter(INTERSECTS(in_aoi)) %>%
    collect() %>%
    {if(nrow(.) > 0) st_intersection(., in_aoi) else .}

  if (nrow(parks) > 0) {
    st_write(parks, file.path(out_path, "parks.gpkg"), append = FALSE)
  } else {
    print("no provincial parks in aoi")
  }

  # 12. National parks (if an option)
  national_parks <- bcdc_query_geodata("88e61a14-19a0-46ab-bdae-f68401d3d0fb") %>%
    bcdata::filter(INTERSECTS(in_aoi)) %>%
    collect()%>%
    {if(nrow(.) > 0) st_intersection(., in_aoi) else .}

  if (nrow(national_parks) > 0) {
    st_write(national_parks, file.path(out_path, "natparks.gpkg"), append = FALSE)
  } else { print("no national parks in aoi")}
}


# 13) transmission lines -------------------------------
test_get_transmission_lines <- function(in_aoi, out_path) {
  message("\rDownloading transmission lines")

  #bcdc_search("transmission")
  trans_line <-  bcdc_query_geodata("384d551b-dee1-4df8-8148-b3fcf865096a") %>%
    bcdata::filter(INTERSECTS(in_aoi)) %>%
    collect()%>%
    {if(nrow(.) > 0) st_intersection(., in_aoi) else .}

  if(nrow(trans_line) > 0) {
    st_write(trans_line, file.path(out_path, "translines.gpkg"), append = FALSE)
  } else {
    print("No transmission lines in area of interest")
  }
}



