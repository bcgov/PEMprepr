
# Create satellite detrivatives for aligned rasters

# devtools::install_github("16EAGLE/getSpatialData")
# library(getSpatialData)
# library(terra)
# library(sf)
# library(sp)
# library(mapview)
# library(tidyverse)
# library(RStoolbox)
# library(rasterVis)
# library(purrr)
# library(RColorBrewer)
# library(raster)


# testing

#in_aoi <- st_read(file.path('temp', "aoi.gpkg"))
#out_path = shape_raw_dir <- file.path('temp')


# input the aligned satelite raster  (use aligned raster)



create_satellite <- function(iraster, rtemplate, out_path = cov_dir, writeout = TRUE){

  iraster = raw satellite
  rtempate = template raster (of res)
  out_path =
  writeout = TRUE

  sat_dir = raw_satellite_dir
            file.path(AOI_dir, "0_raw_inputs", "satellite_layers")
  cov_dir = output folder (per size)
  cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")
  rtemplate<- raster(file.path(cov_dir, res,"template.tif"))





allst <- stack(all)

rtemp <- raster(file.path(cov_dir, res,"template.tif"))###example for use as a template in

rtemp.wgs <- projectRaster(rtemp,
                           crs = "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Point to saved sentinel data location.

sen.dir = file.path(raw_dir, #"\\S2B_MSIL1C_20180714T193859_N0206_R042_T09UXA_20180714T225733.SAFE\\GRANULE\\L1C_T09UXA_A007075_20180714T194841\\IMG_DATA")
"\\Sentinel-2\\S2A\\S2A_MSIL1C_20200731T194911_N0209_R085_T09UWB_20200731T231533.SAFE\\GRANULE\\L1C_T09UWB_A026680_20200731T195619\\IMG_DATA")



# Make a list of the files in a directory of interest
jp2 <- list.files(sen.dir, pattern = "\\.tif$", full.names = T)
jp2_10 <- stack(jp2[c(2,3,4,8)])
jp2_20 <- stack(jp2[c(5,6,7,11,12,13)])
jp2_60 <- stack(jp2[c(1,9,10)])
# # create an RGB to test
# aoi <- mapview::viewRGB(x = jp2_10,
#                         r = "T09UXA_20180714T193859_B04",
#                         g = "T09UXA_20180714T193859_B03",
#                         b = "T09UXA_20180714T193859_B02",
#                         maxpixels = 1e+05) # %>% mapedit::editMap()
#
# function to prepare sentinel layers to match template
senprep <- function(senstack, rtemp, crs.out = "+init=epsg:3005"){
  print("please be patient - this will take a moment")
  rtemp.crs <- projectRaster(rtemp,
                             crs = "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  print("cropping raster stack to raster template")
  senst <- crop(senstack, rtemp.crs)
  print("reprojecting raster stack")
  senst <- projectRaster(senst , crs = crs.out)
  print("downscaling as required")
  senst <- resample(senst, rtemp, method = "bilinear")
}
# down scale the stacks
sout <- senprep(jp2_10, rtemp)
sout20 <- senprep(jp2_20, rtemp)
sout60 <- senprep(jp2_60, rtemp)

# stack and rename
allsen <- stack(sout, sout20, sout60)

# fix the bank names
band_names <- tribble(
  ~band, ~names,
  "blue", "B02",
  "green","B03",
  "red", "B04",
  "nir", "B08",
  "rededge1", "B05",
  "rededge2", "B06",
  "rededge3", "B07",
  "swir1", "B11",
  "swir2", "B12",
  "vegred", "B8A",
  "ultablue", "B01",
  "nir2", "B09",
  "cirrus", "B10"
)


# write out individual bands
short_names <- as.data.frame(names(allsen)) %>%
  rename("names" = "names(allsen)") %>%
  #mutate(names = gsub("T09UXA_20180714T193859_","", names)) %>%
  mutate(names = gsub("t09uwb_20200731t194911_","", names)) %>%
  pull(., names)


names(allsen) <- tolower(short_names)

writeRaster(allsen, file.path(cov_dir, res, ".tif"), bylayer = TRUE, suffix = names(allsen))

# check layers

all <- list.files(file.path(cov_dir, res), full.names = T)
#all <- all[14:48]

allst <- stack(all)

# generate SI and write out
# stack and rename
allsen <- stack(sout, sout20, sout60)

long_names <- as.data.frame(names(allsen)) %>%
  rename("names" = "names(allsen)") %>%
  #mutate(names = gsub("T09UXA_20180714T193859_","", names)) %>%
  mutate(names = gsub("T09UWB_20200731T194911_","", names)) %>%
  left_join(band_names) %>%
  pull(., band)

# Assign new names
names(allsen) <- long_names

# assign names of bands to basic names

band_names <- gsub("T09UWB_20200731T194911_","", names(allsen))

names(allsen) <- band_names

# writ eout basic bands
#writeRaster(allsen, file.path(cov_dir, res, ".tif"), bylayer = TRUE, suffix = names(allsen))


##  Plot multi-band RGB raster stack, three different ways to check the outputs are correct

# plotRGB(x = allsen, r = "red", g = "green", b = "blue", maxpixels = 1e5, stretch = "lin")
# # mapview::viewRGB
# viewRGB(x = allsen, r = "nir", g = "red", b = "green", maxpixels = 1e5)
# # RStoolbox::ggRGB
# ggRGB(img = allsen, r = "swir1", g = "nir", b = "red", maxpixels = 1e5, stretch = "lin")
#

#
#
#
# # generate ndvi 1: Rouse1974 	(nir - red)/(nir + red
# ndvi <- spectralIndices(allsen, red = "red", nir = "nir", indices = "NDVI")
# ggR(ndvi)
#
# # generate normalised differential water index  : McFeeters1996	(green - nir)/(green + nir)
# ndwi <- spectralIndices(allsen, green = "green", nir = "nir", indices = "NDWI")
# ggR(ndwi)
#
# # nnormalised differential water index : Gao1996	nir, swir2	(nir - swir2)/(nir + swir2)
# ndwi2 <- spectralIndices(allsen, swir2 = "swir2", nir = "nir", indices = "NDWI2")
# ggR(ndwi2)


# Generate all possible  derivatives at once:

SI <- spectralIndices(allsen, red = "red", nir = "nir", swir2 = "swir2", green = "green")
names(SI) <- tolower(names(SI))


writeRaster(SI, file.path(cov_dir, res, "sen.tif"), bylayer = TRUE, suffix = names(SI), overwrite = TRUE)

```
























## Sentinel-2

Incorporating satellite imagery can be used to distinguish non-forest from forest features, as well water bodies etc. A single best image can be used, or a composite over a season to reduce individual interference. This is downloaded from Copernicus web site (https://scihub.copernicus.eu/dhus/#/home) and requires a free account to be created in order to use. First, we need to log in to Copernicus. If you don't have an account, you can create one here: https://scihub.copernicus.eu/dhus/

We provide details to extract a single sentinel image (part1), or a composite of images based on seasonal time-frame (part2) ie. downloading cloud free medians of bands as they span across a given time frame (ex: spring).

Below there is coding for 2 methods: 1) Using getspatialData and 2) Google earth engine.

## Method 1:

This method uses getSpatialData and requires more manual steps.

```{r load library, echo = FALSE}
ls <- c("stars", "raster", "tools", "xml2", "RStoolbox", "reticulate",
        "googledrive", "tidyverse", "stringi", "rstudioapi", "foreach")

#devtools::install_github("16EAGLE/getSpatialData")
library(getSpatialData)
library(raster)
library(sf)
library(sp)
library(mapview)
library(rgee)
library(velox)
library(tidyverse)
library(RStoolbox)
library(rasterVis)
library(purrr)
library(RColorBrewer)


invisible(suppressPackageStartupMessages(
  lapply(c("raster", "RStoolbox", "terra", "sf", "googledrive", "tidyverse",
           "rgee", "rstudioapi"), library, character.only = TRUE)))

# for Part 2:

# This script requires python to access Google Earth Engine. The code below will
# install the correct python libraries and initialize the Earth Engine

#ee_install() # Make sure to restart the R session before proceeding
#ee_Initialize(drive = TRUE)

#ee_check() # earthengine check

```


Method 2#

This method requires more set up but is reccomended. Uses Google earth Engine https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html

```{r}
# Define study area folder

AOI <- "Wetzinkwa"
AOI_dir <- file.path(".", paste0(AOI, "_AOI"))
AOI_epsg <- 3005
cov_dir <- file.path(AOI_dir, "1_map_inputs", "covariates")

# Define path for file processing (doesn't have to exist)
raw_dir <- file.path(AOI_dir, "0_raw_inputs", "satellite_layers")
template <- file.path(AOI_dir, "1_map_inputs", "covariates","25m")

# define locations for aoi and template

template_raster <- file.path(template, "template.tif")
#dem <- file.path(AOI_dir, "1_map_inputs", "covariates", "dem.tif")
aoi <- file.path(AOI_dir, "0_raw_inputs", "base_layers", "aoi_snapped.gpkg" )


#shp <- st_read(aoi, quiet = TRUE) %>%
#  st_bbox() %>%
#  st_as_sfc()

aoi<- st_read(aoi) #%>%
  st_transform(4326)

```


# Option 1: Get a single tile from Sentinel for NF and Forest processing

```{r}
AOI_sp <- as(aoi,'Spatial')
#getSpatialData::services_avail()
set_aoi(AOI_sp)

# Login to Copernicus Data Hub (https://scihub.copernicus.eu/dhus/)
login_CopHub(username = "gcperk")
set_archive("raw_dir")

# Use getSentinel_query to search for data (using the session AOI)
records <- getSentinel_records(time_range = c("2019-07-01",
                                            "2020-08-30"),
                             products = "Sentinel-2",
                             aoi = AOI_sp)


records <- records[which(records$cloudcov < 10),] #filter by Level
records <- records[which(records$level == "Level-1C"),] #filter by Level

records_all <- records

# Preview on Map
get_previews(records, dir_out = raw_dir)

# review previews and download the best option / options

# Ie for Date CreeK:
# DateCreek = "S2A_MSIL1C_20200731T194911_N0209_R085_T09UWB_20200731T231533"
#            "S2A_MSIL1C_20190806T194911_N0208_R085_T09UWB_20190806T231546"

# BCTS AREAs Skeena
"S2A_MSIL1C_20190929T193211_N0208_R142_T09UXA_20190929T225238"
"S2A_MSIL1C_20190902T193901_N0208_R042_T09UXA_20190902T230532"
S2B_MSIL1C_20200511T192909_N0209_R142_T09UXA_20200511T212511_preview

# BCTS Areas
records <- records_all[which(records_all$record_id == "S2A_MSIL1C_20190902T193901_N0208_R042_T09UXA_20190902T230532" ),]
# BCTS Areas
records <- records_all[which(records_all$record_id == "S2A_MSIL1C_20190929T193211_N0208_R142_T09UXA_20190929T225238" ),]

# BCTS Areas
records <- records_all[which(records_all$record_id == "S2B_MSIL1C_20200511T192909_N0209_R142_T09UXA_20200511T212511" ),]


check_availability(records)
records_order <- order_data(records)

check_availability(records_order)

records <- get_data(records_order)

datasets <- get_data(records = records_order,
                     dir_out = raw_dir )

# Date Creek
#records <- records[which(records$record_id == "S2A_MSIL1C_20200731T194911_N0209_R085_T09UWB_20200731T231533" ),]


# Download # some download error - UP TO HERE!
#datasets_order <- order_data(records = records)

datasets <- get_data(records = records,
                     dir_out = raw_dir )

#sentFile <- list.files(file.path(records, raw_dir), pattern = ".zip", full.names = TRUE)[1]
#fc_list <- st_layers(sentFile)

satellite_files <- file.path(raw_dir, "Sentinel-2/S2A/S2A_MSIL1C_20200731T194911_N0209_R085_T09UWB_20200731T231533.SAFE/GRANULE/L1C_T09UWB_A026680_20200731T195619/IMG_DATA")

list.files(satellite_files, full.names = TRUE)

```



```{r run functions}
# Initialize the google earth engine
ee <- import("ee")
ee$Initialize()

# Create a bounding box of the AOI. Needs to be transformed to EPSG 4326
AOI_bbox <- st_read(aoi, quiet = TRUE) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(dist = 250, joinStyle = "MITRE") %>%
  st_transform(4326)

# Provide a data frame of dates to collect data between. The longer the data
# frame, the longer it will take to process and download your dataset.
#seasons_df <- data.frame(
#  season = c("winter", "spring", "summer", "fall"),
#  start = c("2018-12-21", "2019-03-20", "2019-06-21", "2019-09-23"),
#  end = c("2019-03-19", "2019-06-20", "2019-09-22", "2019-12-20"),
#  stringsAsFactors = FALSE
#)

seasons_df <- data.frame(
  season = c( "summer19"),
  start = c("2019-06-21"),
  end = c("2019-09-22"),
  stringsAsFactors = FALSE
)

```



The second function (get_sentinel_ee) provides a similar purpose; however it uses Google Earth Engine to do the processing and you need to download a result rather than the raw data. You need to provide the function with an AOI (sf dataframe or sfc object), a template raster that is simply used to adjust the maximum allowable pixels to process, a table of date ranges, and a path to a download folder. This is the recommended function to use for processing satellite imagery because it can capture differences in seasons, for example.

```{r Satellite Download}
# This is a new function, allowing users to process Sentinel 2 bands in an area
# over the course of a given time frame. It harnesses the computational power of
# Google Earth Engine, making the computations much faster than it would be
# locally This is now the recommended function.
get_sentinel_ee <- function(
  aoi = NULL,
  template_raster = NULL,
  date_ranges = NULL,
  drive_folder = NULL,
  download_folder = NULL)
{

  # testing lines
  aoi = AOI_bbox
  template_raster = raster(file.path(template, "template.tif"))
  date_ranges = seasons_df
  drive_folder = "Sentinel2"
  download_folder = file.path(raw_dir, "1_download")


  # Perform checks of function inputs
  if(!any(class(aoi) %in% c("sf", "sfc", "sfg")) || is.null(aoi)) {
    stop("\rError: You must specify an AOI as an sf, sfc, or sfg object")

  } else if(any(class(aoi) %in% c("sf", "sfc"))) {
    if(st_crs(aoi) != st_crs(4326)) aoi <- st_transform(aoi, 4326)

  } else if(any(class(aoi) %in% "sfg")) {
    aoi <- st_geometry(aoi) %>% st_set_crs(4326)
  }
  ee_geom <- as.numeric(paste(t(st_coordinates(aoi)[, c("X", "Y")])))
  ee_geom <- ee$Geometry$Polygon(coords = ee_geom)

  if(is.null(template_raster) ||
     !class(template_raster) %in% c("RasterLayer", "character")) {
    stop(
    "\rError: You must define the template_raster variable as either a
    \r'RasterLayer' object, or a character string file path to a raster image"
    )

  } else if(class(template_raster) == "character") {
    template_raster <- raster(template_raster)
  }

  if(is.null(date_ranges) || !class(date_ranges) %in% "data.frame") {
    stop(
      "\rError: You must supply a data.frame with three columns:
      \r  1: An ID column;
      \r  2: A column of start dates; and
      \r  3: A column of end dates.
      \rDates must be supplied as a character vector with the format 'YYYY-MM-DD'"
    )
  }

  if(is.null(drive_folder)) {
    drive_folder <- "Sentinel2"
  }

  if(is.null(download_folder) || !class(download_folder) %in% "character") {
    stop(
      "\rYou must supply an output directory for the files to be downloaded to"
    )
  }

  # Create a cloud masking function which will run in GEE
  cloud_mask <- function(image) {
    qa <- image$select("QA60")
    cloudBitMask <- bitwShiftL(1, 10)
    cirrusBitMask <- bitwShiftL(1, 11)
    mask <- qa$bitwiseAnd(cloudBitMask)$eq(0)$
      And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
    image <- image$updateMask(mask)$divide(10000L)$
      select("B.*")$
      copyProperties(image, list("system:time_start"))
  }

  # Create the parent folder for the seasonal exports in your Google Drive folder
  try(drive_mkdir(drive_folder, path = "~", overwrite = FALSE), silent = TRUE)

  # Create a dataframe of Sentinel 2 band names and their associated resolutions
  bands <- data.frame(
    band = c("B1", "B2", "B3", "B4", "B5", "B6",
             "B7", "B8", "B9", "B11", "B12", "B8A"),
    new_name = c("b01", "b02", "b03", "b04", "b05", "b06",
                 "b07", "b08", "b09", "b11", "b12", "b8A"),
    res = c(60, 10, 10, 10, 20, 20, 20, 10, 60, 20, 20, 20),
    stringsAsFactors = FALSE)

  for(i in 1:nrow(date_ranges)) {
    i = 1
    date_id <- date_ranges[i, 1]
    message(paste0("\rStarting processing of ", date_id, " dates"))

    # Create the folder for the seasonal exports in your Google Drive folder
    drive_mkdir(date_id, path = "Sentinel2", overwrite = TRUE)

    # Create the folder for the seasonal downloads in your local folder
    out_dir <- file.path(download_folder, date_id)
    if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    # Perform filtering operations in GEE
   # sen <- ee("COPERNICUS/S2_SR")$
    sen <- ee$ImageCollection("COPERNICUS/S2_SR")$
      filterBounds(ee_geom)$
      filterDate(ee$Date(date_ranges[i, 2]), ee$Date(date_ranges[i, 3]))$
      #filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20))$
      map(cloud_mask)

    # Take the median and clip the bands
    composite <- sen$median()
    comp_clip <- composite$clip(ee_geom)
    # Loop through to reproject and save each band
    for(j in 1:nrow(bands)) {
      task_img <- ee$batch$Export$image$toDrive(
        image = comp_clip$select(list(bands$band[j]))$
          reproject("EPSG:3005", NULL, bands$res[j]),
        folder = drive_get(paste0("Sentinel2/", date_id))$name,
        maxPixels = ncell(raster(template_raster)) * 2,
        fileNamePrefix = bands$new_name[j]
      )

      # Save the bands to google drive and download them
      task_img$start()
      while(task_img$active()) {
        print(sprintf("Polling for task (id: %s).", task_img$id))
        Sys.sleep(5)
      }
      print(sprintf("State: %s", task_img$status()$state))
      img <- drive_download(
        file = file.path(drive_folder, date_id, paste0(bands$new_name[j], ".tif")),
        path = file.path(out_dir, paste0(bands$new_name[j], ".tif")),
        overwrite = TRUE)
    }
  }
  return(list.files(download_folder, pattern = ".tif$", full.names = TRUE,
                    recursive = TRUE))
}
```









```{r}

# Run get_sentinel_ee function to process and download satellite images
ee_download <- get_sentinel_ee(
  aoi = AOI_bbox,
  template_raster = dem,
  date_ranges = seasons_df,
  drive_folder = "Sentinel2",
  download_folder = file.path(raw_path, "1_download")
)

season_dirs <- dir(file.path(raw_path, "1_download"),
                   full.names = TRUE)[dir(file.path(raw_path, "1_download"))
                                      %in% seasons_df$season]

# Run the bands_to_indices function to produce satellite indices for each
# of the date ranges
for(i in season_dirs) {
  sat_indices <- bands_to_indices(
    reference_raster = dem,
    satellite_files = list.files(i, full.names = TRUE),
    single_scene = FALSE,
    output_path = output_path,
    remove_index = c("EVI2")
  )
}

```





Some issues may accure where by cutblocks are confused with non-forested surfaces, or where cloud or snow cover can obscure parts of the study area.



Once the data has been downloaded, the layers are clipped to a template raster (as selected), resampled to match the extent and resolution and renamed to enable spectral indicies to be calculated.

Methods were originally developed during the Hackathon in Prince George (https://github.com/bcgov-datasci-labs/BCForestNonForestPEM/blob/master/01_Load_Sentinel.R) and from conversations with Tom Hengl regarding satellite imagery.


Part 1)

Load required packages. For downloading a single scene we require the getSpatialData package while Rgee (R google earth engine) is used for composite layers. Rgee harnesses the google earth engine to process large amounts of data.


Define the study area and input output filepaths.


The getSpatialData package requires that an area of interest (AOI) be set prior to beginning a session to search and download records. Here, we are loading in the AOI shapefile and simply telling R what the proj4 code for the AOI is. The getSpatialData uses this for querying purposes.

Next, a date range is supplied in order to filter down the results from either copernicus or USGS. Enter dates in the YYYY-MM-DD format.





The following chunk builds a function that calls SAGA GIS to perform bulk transformations of the raw satellite files to the projection used in the PEM project. It is used in concert with other chunks below to build a larger toolchain which will be dependent on the files extracted.

A quick note regarding SAGA versions: SAGA 7.5.0 has a bug in the coordinate transformation algorithm which is producing a lot of spots on the resulting maps. 7.4.0 is recommended to be used instead; however, if this is fixed in the next version, the script should be updated to change the clipping mechanism: currently, the way this works is that all of the grids are transformed at once, and then after that they are individually clipped and saved; however, 7.5.0 introduced that the grids could be clipped all at once which is much more efficient. If the transformation issue is fixed in a future released, this functionality should be incorporated which would negate the use for the "Mask_Input" variable to be pasted in the toolchain in a loop.


This next chunk will perform a similar process using the Google Earth Engine (again, the recommeded process). First, you must "initialize" the earth engine environment. This will open your web browser to a couple of pages asking for permission to write to your google drive folders. In the end, you will be given a key to copy and paste into a popup window in this R session, and initializaiton should be complete. Note that in the end the files written to Google Drive should not be very large, likely less than 500 Mb total, so that would be a good amount of available space to have on hand. It would also be possible to script automatic deletion of files after they are downloaded as well.

Next, provide the AOI shape as either an sf or sfc object. Note: It must be in the 4326 projection for GEE to properly read it. It's also a good idea to buffer the AOI in order to capture the edge effects in processing.

You need to create a dataframe with three columns: The first is an ID column, which will be used for creating subfolders in your downloads folder later. The second column is a start date column, and the third is an end date column. The example given here separates the dates by season; however, you can use as many or as little as you wish.

Finally the function is ran. You should specify a drive_folder for where outputs will be stored to on Google Drive as well as a download folder for when files are downloaded locally. The function takes sentinel 2 images from a specified date range (ex: winter), performs a cloud masking function to remove clouds from an image stack, and then calculates the median across that date range. The final image is clipped to your AOI, reprojected in BC Albers, and then saved to your google drive folder. Lastly, the image is downloaded locally to your machine where you specify.


# Generate satellite derivatives

After running your preferred download (and extraction) function, the satellite band files are then reprojected and masked using the DEM's projection parameters and resolution. The DEM should be supplied as a file path (note: the function has changed now to perform only across a single DEM rather than a whole bunch. If you want to do multiple resolutions, you can do so in an iterative loop around this function).

The function bands_to_indices will take bands located in a given folder (satellite_files), reproject and clip them to a given raster (reference_raster), and outputs a raster stack of indices which can be optionally saved to the main covariates folder. The option "single_scene" is essentially asking which function for downloading data you ran above since it has an impact on the spectralIndices function. The option remove_index can be used if you don't want a particular index in your results (ex: EVI2 seems to generate large areas of no data).

Finally, these reprojected and masked files are used in the processing of vegetation and soil indices. Here, the RStoolbox package is being used to generate these indices all at once. This is not meant to be a comprehensive list of which indices should be used for the PEM project, however I think it provides a solid base for thinking of which indices to include. Some documentation:

https://bleutner.github.io/RStoolbox/rstbx-docu/spectralIndices.html
https://pro.arcgis.com/en/pro-app/help/data/imagery/indices-gallery.htm
https://www.usgs.gov/land-resources/nli/landsat/landsat-surface-reflectance-derived-spectral-indices

You might notice that not all of the indices from the ArcGIS and USGS websites are included in this package. As mentioned, the RStoolbox is not meant to represent the "be all and end all" of defining which indices to use. Some may not be appropriate to use here, however the package makes it really easy to generate those indices quite quickly.

```{r Convert raster bands to satellite indices}
# This function will take downloaded satellite images, transorm them, and output
# multiple satellite image indices as a raster stack.
bands_to_indices <- function(
  reference_raster = NULL,
  satellite_files = NULL,
  single_scene = FALSE,
  output_path = NULL,
  remove_index = NULL)
  {

  # test lines
  reference_raster = template_raster
  satellite_files = list.files(satellite_files, full.names = TRUE)
  single_scene = TRUE
  output_path = template
  remove_index = NULL # c("EVI2")




  # Perform input checks
  if(is.null(reference_raster) ||
     !class(reference_raster) %in% c("RasterLayer", "character")) {
    stop("\rError: You must specify a file path to a raster image")
  } else if(class(reference_raster) == "RasterLayer") {
    reference_raster <- reference_raster@file@name
  }

  if(is.null(satellite_files) || !is.character(satellite_files)) {
    stop(
      "\rError: You must supply a character vector of file paths to satellite
      \rraster images"
    )
  }

  process_path <- file.path(dirname(dirname(dirname(satellite_files[1]))), "2_reproject")

  if(!dir.exists(process_path)) dir.create(process_path, recursive = TRUE)
  ref <- raster(reference_raster)

  #############################################################################

  ## Pan sharpening testing: sentinel 2 doesn't have a pan sharpening band, but
  # it might be possible to use band 8 or the mean of bands 2, 3, 4, and 8:
  #https://www.mdpi.com/2072-4292/8/4/354/htm#B34-remotesensing-08-00354
  #https://www.researchgate.net/publication/323933204_Sentinel-2_Pan_Sharpening_-_Comparative_Analysis

  res_10 <- stack(satellite_files[sapply(satellite_files, function(x)
    res(raster(x))[1], simplify = TRUE) %>%
      grep(pattern = 10, value = FALSE)])
  res_20 <- stack(satellite_files[sapply(satellite_files, function(x)
    res(raster(x))[1], simplify = TRUE) %>%
      grep(pattern = 20, value = FALSE)])

  # Band 8 only as pan band:
  # pan <- raster(grep("b08", satellite_files, value = TRUE))

  # Mean of bands 2, 3, 4, and 8 as pan band:
  raster::beginCluster(parallel::detectCores())
  pan <- clusterR(res_10, mean, args = list(na.rm = TRUE))
  raster::endCluster()

  message("\rApplying pan sharpening to low resolution bands")

  satellite_stack <- stack(
    res_10, panSharpen(res_20, pan = pan, method = "pca", norm = FALSE)
  )
  names(satellite_stack) <- gsub("_pan$", "", names(satellite_stack))

  # Change band names to be more descriptive
  band_lookup <- tribble(
    ~band_no, ~band_name,
    "b01", "ultrablue", "b02", "blue", "b03", "green", "b04", "red",
    "b05", "redEdge1", "b06", "redEdge2", "b07", "redEdge3", "b08", "nir",
    "b09", "wv", "b11", "swir2", "b12", "swir3", "b8A", "narrow_nir"
  )

  for(i in 1:nlayers(satellite_stack)) {
    if(any(grepl(names(satellite_stack)[i], band_lookup$band_no))) {
      names(satellite_stack)[i] <- band_lookup$band_name[
        which(band_lookup$band_no %in% names(satellite_stack)[i])
        ]
    }
  }

  # Produce vegetation indices from band layers
  satellite_indices <- spectralIndices(
    img = satellite_stack,
    blue = "blue", green = "green", red = "red", nir = "nir",
    redEdge1 = "redEdge1", redEdge2 = "redEdge2", redEdge3 = "redEdge3",
    swir2 = "swir2", swir3 = "swir3",
    scaleFactor = ifelse(single_scene == TRUE, 10000, 1),
    coefs = list(swir2ccc = satellite_stack$swir2@data@min,
                 swir2coc = satellite_stack$swir2@data@max)
  )

  if(!is.null(remove_index)) {
    satellite_indices <- dropLayer(satellite_indices, remove_index)
  }
  message("\rMasking and writing ouputs")

  # Unfortunately this step takes a while. Could perhaps move this over to SAGA...
  satellite_indices <- projectRaster(satellite_indices, ref)
  for(i in 1:nlayers(satellite_indices)) {
    mask(
      subset(satellite_indices, i), ref, format = "GTiff",
      filename = file.path(
        output_path,
        tolower(paste0("sentinel2_",
                       names(satellite_indices)[i], "_",
                       basename(dirname(satellite_files[1]))))),
      overwrite = TRUE)

  }
  # Generare an RGB for background image
  RGB <- subset(satellite_stack, c("red", "green", "blue"))
  writeRaster(
    RGB,
    file.path(
      process_path,
      paste0("sentinel2_RGB_", basename(dirname(satellite_files[1])), ".tif")),
    overwrite = TRUE)

  return(satellite_indices)
}
```

Finally, we just need to run the function. I've included examples for processing this when a single scene is downloaded, and for when composite images from google earth engine have been downloaded

##Water from NDWI layer
According to multiple sources, values of > 0 for the MNDWI layer indicate the presence of a water body. This process can be easily scripted to detect the location of water across a landscape and can be used to compare to that of the water layer downloaded as shapes from the bcdata package

https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2018RG000598

https://www.researchgate.net/profile/Hanqiu_XU/publication/232724072_Modification_of_Normalized_Difference_Water_Index_NDWI_to_Enhance_Open_Water_Features_in_Remotely_Sensed_Imagery/links/5c9aee13299bf1116949a345/Modification-of-Normalized-Difference-Water-Index-NDWI-to-Enhance-Open-Water-Features-in-Remotely-Sensed-Imagery.pdf

```{r find water from MNDWI}
# Looking at all of the mndwi files produced, the best one to use is going to be
# the one from the summer time frame
mndwi_summer <- raster(file.path(output_path, "sentinel2_mndwi_summer.tif"))

# Reclassify raster values
water <- reclassify(mndwi_summer, c(-Inf, 0, NA, 0, Inf, 1))

# Raster to sf conversion via stars package
water_sf <- st_as_stars(water) %>%
  st_as_sf(as_points = FALSE, merge = TRUE, na.rm = TRUE, use_integer = TRUE) %>%
  st_geometry() %>%
  st_union()

st_write(
  water_sf,
  file.path(AOI_dir, "0_raw_inputs", "base_layers", map_res, "water_mndwi.gpkg"),
  delete_layer = TRUE)
```

































# Part 2: Generate sentinel average covariates

Next, set up a function that will create the Earth Engine tasks to process cloud free Sentinel-2 bands. This took a while to figure out! The function requires 3 inputs:

1) An AOI polygon or multipolygon. This is provided as an sf object, converted to a bounding box, and then the bounding box shape is used as the Earth Engine polygon area of interest
2) A data.frame of date ranges used to limit the Earth Engine processing. This data.frame has 3 columns: an ID column, a start date column, and an end date column. I'm still thinking of ways to make that better, but for now that's how it is
3) A dsn (data source name) for where to place the downloaded/processed data.

The Earth Engine function works in the following manner:
1) Filters the sentinel-2 images taken between the specified start and end dates
2) Removes clouds from each of the images for each of the bands (bands are identified as beginning with the letter "B")
3) Takes the median value at each pixel for the band "stack"

Additionally, the true color bands are downloaded using their raw values (0-255). After processing, Google Earth Engine requires that the bands are saved to a Google Drive folder (or Google Cloud Services (GCS) repository, but that costs money). The bands for the specified date ranges are downloaded locally and then processed to produce the satellite indices. Satellite indices are saved within the specified DSN. Only indices that have enough data are kept (usually this means tossing out the EVI2 file).

This function keeps all of the bands at their native resolutions throughout and results with each satellite index at a raw resolution of 10m^2.

```{r Satellite Download}
# Function requires that rgee is working
get_satellite_indices <- function(aoi, date_ranges, dsn = NULL) {

  # testing
  aoi = aoi
  date_ranges =

  # Evaluate aoi input
  if(!inherits(aoi, c("sf", "sfc")))
    stop("'aoi' must be of class 'sf' or 'sfc'")

  if(!st_geometry_type(aoi, by_geometry = FALSE) %in% c("POLYGON", "MULTIPOLYGON"))
    stop("'aoi' must be an sf or sfc POLYGON or MULTIPOLYGON")

  # Evaluate date_ranges input
  if(ncol(date_ranges) != 3)
    stop(
      "'date_ranges' should be a data.frame object with 3 character class columns.
    \rColumn 1 should be a character value indicating the subfolder for the specified date range.
    \rColumn 2 should be a character value indicating the starting point of the date range.
    \rColumn 3 should be a character value indicating the ending point of the date range.")

  if(!all(apply(date_ranges, 2, is.character)))
    stop(
      "'date_ranges' should be a data.frame object with 3 character class columns.
    \rColumn 1 should be a character value indicating the subfolder for the specified date range.
    \rColumn 2 should be a character value indicating the starting point of the date range.
    \rColumn 3 should be a character value indicating the ending point of the date range.")

  # Evaluate dsn input
  if(is.null(dsn)) dsn <- getwd()
  if(!is.character(dsn))
    stop("'dsn' requires a valid file path as a character input")

  # Create directories for saving and reprojecting
  dl_dir <- file.path(dsn, "1_download")
  ind_dir <- file.path(dsn, "2_indices")

  dir.create(dl_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(ind_dir, showWarnings = FALSE, recursive = TRUE)

  # Set geometry for use in GEE (should make checks for polygon/multipolygon type)
  # Should force conversion to sf bounding box!
  # Projection doesn't matter, it is set properly in the download function
  ee_geom <- st_geometry(aoi) %>%
    st_bbox() %>%
    st_as_sfc() %>%
    sf_as_ee()

  # Determine some important metadata RE sentinel 2 satellites
  sen <- ee$ImageCollection("COPERNICUS/S2_SR")
  bands <- grep("^B.*|^TCI.*", sen$first()$bandNames()$getInfo(), value = TRUE)
  scales <- sapply(bands, function(x)
    sen$first()$select(x)$projection()$nominalScale()$getInfo())

  # Create a cloud masking function which will run in GEE
  cloud_mask <- function(image) {
    qa <- image$select("QA60")
    cloudBitMask <- bitwShiftL(1, 10)
    cirrusBitMask <- bitwShiftL(1, 11)
    mask <- qa$bitwiseAnd(cloudBitMask)$eq(0)$
      And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
    image <- image$select("B.*")$updateMask(mask)$divide(10000L)
  }

  # Function selects the TCI images as well (easier viewing in GIS programs)
  add_tci <- function(image) {
    image$select("TCI.*")
  }

  # Perform filtering operations in GEE
  gee_run <- lapply(1:nrow(date_ranges), function(i) {
    dl_subdir <- file.path(dl_dir, date_ranges[i, 1])
    ind_subdir <- file.path(ind_dir, date_ranges[i, 1])

    dir.create(dl_subdir, showWarnings = FALSE)
    dir.create(ind_subdir, showWarnings = FALSE)

    dataset <- sen$
      filterDate(date_ranges[i, 2], date_ranges[i, 3])$
      filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20))

    rgb <- dataset$map(add_tci)$median()
    composite <- dataset$map(cloud_mask)$median()$addBands(rgb)

    message("Downloading Sentinel-2 Level-2A bands via GEE for dates between ",
            date_ranges[i, 2], " and ", date_ranges[i, 3])
    # Composite image is made up of each sentinel 2 band. Need to extract the
    # bands and run the function to save the files to the download directory
    gee_dl <- lapply(bands, function(x) {
      ee_as_raster(
        composite$select(x),
        region = ee_geom,
        dsn = file.path(dl_subdir, paste0(x, ".tif")),
        scale = scales[x],
        lazy = TRUE, quiet = TRUE)})

    rast_bands <- ee_utils_future_value(gee_dl) %>% setNames(lapply(., names))
    message("Downloading finished! Producing satellite indices")

    # At this point, should perform calculations for satellite indices. Extract
    # the 10 and 20m raster bands only
    tci <- raster::stack(rast_bands[startsWith(names(rast_bands), "TCI")])
    bands_10 <- names(scales)[!startsWith(names(scales), "TCI") & scales == 10]
    bands_20 <- names(scales)[!startsWith(names(scales), "TCI") & scales == 20]
    bands_60 <- names(scales)[!startsWith(names(scales), "TCI") & scales == 60]
    res_10 <- raster::stack(rast_bands[bands_10]) %>% setMinMax()
    res_20 <- raster::stack(rast_bands[bands_20]) %>% setMinMax()

    # Perform pan sharpening on the 20m bands
    pan <- panSharpen(res_20, mean(res_10, na.rm = TRUE), method = "pca", norm = FALSE)

    # Stack the 10m bands and pan sharpened bands that are now 10m
    satellite_stack <- raster::stack(res_10, pan) %>%
      setNames(gsub("_pan$", "", names(.)))

    # Create satellite indices in a single function
    satellite_indices <- spectralIndices(
      img = satellite_stack,
      blue = "B2", green = "B3", red = "B4", nir = "B8",
      redEdge1 = "B5", redEdge2 = "B6", redEdge3 = "B7",
      swir2 = "B11", swir3 = "B12",
      coefs = list(swir2ccc = max(0, minValue(satellite_stack$B11)),
                   swir2coc = min(1, maxValue(satellite_stack$B11))))

    # Workaround for proper name setting
    ind_names <- names(satellite_indices)
    tci_names <- names(tci)
    satellite_indices <- rast(satellite_indices) %>% setNames(ind_names)
    tci <- rast(tci) %>% setNames(tci_names)

    # Remove layers that have missing data
    layer_check <- data.frame(freq(satellite_indices, value = NA)) %>%
      dplyr::mutate(layer = names(satellite_indices[layer]),
                    count = ncell(satellite_indices) - count) %>%
      dplyr::filter(count > 0.95 * median(count))

    indices_flt <- subset(satellite_indices, layer_check$layer)
    f_names <- file.path(ind_subdir, paste0(names(indices_flt), ".tif"))

    # Write out files
    message("Cleaning Google Drive container and writing outputs")
    ee_clean_container(quiet = TRUE)
    tci <- writeRaster(tci, file.path(ind_subdir, "true_color.tif"), overwrite = TRUE)
    out <- writeRaster(indices_flt, f_names, overwrite = TRUE)
    raster::removeTmpFiles(h = 0)
    return(out)

  }) %>% setNames(date_ranges[, 1])

  return(gee_run)
}
```

Finally, we just need to run the function. The candidate AOI is buffered by 250m, the above function is run, and then each folder of satellite indices gets reprojected to match the DEM at each resolution. The result is masked to match the extent and shape of the DEM, and then each file is saved separately.

```{r Run functions}
# Create a buffered bounding box of the AOI
AOI_bbox <- st_buffer(shp, 250)
# Provide a data frame of dates to collect data between. The longer the data
# frame, the longer it will take to process and download your dataset.
seasons_df <- data.frame(
  season = c("winter", "spring", "summer", "fall", "2019"),
  start = c("2018-12-21", "2019-03-20", "2019-06-21", "2019-09-23", "2019-01-01"),
  end = c("2019-03-19", "2019-06-20", "2019-09-22", "2019-12-21", "2019-12-31"),
  stringsAsFactors = FALSE)
# Run get_satellite_indices function to download and process sentinel indices
sentinel_indices <- get_satellite_indices(
  aoi = AOI_bbox,
  date_ranges = seasons_df,
  dsn = raw_dir)
# The function above outputs a list format. Create a single SpatRaster of all
# of the satellite indices
sentinel_dirs <- list.dirs(file.path(raw_dir, "2_indices"), recursive = FALSE)
sentinel_indices <- rast(lapply(sentinel_dirs, function(x) {
  rast(grep("true_color.tif",
            list.files(x, pattern = ".tif$", full.names = TRUE),
            invert = TRUE, value = TRUE)) %>%
    setNames(tolower(paste0("sentinel2_", names(.), "_", basename(x))))}))
# Remove progress bars for below reprojection
def_ops <- terra:::spatOptions()$progress
terraOptions(progress = 0)


```







## OLD stuff to check


First, you need to provide it a table created by the getSentinel_query function (in the getSpatialData package). You will also provide row ID's that specify which records you wish to download from the sentinel_query_table. You can provide one ID if your AOI is encompassed by the scene, or multiple ID's if your AOI is encompassed by multiple scenes. You also need to provide a download folder and an extraction folder path. The function first checks your arguments to make sure that you've provided enough and the right type of data, and then proceeds with the function (downloads data from Copernicus, extracts the files from the zip files to a destination folder)


Finally the function is ran. If multiple scenes are downloaded because there was no single scene that encompassed an AOI, they are automatically mosaicked together.

Finally, we can plot the RGB layers to make sure that the outputs are correct


The following chunk builds a function that calls SAGA GIS to perform bulk transformations of the raw satellite files to the projection used in the PEM project. It is used in concert with other chunks below to build a larger toolchain which will be dependent on the files extracted.

A quick note regarding SAGA versions: SAGA 7.5.0 has a bug in the coordinate transformation algorithm which is producing a lot of spots on the resulting maps. 7.4.0 is recommended to be used instead; however, if this is fixed in the next version, the script should be updated to change the clipping mechanism: currently, the way this works is that all of the grids are transformed at once, and then after that they are individually clipped and saved; however, 7.5.0 introduced that the grids could be clipped all at once which is much more efficient. If the transformation issue is fixed in a future released, this functionality should be incorporated which would negate the use for the "Mask_Input" variable to be pasted in the toolchain in a loop.


This next chunk will perform a similar process using the Google Earth Engine (again, the recommeded process). First, you must "initialize" the earth engine environment. This will open your web browser to a couple of pages asking for permission to write to your google drive folders. In the end, you will be given a key to copy and paste into a popup window in this R session, and initializaiton should be complete. Note that in the end the files written to Google Drive should not be very large, likely less than 500 Mb total, so that would be a good amount of available space to have on hand. It would also be possible to script automatic deletion of files after they are downloaded as well.

Next, provide the AOI shape as either an sf or sfc object. Note: It must be in the 4326 projection for GEE to properly read it. It's also a good idea to buffer the AOI in order to capture the edge effects in processing.

You need to create a dataframe with three columns: The first is an ID column, which will be used for creating subfolders in your downloads folder later. The second column is a start date column, and the third is an end date column. The example given here separates the dates by season; however, you can use as many or as little as you wish.

Finally the function is ran. You should specify a drive_folder for where outputs will be stored to on Google Drive as well as a download folder for when files are downloaded locally. The function takes sentinel 2 images from a specified date range (ex: winter), performs a cloud masking function to remove clouds from an image stack, and then calculates the median across that date range. The final image is clipped to your AOI, reprojected in BC Albers, and then saved to your google drive folder. Lastly, the image is downloaded locally to your machine where you specify.


# Generate satellite derivatives

After running your preferred download (and extraction) function, the satellite band files are then reprojected and masked using the DEM's projection parameters and resolution. The DEM should be supplied as a file path (note: the function has changed now to perform only across a single DEM rather than a whole bunch. If you want to do multiple resolutions, you can do so in an iterative loop around this function).

The function bands_to_indices will take bands located in a given folder (satellite_files), reproject and clip them to a given raster (reference_raster), and outputs a raster stack of indices which can be optionally saved to the main covariates folder. The option "single_scene" is essentially asking which function for downloading data you ran above since it has an impact on the spectralIndices function. The option remove_index can be used if you don't want a particular index in your results (ex: EVI2 seems to generate large areas of no data).

Finally, these reprojected and masked files are used in the processing of vegetation and soil indices. Here, the RStoolbox package is being used to generate these indices all at once. This is not meant to be a comprehensive list of which indices should be used for the PEM project, however I think it provides a solid base for thinking of which indices to include. Some documentation:

https://bleutner.github.io/RStoolbox/rstbx-docu/spectralIndices.html
https://pro.arcgis.com/en/pro-app/help/data/imagery/indices-gallery.htm
https://www.usgs.gov/land-resources/nli/landsat/landsat-surface-reflectance-derived-spectral-indices

You might notice that not all of the indices from the ArcGIS and USGS websites are included in this package. As mentioned, the RStoolbox is not meant to represent the "be all and end all" of defining which indices to use. Some may not be appropriate to use here, however the package makes it really easy to generate those indices quite quickly.

```{r Convert raster bands to satellite indices}
# This function will take downloaded satellite images, transorm them, and output
# multiple satellite image indices as a raster stack.
bands_to_indices <- function(
  reference_raster = NULL,
  satellite_files = NULL,
  single_scene = FALSE,
  output_path = NULL,
  remove_index = NULL)
  {

  # Perform input checks
  if(is.null(reference_raster) ||
     !class(reference_raster) %in% c("RasterLayer", "character")) {
    stop("\rError: You must specify a file path to a raster image")
  } else if(class(reference_raster) == "RasterLayer") {
    reference_raster <- reference_raster@file@name
  }

  if(is.null(satellite_files) || !is.character(satellite_files)) {
    stop(
      "\rError: You must supply a character vector of file paths to satellite
      \rraster images"
    )
  }

  process_path <- file.path(dirname(dirname(dirname(satellite_files[1]))), "2_reproject")

  if(!dir.exists(process_path)) dir.create(process_path, recursive = TRUE)
  ref <- raster(reference_raster)

  #############################################################################

  ## Pan sharpening testing: sentinel 2 doesn't have a pan sharpening band, but
  # it might be possible to use band 8 or the mean of bands 2, 3, 4, and 8:
  #https://www.mdpi.com/2072-4292/8/4/354/htm#B34-remotesensing-08-00354
  #https://www.researchgate.net/publication/323933204_Sentinel-2_Pan_Sharpening_-_Comparative_Analysis

  res_10 <- stack(satellite_files[sapply(satellite_files, function(x)
    res(raster(x))[1], simplify = TRUE) %>%
      grep(pattern = 10, value = FALSE)])
  res_20 <- stack(satellite_files[sapply(satellite_files, function(x)
    res(raster(x))[1], simplify = TRUE) %>%
      grep(pattern = 20, value = FALSE)])

  # Band 8 only as pan band:
  # pan <- raster(grep("b08", satellite_files, value = TRUE))

  # Mean of bands 2, 3, 4, and 8 as pan band:
  raster::beginCluster(parallel::detectCores())
  pan <- clusterR(res_10, mean, args = list(na.rm = TRUE))
  raster::endCluster()

  message("\rApplying pan sharpening to low resolution bands")

  satellite_stack <- stack(
    res_10, panSharpen(res_20, pan = pan, method = "pca", norm = FALSE)
  )
  names(satellite_stack) <- gsub("_pan$", "", names(satellite_stack))

  # Change band names to be more descriptive
  band_lookup <- tribble(
    ~band_no, ~band_name,
    "b01", "ultrablue", "b02", "blue", "b03", "green", "b04", "red",
    "b05", "redEdge1", "b06", "redEdge2", "b07", "redEdge3", "b08", "nir",
    "b09", "wv", "b11", "swir2", "b12", "swir3", "b8A", "narrow_nir"
  )

  for(i in 1:nlayers(satellite_stack)) {
    if(any(grepl(names(satellite_stack)[i], band_lookup$band_no))) {
      names(satellite_stack)[i] <- band_lookup$band_name[
        which(band_lookup$band_no %in% names(satellite_stack)[i])
        ]
    }
  }

  # Produce vegetation indices from band layers
  satellite_indices <- spectralIndices(
    img = satellite_stack,
    blue = "blue", green = "green", red = "red", nir = "nir",
    redEdge1 = "redEdge1", redEdge2 = "redEdge2", redEdge3 = "redEdge3",
    swir2 = "swir2", swir3 = "swir3",
    scaleFactor = ifelse(single_scene == TRUE, 10000, 1),
    coefs = list(swir2ccc = satellite_stack$swir2@data@min,
                 swir2coc = satellite_stack$swir2@data@max)
  )

  if(!is.null(remove_index)) {
    satellite_indices <- dropLayer(satellite_indices, remove_index)
  }
  message("\rMasking and writing ouputs")

  # Unfortunately this step takes a while. Could perhaps move this over to SAGA...
  satellite_indices <- projectRaster(satellite_indices, ref)
  for(i in 1:nlayers(satellite_indices)) {
    mask(
      subset(satellite_indices, i), ref, format = "GTiff",
      filename = file.path(
        output_path,
        tolower(paste0("sentinel2_",
                       names(satellite_indices)[i], "_",
                       basename(dirname(satellite_files[1]))))),
      overwrite = TRUE)

  }
  # Generare an RGB for background image
  RGB <- subset(satellite_stack, c("red", "green", "blue"))
  writeRaster(
    RGB,
    file.path(
      process_path,
      paste0("sentinel2_RGB_", basename(dirname(satellite_files[1])), ".tif")),
    overwrite = TRUE)

  return(satellite_indices)
}
```

Finally, we just need to run the function. I've included examples for processing this when a single scene is downloaded, and for when composite images from google earth engine have been downloaded

```{r run functions}
# Initialize the google earth engine
ee <- import("ee")
ee$Initialize()

# Create a bounding box of the AOI. Needs to be transformed to EPSG 4326
AOI_bbox <- st_read(AOI_shapefile, quiet = TRUE) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(dist = 250, joinStyle = "MITRE") %>%
  st_transform(4326)

# Provide a data frame of dates to collect data between. The longer the data
# frame, the longer it will take to process and download your dataset.
seasons_df <- data.frame(
  season = c("winter", "spring", "summer", "fall"),
  start = c("2018-12-21", "2019-03-20", "2019-06-21", "2019-09-23"),
  end = c("2019-03-19", "2019-06-20", "2019-09-22", "2019-12-20"),
  stringsAsFactors = FALSE
)

# Run get_sentinel_ee function to process and download satellite images
ee_download <- get_sentinel_ee(
  aoi = AOI_bbox,
  template_raster = dem,
  date_ranges = seasons_df,
  drive_folder = "Sentinel2",
  download_folder = file.path(raw_path, "1_download")
)

season_dirs <- dir(file.path(raw_path, "1_download"),
                   full.names = TRUE)[dir(file.path(raw_path, "1_download"))
                                      %in% seasons_df$season]

# Run the bands_to_indices function to produce satellite indices for each
# of the date ranges
for(i in season_dirs) {
  sat_indices <- bands_to_indices(
    reference_raster = dem,
    satellite_files = list.files(i, full.names = TRUE),
    single_scene = FALSE,
    output_path = output_path,
    remove_index = c("EVI2")
  )
}

```

##Water from NDWI layer
According to multiple sources, values of > 0 for the MNDWI layer indicate the presence of a water body. This process can be easily scripted to detect the location of water across a landscape and can be used to compare to that of the water layer downloaded as shapes from the bcdata package

https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2018RG000598

https://www.researchgate.net/profile/Hanqiu_XU/publication/232724072_Modification_of_Normalized_Difference_Water_Index_NDWI_to_Enhance_Open_Water_Features_in_Remotely_Sensed_Imagery/links/5c9aee13299bf1116949a345/Modification-of-Normalized-Difference-Water-Index-NDWI-to-Enhance-Open-Water-Features-in-Remotely-Sensed-Imagery.pdf

```{r find water from MNDWI}
# Looking at all of the mndwi files produced, the best one to use is going to be
# the one from the summer time frame
mndwi_summer <- raster(file.path(output_path, "sentinel2_mndwi_summer.tif"))

# Reclassify raster values
water <- reclassify(mndwi_summer, c(-Inf, 0, NA, 0, Inf, 1))

# Raster to sf conversion via stars package
water_sf <- st_as_stars(water) %>%
  st_as_sf(as_points = FALSE, merge = TRUE, na.rm = TRUE, use_integer = TRUE) %>%
  st_geometry() %>%
  st_union()

st_write(
  water_sf,
  file.path(AOI_dir, "0_raw_inputs", "base_layers", map_res, "water_mndwi.gpkg"),
  delete_layer = TRUE)
```



