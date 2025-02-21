#' Script to create fishing effort map using Synthethic Aperture Radar imagery
#'
#' 
#' @param data vessel detections using the SUMO algorithm
#' 
#' @return csv of spatialized fishing effort
#'
#' @export
#' 


rm(list=ls())

#-----------------Loading packages-------------------

pkgs <- c("tidyverse","here","lme4","broom","tidymodels","parallel","cowplot","sf","RColorBrewer","ggridges","plotly","heatmaply","parsedate","birk","ggthemes","MASS","automap",
          "harrypotter","wesanderson","ranger","ggpubr","data.table","xml2","XML","rnaturalearth","ggExtra","raster","exactextractr","gstat","magrittr","scales","grid","gridExtra", "terra", "filesstrings", "imager")
nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)
ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

#-----------------Loading all data ---------------------------------------------

files <- list.files(here("data/SAR_Mediterranean"), pattern="Rdata", full.names = T)
data_list = lapply(files, load, .GlobalEnv)
sar_2019 <- rbind(S1A_2019_medsea_detections, S1B_2019_medsea_detections)

#----------------- Loading OSMOSE grid -----------------------------------------

OSMOSE_raster = raster(here("data/SAR_Mediterranean/OSMOSE_MED_shapefile/rastmed_20.tif"))
OSMOSE_grid = st_read(here("data/SAR_Mediterranean/OSMOSE_MED_shapefile/OSMOSEMED_grid_sea.shp")) #crs 3035
OSMOSE_sea = filter(OSMOSE_grid, VALUE==0) # keep only cells in grid corresponding to sea


#-----------------Loading all functions-----------------------------------------

files.source = list.files(here("code/fishing_effort/process"), full.names = T)
sapply(files.source, source)


# DATA PROCESSING 
data_reprocess = FALSE

if (data_reprocess == TRUE){

  # Data cleaning
  sar_all_cleaned = data_clean(sar_2019) # WGS84
  
  # ---------Filtering shipping routes------------------------------------------
  shipping_routes = raster(here("data/SAR_Mediterranean/shipping/shipping.tif")) # WGS84
  
  sar_all_cleaned_filter_shipping = filter_shipping(sar_all_cleaned, shipping_routes) # converting shipping routes to crs 3035
  
  # ---------Filtering detections by vessel length (only keeps boats under 60 m) --------------------
  
  sar_all_cleaned_filter_shipping$length = as.numeric(sar_all_cleaned_filter_shipping$length) # convert from chr to num
  sar_all_cleaned_filter_shipping_under60 = filter(sar_all_cleaned_filter_shipping, sar_all_cleaned_filter_shipping$length<60)
  
  # --------Filtering offshore platforms ---------------------------------------
  
  offshore_platforms = st_read(here("data/SAR_Mediterranean/offshore_platforms_EMODnet/EMODnet_HA_OG_Offshore_Installations_20220422.shp"))
  sar_all_cleaned_filter_shipping_under60_noplatforms = remove_emodnet(SAR.shp = sar_all_cleaned_filter_shipping_under60, emodnet.shp = offshore_platforms)
  
} else{
  sar_all_cleaned_filter_shipping_under60_noplatforms = st_read(here("data/SAR_Mediterranean/SAR_cleaned_2019_under_60m_filter_shipping_platforms.shp"))
  
}


# --------------Delete RFI-labelled images from dataset  ----------------------- 

sar_all_cleaned_filter_shipping_under60_noplatforms_noRFI = filter.RFI.fn(sar_all_cleaned_filter_shipping_under60_noplatforms)


# ----------Transforming data to OSMOSE_grid crs - 3035 ------------------------

sar_data = st_transform(sar_all_cleaned_filter_shipping_under60_noplatforms_noRFI, 3035)

#----------------- Interpolating SAR detections to OSMOSE grid -----------------

# Intersection between OSMOSE grid cells and SAR detections
pt_count_boats <- st_intersects(OSMOSE_grid, sar_data)

# Count boats per cell
boat_count <- unlist(lapply(pt_count_boats, length))  

# Assign boat count to OSMOSE_grid
OSMOSE_grid$vessel_count = boat_count


# ----- Computing a correction factor of number of detections per cell according to number of images per cell--------------------------------

# Extent of areas photographed by the satellite in a 12-day period 
combinedShp = st_read(here("data/SAR_Mediterranean/combinedShp_12days.shp"))
plot(combinedShp$geometry, col="blue")

# Intersect bbox of areas between them to find number of overlaps per area in a 12-day period
intersect_bbox = st_intersection(combinedShp$geometry, combinedShp$geometry)
intersect_bbox_centroid = st_centroid(intersect_bbox) # points representing centroids of polygons that intersect

image_count <- st_intersects(combinedShp, intersect_bbox_centroid)
image_count_per_area <- unlist(lapply(image_count, length))
combinedShp$image_count_per_area <- image_count_per_area

st_crs(combinedShp) <-   "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Intersection between OSMOSE_grid and image count per bbox
combinedShp_3035 <- st_transform(combinedShp, 3035)
pt_count_OSMOSE <- st_intersects(OSMOSE_grid, combinedShp_3035)
image_count_per_area <- unlist(lapply(pt_count_OSMOSE, length))
image_density_per_area <- image_count_per_area/nrow(combinedShp_3035)

OSMOSE_grid$number_of_images_per_cell <- image_count_per_area

# Apply correction factor to cells corresponding to the sea (value = 0) 
OSMOSE_grid$correction_factor[OSMOSE_grid$VALUE == 0] <- max(image_count_per_area)/OSMOSE_grid$number_of_images_per_cell[OSMOSE_grid$VALUE == 0]

# Correct boat count
boat_count_corrected = OSMOSE_grid$vessel_count * OSMOSE_grid$correction_factor
boat_density_corrected <- boat_count_corrected/400
boat_density_corrected_standardised <- boat_density_corrected/mean(boat_density_corrected, na.rm=TRUE)
boat_count_corrected_standardised <-boat_count_corrected / mean(boat_count_corrected, na.rm = T)

boat_density_corrected_standardised[is.na(boat_density_corrected_standardised)] <- 0 #replace NAs with 0s

sum(boat_density_corrected_standardised, na.rm = T)
mean(boat_density_corrected_standardised, na.rm = T)

# To create a dataframe of vessels count ---------------------------------------
OSMOSE_grid$boat_count_corrected = OSMOSE_grid$vessel_count * OSMOSE_grid$correction_factor

st_write(OSMOSE_grid, here("data/SAR_Mediterranean/boat_count_cells.gpkg"))

# -------------- Create fishing effort csv for OSMOSE -------------------------------------

OSMOSE_rast = rast(ncol=190, 
                   nrow=83, 
                   xmin=2880851, 
                   xmax=6680851, 
                   ymin=855444, 
                   ymax=2515444) 

# Assign raster cell values of boat density corrected and standardised
values(OSMOSE_rast) = boat_density_corrected_standardised
crs(OSMOSE_rast)<- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
# Plot
brk = c(0.1,1,2,3,4,5,10,15,20,25,30, 40,50)
plot(OSMOSE_rast, breaks = brk, col = rev(viridis(20)), axes = FALSE)

# Convert raster to csv file
write.table(as.array(OSMOSE_rast), 
            here("data/SAR_Mediterranean/Fishing_effort__density_boats_under60_filtered_satellitebiaiscorrected.csv"),
            row.names = FALSE, 
            col.names=FALSE, 
            sep=",")



# plot Figure A2.3 - SAR detections used for mapping fishing effort for OSMOSE-MED
figA2.3.map.detections <- ggplot() +
  geom_sf(fill="white" ) +
  geom_sf(data=sar_all_cleaned_filter_shipping_under60_noplatforms_noRFI, size = 1.5, alpha = 0.7, shape = ".", color = "red") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "#FEEFDB", fill=NA, size=1))

ggsave(here("figures/figA2_SAR_detections_Med.png"), width = 10, height = 5, dpi = 1000)
