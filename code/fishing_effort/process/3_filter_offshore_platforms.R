#Script to remove known immobile objects from potential false positives
#IMPORTANT : CRS needs to be WGS84 decimal degrees 
#Also, it works on .shp files so be sure to have all adjacent files in the same folder (.dbf, .prj., .shx, ...)


# Needed library ----------------------------------------------------------

library(sf)

# Function ----------------------------------------------------------------

remove_emodnet <- function(SAR.shp, emodnet.shp){
  
  coords_emodnet <- as.data.frame(st_coordinates(emodnet.shp$geometry))
  coords_SAR <- as.data.frame(st_coordinates(SAR.shp$geometry))
  colnames(coords_emodnet) <- c("lon","lat")
  colnames(coords_SAR) <- c("lon","lat")
  remove <- c()
  
  for (i in 1:length(coords_emodnet$lon)) {
    #We create a square buffer of around 10m around the structure (actually 0.0001 degrees = 11.1 meters)
    #If we want to use another coordinate system, we would just need to change this part
    lon_max <- coords_emodnet$lon[i]+0.0001
    lon_min <- coords_emodnet$lon[i]-0.0001
    lat_max <- coords_emodnet$lat[i]+0.0001
    lat_min <- coords_emodnet$lat[i]-0.0001
    
    for (j in 1:length(coords_SAR$lon)){
      if (coords_SAR$lon[j]>lon_min && coords_SAR$lon[j]<lon_max && coords_SAR$lat[j]>lat_min && coords_SAR$lat[j]<lat_max){
        #We identify all detections that find themselves in that buffer
        remove <- c(remove, j)
        
      }
    }
  }
  
  #We remove all those detections from the original dataset
  SAR_emodnetless <- SAR.shp[-remove,]
  
  return(SAR_emodnetless)
}


