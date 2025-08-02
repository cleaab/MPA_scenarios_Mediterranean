#' Function to calculate spatial metrics
#'
#' 
#' @param data 
#' 
#' @return spatial metrics from landscapemetrics v 2.1.4 and mean distance to coast
#'
#' @export
#' 

# Calculate average network distance to coast for 10% coverage ------------------------------------

calculate.network.distance.to.coast.fn <- function(Med_grid_sea_contour, MPA_scenarios, scenarios, lit.mpa.mask.dir, random.mpa.mask.dir, mpa_coverage = 10, save = TRUE){
  
  # MPA indices
  mean_distance_to_coast = c()
  total_distance_to_coast = c()
  
  for (i in 1:MPA_scenarios){
    
    Med_grid = st_read(here("data/shapefiles/Med_polygon.gpkg"))
    
    
    if (i <= 4){
      
      mpas = read_csv(paste0(lit.mpa.mask.dir[i], "/", mpa_coverage, ".csv"), col_names = FALSE)
      
    } else{
      mpas = read_csv(paste0(random.mpa.mask.dir[i-4], "/", mpa_coverage, ".csv"), col_names = FALSE)
      
    }
    
    
    mpa.ind = as.data.frame(which(mpas == 1, arr.ind = TRUE))
    mpa_id = unique(mpa.ind$col + 190* (mpa.ind$row - 1))
    
    Med_grid$VALUE[mpa_id] = 1
    MPA = Med_grid[Med_grid$VALUE == 1,]
    
    MPA_union = st_union(MPA)
    
    distance_to_coast = c()
    
    for (i in 1:length(MPA_union[[1]])){
      
      coordinates = as.data.frame(MPA_union[[1]][[i]][[1]])
      MPA_sf <- coordinates %>%
        st_as_sf(coords = c("V1", "V2"), crs=3035)
      centroid = st_centroid(st_union(MPA_sf))
      distance_to_coast = c(distance_to_coast, st_distance(Med_grid_sea_contour, centroid))
      
    }
    
    mean_distance_to_coast = c(mean_distance_to_coast, mean(distance_to_coast))
    total_distance_to_coast = c(total_distance_to_coast, sum(distance_to_coast))
  }
  
  dist_to_coast_metric = data.frame(mean_distance_to_coast)
  dist_to_coast_metric$total_distance_to_coast = total_distance_to_coast
  dist_to_coast_metric$scenario = scenarios
  
  if (save == TRUE){
    write_csv(dist_to_coast_metric, here("data/spatial_metrics/Distance_to_coast_from_centroid.csv"))
  }
}



# Calculate metrics from landscapemetrics R package ---------------------------------------------

calculate.metrics.landscapemetrics.fn <- function(MPA_scenarios, scenarios, lit.mpa.mask.dir, random.mpa.mask.dir, mpa_coverage = 10, save = TRUE){
  
  
  Metrics_mpa_scenarios = data.frame()
  
  for (i in 1:MPA_scenarios){
    
    Med_grid = st_read(here("data/shapefiles/Med_polygon.gpkg"))
    
    if (i <= 4){
      
      mpas = read_csv(paste0(lit.mpa.mask.dir[i], "/", mpa_coverage, ".csv"), col_names = FALSE)
      
    } else{
      mpas = read_csv(paste0(random.mpa.mask.dir[i-4], "/", mpa_coverage, ".csv"), col_names = FALSE)
      
    }
    
    mpa.ind = as.data.frame(which(mpas == 1, arr.ind = TRUE))
    mpa_id = unique(mpa.ind$col + 190* (mpa.ind$row - 1))
    
    Med_grid$VALUE[mpa_id] = 1
    
    MPA_rast = rast(ncol=190, 
                    nrow=83, 
                    xmin=2880851, 
                    xmax=6680851, 
                    ymin=855444, 
                    ymax=2515444)
    
    Med_grid$VALUE[Med_grid$VALUE == -99] = NA
    values(MPA_rast) = Med_grid$VALUE # assign raster cell values
    plot(MPA_rast)
    class(MPA_rast)
    crs(MPA_rast) = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"
    crs(MPA_rast)
    check_landscape(MPA_rast)
    
    # Calculate metrics
    Metrics_all = calculate_lsm(MPA_rast, level = c("class"), directions = 8, neighbourhood = 8) 
    #Metrics_all = calculate_lsm(MPA_rast, level = c("patch"), directions = 8, neighbourhood = 8) # for metrics by patch
    Metrics_mpa = Metrics_all[Metrics_all$class == 1, ]
    Metrics_mpa$scenario = scenarios[i]
    
    Metrics_mpa_scenarios = rbind(Metrics_mpa_scenarios, Metrics_mpa)
  }
  
  if (save == TRUE){
    
    write_csv(Metrics_mpa_scenarios, here("data/spatial_metrics/Metrics_landscapemetrics.csv"))
    
  }
  
}
  