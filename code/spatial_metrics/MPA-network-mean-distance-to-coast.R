rm(list=ls())
getwd()
# Load libraries
library(readr)
library(sf)
library(units)
library(dplyr)
library(raster)
library(spData)
library(terra)
library(spDataLarge)
library(stars)
library(qgisprocess)
library(ggplot2)
library(polynom) #to solve polynomial equation
library(here)

# Function to get adjacent indices of cells
get.adjacent.indices.fn <- function(mpa.ind, ncol, nrow){ 
  
  adjacent_indices_row = c()
  adjacent_indices_col = c()
  
  n = 1
  
  while (n <= dim(mpa.ind)[1]){
    i = mpa.ind$row[n]
    j = mpa.ind$col[n]
    
    if (i > 1) {
      adjacent_indices_row = c(adjacent_indices_row, i-1)
      adjacent_indices_col = c(adjacent_indices_col, j)
    }
    if (i < nrow){
      adjacent_indices_row = c(adjacent_indices_row, i+1)
      adjacent_indices_col = c(adjacent_indices_col, j)
    }
    if (j > 1){
      adjacent_indices_row = c(adjacent_indices_row, i)
      adjacent_indices_col = c(adjacent_indices_col, j-1)
    }
    if (j < ncol){
      adjacent_indices_row = c(adjacent_indices_row, i)
      adjacent_indices_col = c(adjacent_indices_col, j+1)
    }
    if (i < nrow & j < ncol){
      adjacent_indices_row = c(adjacent_indices_row, i+1)
      adjacent_indices_col = c(adjacent_indices_col, j+1)
    }
    if (i > 1 & j > 1){
      adjacent_indices_row = c(adjacent_indices_row, i-1)
      adjacent_indices_col = c(adjacent_indices_col, j-1)
    }
    if (i > 1 & j < ncol){
      adjacent_indices_row = c(adjacent_indices_row, i-1)
      adjacent_indices_col = c(adjacent_indices_col, j+1)
    }
    if (i < nrow & j > 1){
      adjacent_indices_row = c(adjacent_indices_row, i+1)
      adjacent_indices_col = c(adjacent_indices_col, j-1)
    }
    n = n + 1
  }
  adjacent_ind = cbind(adjacent_indices_row, adjacent_indices_col)
  return(adjacent_ind)
}

################ START - calculate distance to coast of Med_grid cells
Med_grid_boundary = st_read(here("data/shapefiles/Med_grid_boundary_joint_Med_polygon.shp"))


calculate.distance.to.coast.of.all.cells.fn <- function(Med_grid_boundary){
  Med_grid_boundary = Med_grid_boundary[, c(1,2,5,6)]
  colnames(Med_grid_boundary) = c("fid", "VALUE", "Distance_to_coast", "geom")
  
  condition = TRUE
  
  i = 1
  
  while (condition == TRUE){
    
    Med_matrix = matrix(Med_grid_boundary$Distance_to_coast, ncol = 190, nrow = 83, byrow= TRUE) # convert list of cell id to matrix
    mpa.ind = as.data.frame(which(Med_matrix == i, arr.ind = TRUE))
    adjacent_ind = get.adjacent.indices.fn(mpa.ind, 190, 83)
    adjacent_ind_df = as.data.frame(adjacent_ind)
    cell_id = unique(adjacent_ind_df$adjacent_indices_col + 190* (adjacent_ind_df$adjacent_indices_row - 1))
    
    potential_cells = cell_id[Med_grid_boundary$VALUE[cell_id] == 0 & Med_grid_boundary$Distance_to_coast[cell_id] == 0]
    print(table(Med_grid_boundary$VALUE[potential_cells]))
    
    if (length(potential_cells)>0){
      condition = TRUE
      Med_grid_boundary$Distance_to_coast[potential_cells] = i + 1
      table(Med_grid_boundary$Distance_to_coast)
      #plot(Med_grid_boundary$geom[Med_grid_boundary$Distance_to_coast == i + 1])
      i = i + 1
    } else{
      condition = FALSE
    }
    
  }
}



############### Look where MPAs are in each scenario

output.Dir.list <- c(here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Mazor_scenario_8/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Mazor_scenario_9/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Micheli/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Existing_network/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Random/rep11/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Random/rep12/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Random/rep13/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Random/rep14/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Random/rep15/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Random/rep16/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Random/rep17/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Random/rep18/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Random/rep19/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/Random/rep20/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/EEZ-conservation/rep1/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/EEZ-conservation/rep2/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/EEZ-conservation/rep3/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/EEZ-conservation/rep4/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/EEZ-conservation/rep5/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/EEZ-conservation/rep6/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/EEZ-conservation/rep7/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/EEZ-conservation/rep8/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/EEZ-conservation/rep9/csv_gpkg_files"),
                     here("Data/Scenario_csv_masks/FINAL_2022-11-08-MPA_scenarios/EEZ-conservation/rep10/csv_gpkg_files"))

scenarios <- c("Mazor scenario 8",
               "Mazor scenario 9",
               "Micheli",
               "Existing network", 
               "Random 11",
               "Random 12",
               "Random 13",
               "Random 14",
               "Random 15",
               "Random 16",
               "Random 17",
               "Random 18",
               "Random 19",
               "Random 20",
               "EEZ-conservation 1",
               "EEZ-conservation 2",
               "EEZ-conservation 3",
               "EEZ-conservation 4",
               "EEZ-conservation 5",
               "EEZ-conservation 6",
               "EEZ-conservation 7",
               "EEZ-conservation 8",
               "EEZ-conservation 9",
               "EEZ-conservation 10")

mean = c()
total = c()
mpa_coverage = 10
for (i in 1:length(output.Dir.list)){
  mpas =read_csv(paste0(output.Dir.list[i], "/", mpa_coverage, ".csv"), col_names = FALSE)
  
  mpa.ind = as.data.frame(which(mpas == 1, arr.ind = TRUE))
  mpa_id = unique(mpa.ind$col + 190* (mpa.ind$row - 1))
  
  mean_distance_to_coast = mean(Med_grid_boundary$Distance_to_coast[mpa_id] * 20)# en km
  total_distance_to_coast = sum(Med_grid_boundary$Distance_to_coast[mpa_id] * 20)# en km
  
  mean =  c(mean, mean_distance_to_coast) 
  total = c(total, total_distance_to_coast) 
}


Distance_to_coast = as.data.frame(mean)
Distance_to_coast$total = total
Distance_to_coast$scenario = scenarios

write_csv(Distance_to_coast, "Data/MPA_spatial_metrics/Distance_to_coast.csv")

#################### Calculer distance moyenne des patchs AMPs à la côte.
# D'abord distance de chaque MPA patch à la côte est calculé à partir de son centroide (en m)
# Puis moyenne et somme de la distance

# Contour of Med grid
Med_grid_sea_contour = st_read(here("data/shapefiles/Med_grid_sea_contour.shp"))
plot(Med_grid_sea_contour$geometry)

# MPA indices

mean_distance_to_coast = c()
total_distance_to_coast = c()
mpa_coverage = 10

for (i in 1:length(output.Dir.list)){
  Med_grid = st_read(here("data/shapefiles/Med_polygon.gpkg"))
  
  mpas = read_csv(paste0(output.Dir.list[i], "/", mpa_coverage, ".csv"), col_names = FALSE)
  
  mpa.ind = as.data.frame(which(mpas == 1, arr.ind = TRUE))
  mpa_id = unique(mpa.ind$col + 190* (mpa.ind$row - 1))
  
  Med_grid$VALUE[mpa_id] = 1
  MPA = Med_grid[Med_grid$VALUE == 1,]
  #plot(MPA$geom)
  #crs(MPA)
  #plot(Med_grid_sea_contour$geometry)
  
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
write_csv(dist_to_coast_metric, here("data/spatial_metrics/Distance_to_coast_from_centroid.csv"))
