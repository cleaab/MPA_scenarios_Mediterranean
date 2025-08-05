#' Scripts to compute different fishing redistribution strategies
#'
#' 
#' @param data MPA scenarios
#' 
#' @return MPA scenarios after redistribution (inputs for OSMOSE-MED)
#'
#' @export
#' 


rm(list=ls())
library(tidyverse)
library(terra)
library(here)
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(RColorBrewer)

# Read shapefile of GSA --------------------------------------------------------
# For plots
worldbounds = st_read(here("data/shapefiles/world_map/world-bounds/world-bounds-a.shp"))

worldbounds_3035 = st_transform(worldbounds, 3035)

gsa_outline = st_read(here("data/shapefiles/GSAs_simplified/GSAs_simplified_outline.shp"))

gsa <- st_read(here("data/shapefiles/GSAs_simplified/GSAs_simplified.shp"))

gsa_Med <- gsa %>%
  filter(!F_GSA_LIB %in% c("GSA 28", "GSA 29", "GSA 30")) %>%
  dplyr::select("SMU_NAME", "F_GSA_LIB", "geometry") %>% 
  rename("GSA_name" = "SMU_NAME", "GSA" = "F_GSA_LIB") %>%
  st_transform(3035) # transform crs to 3035 (same as Med_grid)

plot(gsa_Med$geometry)

Med_grid = st_read(here("data/shapefiles/Med_polygon.gpkg"))
Med_grid$cell_ID = as.numeric(rownames(Med_grid))

# Assign each GSA to a cell
Intersect.Medgrid.GSA = st_intersection(Med_grid, gsa_Med) %>%
  filter(VALUE == 0) %>%
  filter(!duplicated(cell_ID)) %>%
  dplyr::select(-"VALUE") %>% 
  st_drop_geometry()


# Assign a GSA number to each OSMOSE-MED cell
Med_grid_GSA = merge(Med_grid, Intersect.Medgrid.GSA, by="cell_ID", all = TRUE)

# Assign orphan cell to GSA 3
Med_grid_sea = Med_grid %>%
  filter(VALUE == 0)
Med_grid_GSA$GSA[Med_grid_GSA$cell_ID == setdiff(Med_grid_sea$cell_ID, Intersect.Medgrid.GSA$cell_ID)] = "GSA 3"
Med_grid_GSA$GSA_name[Med_grid_GSA$cell_ID == setdiff(Med_grid_sea$cell_ID, Intersect.Medgrid.GSA$cell_ID)] = "Southern Alboran Sea"


# Load fishing effort file -----------------------------------------------------
effort = read_csv(here("data/SAR_Mediterranean/Fishing_effort__density_boats_under60_filtered_satellitebiaiscorrected.csv"),  col_names = FALSE)
class(effort)

effort.cells = unlist(as.list(t(effort))) # unlisted not on the right way
class(effort.cells)

# Assign effort to each cell
Med_grid_GSA$initial_effort = effort.cells
sum(Med_grid_GSA$initial_effort[Med_grid_GSA$VALUE == 0], na.rm = T)
mean(Med_grid_GSA$initial_effort[Med_grid_GSA$VALUE == 0], na.rm = T)


# Load MPA files ---------------------------------------------------------------

MPA_dir = here("data/MPA_scenarios/")

MPA.scenarios.name <- c("Existing_network",
                        "Micheli",
                        "Mazor_scenario_8",
                        "Mazor_scenario_9")

mpa_step = 30

# Uniform redistribution by GSA ----------------------------------------------
for (scen in 1:length(MPA.scenarios.name)){
  for (mpa_coverage in 1:mpa_step){

    # Create dirs
    dir.create(paste0(MPA_dir, "/", MPA.scenarios.name[scen], "/after_redistribution"))
    dir.create(paste0(MPA_dir, "/", MPA.scenarios.name[scen],"/after_redistribution/csv_gpkg_files"))
    dir.create(paste0(MPA_dir, "/", MPA.scenarios.name[scen], "/after_redistribution/Plots"))
    
    # -----
    MPA = read_csv(paste0(MPA_dir, "/", MPA.scenarios.name[scen], "/csv_gpkg_files/", mpa_coverage, ".csv"), col_names =  FALSE)
    MPA.cells = unlist(as.list(t(MPA))) # unlisted not on the right way
    Med_grid_GSA$MPA = MPA.cells # 0 - unprotected, 1 - no-take area
    
    Med_grid_GSA_allcells = Med_grid_GSA %>%
      group_by(GSA, MPA) %>%
      mutate(total_effort_by_GSA = sum(initial_effort)) %>% 
      mutate(nb_of_cells_by_GSA = length(cell_ID)) %>% # number of cells outside MPAs and inside MPAs by GSA because of group_by
      ungroup() %>%
      mutate(GSA_name = case_when(VALUE == -99 ~ "terrestrial",
                                  TRUE ~ GSA_name)) %>%
      mutate(GSA = case_when(VALUE == -99 ~ "terrestrial",
                             TRUE ~ GSA)) %>%
      mutate(VALUE = MPA)
    
    Med_grid_GSA_wide = pivot_wider(Med_grid_GSA_allcells, names_from = "MPA", values_from = "total_effort_by_GSA", names_expand = TRUE)
    
    # Summarize info by GSA
    Med_grid_GSA_final = Med_grid_GSA_allcells %>%
      group_by(GSA, MPA) %>%
      summarise(effort_by_GSA = sum(initial_effort)) %>%
      pivot_wider(names_from = "MPA", values_from = "effort_by_GSA") %>% 
      st_drop_geometry() %>%
      rename("initial_effort_outside_byGSA" = "0", "initial_effort_inside_byGSA" = "1") %>% 
      dplyr::select(-"-99") %>% 
      group_by(GSA) %>% 
      summarize(initial_effort_outside_byGSA = sum(initial_effort_outside_byGSA, na.rm = T),
                initial_effort_inside_byGSA = sum(initial_effort_inside_byGSA, na.rm = T)) %>% 
      right_join(Med_grid_GSA_wide, by = "GSA") %>%
      dplyr::select(-"-99", -"0", -"1") %>%
      ungroup() %>%
      arrange(cell_ID) %>%
      mutate(new_effort_uniform = case_when(VALUE == 1 ~ 0,
                                            TRUE ~ initial_effort + initial_effort_inside_byGSA/nb_of_cells_by_GSA)) %>% #nb of cells outside GSA
      mutate(new_effort_proportional = case_when(VALUE %in% c(1, -99) ~ 0,
                                                 TRUE ~ initial_effort + initial_effort*initial_effort_inside_byGSA/initial_effort_outside_byGSA)) %>%
      
      mutate(new_effort_prop_test2 = case_when(VALUE %in% c(1, -99) ~ 0,
                                               TRUE ~ initial_effort*initial_effort_inside_byGSA))
    
    
    sum(unique(Med_grid_GSA_final$initial_effort_outside_byGSA)) + sum(unique(Med_grid_GSA_final$initial_effort_inside_byGSA))
 
    

    # Save corrected effort files
    Effort_uniform = matrix(Med_grid_GSA_final$new_effort_uniform, ncol=190, byrow=TRUE)
    
    write.table(Effort_uniform,
                paste0(MPA_dir, "/", MPA.scenarios.name[scen],"/after_redistribution/csv_gpkg_files/Corrected_effort_uniform_by_GSA_", mpa_coverage, ".csv"),
                row.names = FALSE,
                col.names=FALSE,
                sep=",")
    
    
    Effort_prop = matrix(Med_grid_GSA_final$new_effort_proportional, ncol=190, byrow=TRUE)
    
    write.table(Effort_prop,
                paste0(MPA_dir, "/", MPA.scenarios.name[scen],"/after_redistribution/csv_gpkg_files/Corrected_effort_prop_by_GSA_", mpa_coverage, ".csv"),
                row.names = FALSE,
                col.names=FALSE,
                sep=",")
  }
}


# Random scenarios -------------------------------------------------------------
MPA.scenarios.name <- c("EEZ-conservation", 
                        "Random")


for (scen in 1:length(MPA.scenarios.name)){
  for(rep in 1:10){
    if (scen == 2){
      rep = 10+rep
    }
    for (mpa_coverage in 1:mpa_step){
      # Create dirs
      dir.create(paste0(MPA_dir, "/", MPA.scenarios.name[scen], "/rep", rep, "/after_redistribution"))
      dir.create(paste0(MPA_dir, "/", MPA.scenarios.name[scen], "/rep", rep, "/after_redistribution/csv_gpkg_files"))
      dir.create(paste0(MPA_dir, "/", MPA.scenarios.name[scen], "/rep", rep, "/after_redistribution/Plots"))
      
      # -----
      MPA = read_csv(paste0(MPA_dir, "/", MPA.scenarios.name[scen],  "/rep", rep, "/csv_gpkg_files/", mpa_coverage, ".csv"), col_names =  FALSE)
      MPA.cells = unlist(as.list(t(MPA))) # unlisted not on the right way
      Med_grid_GSA$MPA = MPA.cells # 0 - unprotected, 1 - no-take area
      
      Med_grid_GSA_allcells = Med_grid_GSA %>%
        group_by(GSA, MPA) %>%
        mutate(total_effort_by_GSA = sum(initial_effort)) %>% 
        mutate(nb_of_cells_by_GSA = length(cell_ID)) %>%
        ungroup() %>%
        mutate(GSA_name = case_when(VALUE == -99 ~ "terrestrial",
                                    TRUE ~ GSA_name)) %>%
        mutate(GSA = case_when(VALUE == -99 ~ "terrestrial",
                               TRUE ~ GSA)) %>%
        mutate(VALUE = MPA)
      
      Med_grid_GSA_wide = pivot_wider(Med_grid_GSA_allcells, names_from = "MPA", values_from = "total_effort_by_GSA", names_expand = TRUE)
      
      # Summarize info by GSA
      Med_grid_GSA_final = Med_grid_GSA_allcells %>%
        group_by(GSA, MPA) %>%
        summarise(effort_by_GSA = sum(initial_effort)) %>%
        pivot_wider(names_from = "MPA", values_from = "effort_by_GSA") %>% 
        st_drop_geometry() %>%
        rename("initial_effort_outside_byGSA" = "0", "initial_effort_inside_byGSA" = "1") %>% 
        dplyr::select(-"-99") %>% 
        group_by(GSA) %>% 
        summarize(initial_effort_outside_byGSA = sum(initial_effort_outside_byGSA, na.rm = T),
                  initial_effort_inside_byGSA = sum(initial_effort_inside_byGSA, na.rm = T)) %>% 
        right_join(Med_grid_GSA_wide, by = "GSA") %>%
        dplyr::select(-"-99", -"0", -"1") %>%
        ungroup() %>%
        arrange(cell_ID) %>%
        mutate(new_effort_uniform = case_when(VALUE == 1 ~ 0,
                                              TRUE ~ initial_effort + initial_effort_inside_byGSA/nb_of_cells_by_GSA)) %>% 
        mutate(new_effort_proportional = case_when(VALUE %in% c(1, -99) ~ 0,
                                                   TRUE ~ initial_effort + initial_effort*initial_effort_inside_byGSA/initial_effort_outside_byGSA))
      
      
      
      Effort_uniform = rast(ncol=190,
                            nrow=83,
                            xmin=2880851,
                            xmax=6680851,
                            ymin=855444,
                            ymax=2515444)
      values(Effort_uniform) = Med_grid_GSA_final$new_effort_uniform #assign raster cell values
  
      # Save corrected effort files
      Effort_uniform = matrix(Med_grid_GSA_final$new_effort_uniform, ncol=190, byrow=TRUE)
      sum(Med_grid_GSA_final$new_effort_uniform)
      mean(Med_grid_GSA_final$new_effort_uniform)
      
      write.table(Effort_uniform,
                  paste0(MPA_dir, "/", MPA.scenarios.name[scen], "/rep", rep, "/after_redistribution/csv_gpkg_files/Corrected_effort_uniform_by_GSA_", mpa_coverage, ".csv"),
                  row.names = FALSE,
                  col.names=FALSE,
                  sep=",")
      
      Effort_prop = matrix(Med_grid_GSA_final$new_effort_proportional, ncol=190, byrow=TRUE)
      sum(Med_grid_GSA_final$new_effort_proportional)
      mean(Med_grid_GSA_final$new_effort_proportional)
      
      write.table(Effort_prop,
                  paste0(MPA_dir, "/", MPA.scenarios.name[scen], "/rep", rep, "/after_redistribution/csv_gpkg_files/Corrected_effort_prop_by_GSA_", mpa_coverage, ".csv"),
                  row.names = FALSE,
                  col.names=FALSE,
                  sep=",")
      
      
    }
    
  }
}


### Plot effort and fishing strategies  ----------------------------------------
### ----------------------------------------------------------------------------

dir.create(here("figures/fishing_effort_maps"))

# Plot initial effort ------------------------------------------------------
# --------------------------------------------------------------------------


Effort_initial = rast(ncol=190, 
                      nrow=83, 
                      xmin=2880851, 
                      xmax=6680851, 
                      ymin=855444, 
                      ymax=2515444)
values(Effort_initial) = Med_grid_GSA_final$initial_effort #assign raster cell values


png(here("figures/fishing_effort_maps/Initial_effort_red_without_MPA_outline.png"), height = 481, width = 994)
ncol = 19
mycol = Reds(n = ncol)
brk = c(0.001,0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1, 1.5, 2, 3,4,5,10,15,20,25,30,40,max(Med_grid_GSA_final$initial_effort, na.rm = T))

image.plot(Effort_initial, breaks = brk, col = mycol, axes = FALSE, legend.cex = 3)
plot(worldbounds_3035$geometry,add=T, col= "#F0F0F0")
MPA_polygons = Med_grid_GSA %>%
  filter(MPA == 1)
MPA_polygons_union = st_union(MPA_polygons)
#plot(MPA_polygons_union, add = T, col = NA, border = "black", lwd = 2)
box()
dev.off()


png(here("figures/fishing_effort_maps/Initial_effort_with_MPA_outline.png"), height = 481, width = 994)
ncol = 19
mycol = Reds(n = ncol)
brk = c(0.001,0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1, 1.5, 2, 3,4,5,10,15,20,25,30,40,max(Med_grid_GSA_final$initial_effort, na.rm = T))

image.plot(Effort_initial, breaks = brk, col = mycol, axes = FALSE, legend.cex = 3)
plot(worldbounds_3035$geometry,add=T, col= "#F0F0F0")
MPA_polygons = Med_grid_GSA %>%
  filter(MPA == 1)
MPA_polygons_union = st_union(MPA_polygons)
plot(MPA_polygons_union, add = T, col = NA, border = "black", lwd = 2)
box()
dev.off()

#### Plot map of change in fishing effort with fishing strategies --------------
# Uniform effort redistribution by GSA 
Med_grid_GSA_final2 = Med_grid_GSA_final %>%
  mutate(change_uniform = case_when(VALUE == 0 | (new_effort_uniform > 0 & initial_effort > 0)~ (new_effort_uniform - initial_effort)/initial_effort *100,
                                    TRUE ~ 0)) %>%
  mutate(change_prop = case_when(VALUE == 0 | (new_effort_proportional > 0 & initial_effort > 0)~ (new_effort_proportional - initial_effort)/initial_effort *100,
                                 TRUE ~ 0)) %>%
  mutate(diff_uniform = case_when(VALUE == 0 ~ (new_effort_uniform - initial_effort))) %>%
  mutate(diff_prop = case_when(VALUE == 0 ~ (new_effort_proportional - initial_effort)))


Effort_change_uniform = rast(ncol=190, 
                             nrow=83, 
                             xmin=2880851, 
                             xmax=6680851, 
                             ymin=855444, 
                             ymax=2515444)
values(Effort_change_uniform) = Med_grid_GSA_final2$change_uniform #assign raster cell values
col = colorful::directionalPalette(col="firebrick3", p=1)
max(Med_grid_GSA_final2$change_uniform, na.rm = T)
brk = c(0.001,0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1, 1.5, 2, 3,4,5,10,15,20,25,30,40,50)
image.plot(Effort_change_uniform, breaks = brk, col = mycol, axes = FALSE, legend.cex = 3)

fields::image.plot(Effort_change_uniform, col=col, zlim=c(0,50))

Effort_diff_uniform = rast(ncol=190, 
                           nrow=83, 
                           xmin=2880851, 
                           xmax=6680851, 
                           ymin=855444, 
                           ymax=2515444)
values(Effort_diff_uniform) = Med_grid_GSA_final2$diff_uniform #assign raster cell values
col = colorful::directionalPalette(col="firebrick3", p=1)
max = max(Med_grid_GSA_final2$diff_uniform, na.rm = T)
min(Med_grid_GSA_final2$diff_uniform, na.rm = T)
median(Med_grid_GSA_final2$diff_uniform, na.rm = T)

brk = c(0, 0.001, seq(0.05,1,0.05), 6, 7)
mycol = c("white", Reds(n = ncol))
png(here("figures/fishing_effort_maps/diff_uniform.png"), height = 481, width = 994)
image.plot(Effort_diff_uniform, breaks = brk, col = mycol, axes = FALSE, legend.cex = 3)
plot(gsa_outline$geometry, add=T, col= "gray20") 
plot(worldbounds_3035$geometry,add=T, col= "#F0F0F0")
MPA_polygons = Med_grid_GSA %>%
  filter(MPA == 1)
MPA_polygons_union = st_union(MPA_polygons)
plot(MPA_polygons_union, add = T, col = "gray60", border = "black", lwd = 2)
box()
dev.off()

fields::image.plot(Effort_diff_uniform, col=col, zlim=c(0,7))


# Proportional effort redistribution by GSA ------------------------------------

Effort_change_prop = rast(ncol=190, 
                          nrow=83, 
                          xmin=2880851, 
                          xmax=6680851, 
                          ymin=855444, 
                          ymax=2515444)
values(Effort_change_prop) = Med_grid_GSA_final2$change_prop #assign raster cell values
col = colorful::directionalPalette(col="firebrick3", p=1)
image.plot(Effort_change_prop, breaks = brk, col = mycol, axes = FALSE, legend.cex = 3)

fields::image.plot(Effort_change_prop, col=col, zlim=c(0,50))



Effort_diff_prop = rast(ncol=190, 
                        nrow=83, 
                        xmin=2880851, 
                        xmax=6680851, 
                        ymin=855444, 
                        ymax=2515444)
values(Effort_diff_prop) = Med_grid_GSA_final2$diff_prop #assign raster cell values
col = colorful::directionalPalette(col="firebrick3", p=1)
max = max(Med_grid_GSA_final2$diff_prop, na.rm = T)
min(Med_grid_GSA_final2$diff_prop, na.rm = T)
median(Med_grid_GSA_final2$diff_prop, na.rm = T)

brk = c(0, 0.001, seq(0.05,1,0.005), 2,3,4,6,7,15,30,120)
ncol = length(brk) - 2
mycol = c("white", Reds(n = ncol))
png(here("figures/fishing_effort_maps/diff_proportional.png"), height = 481, width = 994)
image.plot(Effort_diff_prop, breaks = brk, col = mycol, axes = FALSE, legend.cex = 3)
plot(gsa_outline$geometry, add=T, col= "gray20") 
plot(worldbounds_3035$geometry,add=T, col= "#F0F0F0")
MPA_polygons = Med_grid_GSA %>%
  filter(MPA == 1)
MPA_polygons_union = st_union(MPA_polygons)
plot(MPA_polygons_union, add = T, col = "gray60", border = "black", lwd = 2)
box()
dev.off()

                    