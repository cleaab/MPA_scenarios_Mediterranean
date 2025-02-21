#' Script to create MPA scenarios masks
#'
#' 
#' @param data QGIS shqpefiles of overlap between OSMOSE-MED grid and scenarios from the scientific literature (S4-S6) and current MPA location (S1)
#' 
#' @return csv files indicating MPA placement (cell = 1)
#'
#' @export
#' 

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
#library(spDataLarge)
library(stars)
library(qgisprocess)
library(ggplot2)
library(polynom) #to solve polynomial equation
library(here)
set.seed(123)

# Functions --------------------------------------------------------------------

# Gets indices cells adjacent to MPAs - 8 ways
get.adjacent.indices.8ways.fn <- function(mpa.ind, ncol, nrow){ 
  
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

# Gets indices cells adjacent to MPAs - 4 ways
get.adjacent.indices.4ways.fn <- function(mpa.ind, ncol, nrow){ 
  
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
    
    n = n + 1
  }
  adjacent_ind = cbind(adjacent_indices_row, adjacent_indices_col)
  return(adjacent_ind)
}

# New cells = cells most chosen by plans and then 4-way expansion
create.mpa.mask.literature.fn <- function(output.Dir, pas, count_cells, Max_cells, Med_grid, Max_cells_plans, cells_by_step, Intersection, scenario_name){
  
  Med_grid$Plans_coverage = Intersection$Overlap_percentage
  Med_grid$cell_ID = row.names(Med_grid)
  
  # Initialisation - graines AMPs
  
  Init_cells = Intersection$fid[Intersection$Cells_38 == 2]
  
  MPA_cells = Init_cells
  Med_grid$VALUE[MPA_cells] = 1
  count_cells = length(Init_cells)
  graine_AMP_cells = Init_cells
  
  if (count_cells %% cells_by_step == 0){
    # Plot
    Med_grid$color[Med_grid$VALUE == -99] = "white"
    Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
    Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
    Med_grid$color[graine_AMP_cells] = "orange"
    
    print(round((count_cells)/6229*100,2))
    
    plot <- ggplot() +
      geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
      geom_sf(data = EEZ_MedSea_only_3035_union, fill = NA, colour = "black") +
      #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
      theme(panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "#000000", fill=NA, size=1),
            plot.title = element_text(family = "Helvetica", size = (12)))  +
      labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
    
    plot
    ggsave(paste0(output.Dir, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
    
    # Print csv
    MPA_rast = rast(ncol=190, 
                    nrow=83, 
                    xmin=2880851, 
                    xmax=6680851, 
                    ymin=855444, 
                    ymax=2515444)
    values(MPA_rast) = Med_grid$VALUE #assign raster cell values
    plot(MPA_rast)
    
    # Convert raster to csv file
    write.table(as.array(MPA_rast),
                paste0(output.Dir, "/", count_cells/(pas*cells_by_step), ".csv"),
                row.names = FALSE,
                col.names=FALSE,
                sep=",")
    
  }
  
  # Nouvelles cellules - recouvertes par une AMP existante qqsoit le % et d'abord sélection des cellules avec couverture par AMP plus importante 
  while (count_cells >= cells_by_step & count_cells < Max_cells_plans){ #Max_cells_plans = 971
    
    potential_cells = Med_grid$cell_ID[Med_grid$VALUE == 0]
    
    cov_potential_cells = Med_grid$Plans_coverage[Med_grid$cell_ID %in% potential_cells]
    table(cov_potential_cells)
    print(max(cov_potential_cells))
    
    Candidate_MPA_cells = Med_grid$cell_ID[Med_grid$Plans_coverage == max(cov_potential_cells) & Med_grid$VALUE == 0]
    print(Candidate_MPA_cells)
    if (length(Candidate_MPA_cells) > 1){
      MPA_cells = sample(Candidate_MPA_cells, pas, replace = FALSE)
      print(MPA_cells)
    } else{
      MPA_cells = Candidate_MPA_cells
      print(MPA_cells)
      
    }
    
    Med_grid$VALUE[Med_grid$cell_ID == MPA_cells] = 1
    print(table(Med_grid$VALUE))
    table(Med_grid$VALUE)
    count_cells = count_cells + pas
    
    if (count_cells %% cells_by_step == 0){
      # Plot
      Med_grid$color[Med_grid$VALUE == -99] = "white"
      Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
      Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
      Med_grid$color[graine_AMP_cells] = "orange"
      
      print(round((count_cells)/6229*100,2))
      
      plot <- ggplot() +
        geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
        geom_sf(data = EEZ_MedSea_only_3035_union, fill = NA, colour = "black") +
        #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "#000000", fill=NA, size=1),
              plot.title = element_text(family = "Helvetica", size = (12)))  +
        labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
      
      plot
      ggsave(paste0(output.Dir, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
      
      # Print csv
      MPA_rast = rast(ncol=190, 
                      nrow=83, 
                      xmin=2880851, 
                      xmax=6680851, 
                      ymin=855444, 
                      ymax=2515444)
      values(MPA_rast) = Med_grid$VALUE #assign raster cell values
      plot(MPA_rast)
      
      # Convert raster to csv file
      write.table(as.array(MPA_rast),
                  paste0(output.Dir, "/", count_cells/(pas*cells_by_step), ".csv"),
                  row.names = FALSE,
                  col.names=FALSE,
                  sep=",")
      
    }
    
  }
  
  while (count_cells >= Max_cells_plans & count_cells < Max_cells){ # Expansion 4-way
    
    # calculate neighbouring cells of already selected MPAs in 4 directions
    Med_matrix = matrix(Med_grid$VALUE, ncol = 190, nrow = 83, byrow= TRUE) # convert list of cell id to matrix
    mpa.ind = as.data.frame(which(Med_matrix == 1, arr.ind = TRUE))
    adjacent_ind = get.adjacent.indices.4ways.fn(mpa.ind, 190, 83)
    adjacent_ind_df = as.data.frame(adjacent_ind)
    adjacent_cell_id = adjacent_ind_df$adjacent_indices_col + 190* (adjacent_ind_df$adjacent_indices_row - 1) 
    
    # potential cells to sample from (which are neighbours of already selected MPAs)
    potential_cells = Med_grid$cell_ID[Med_grid$VALUE == 0] # verifies that MPA cell = 0 before selection
    Candidate_MPA_cells = adjacent_cell_id[adjacent_cell_id %in% potential_cells == TRUE] # to have repetitions --> that way more chances of selecting cells with many neighbors 
    
    if (length(Candidate_MPA_cells) > 1){
      MPA_cells = sample(Candidate_MPA_cells, pas, replace = FALSE)
    } else{
      MPA_cells = Candidate_MPA_cells
    }
    
    Med_grid$VALUE[Med_grid$cell_ID == MPA_cells] = 1
    print(table(Med_grid$VALUE))
    count_cells = count_cells + pas
    
    if (count_cells %% cells_by_step == 0){
      # Plot
      Med_grid$color[Med_grid$VALUE == -99] = "white"
      Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
      Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
      Med_grid$color[graine_AMP_cells] = "orange"
      
      print(round((count_cells)/6229*100,2))
      
      plot <- ggplot() +
        geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
        geom_sf(data = EEZ_MedSea_only_3035_union, fill = NA, colour = "black") +
        #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "#000000", fill=NA, size=1),
              plot.title = element_text(family = "Helvetica", size = (12)))  +
        labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
      
      plot
      ggsave(paste0(output.Dir, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
      
      # Print csv
      MPA_rast = rast(ncol=190, 
                      nrow=83, 
                      xmin=2880851, 
                      xmax=6680851, 
                      ymin=855444, 
                      ymax=2515444)
      values(MPA_rast) = Med_grid$VALUE #assign raster cell values
      plot(MPA_rast)
      
      # Convert raster to csv file
      write.table(as.array(MPA_rast),
                  paste0(output.Dir, "/", count_cells/(pas*cells_by_step), ".csv"),
                  row.names = FALSE,
                  col.names=FALSE,
                  sep=",")
      
    }
    
  }
  
}

# Good one - number of clusters = max nb of MPAs, expansion of MPAs corresponds to coverage % of each EEZ
create.mpa.mask.randomEEZ.fn <- function(output.Dir, pas, count_cells, pct_coverage, Med_grid, EEZ_MedSea, cells_by_step, Max_nb_of_MPAs, MPA_cell_size, percentage_of_EEZ_target, graine_AMP, rep, scenario_name){
  
  EEZ_MedSea_3035 = st_transform(EEZ_MedSea, 3035)
  EEZ_MedSea_only_3035 = st_intersection(EEZ_MedSea_3035,Med_grid_union)
  EEZ_MedSea_only_3035$Area_km2 = set_units(st_area(EEZ_MedSea_only_3035), km^2)
  
  Candidates_EEZ = EEZ_MedSea_only_3035 %>%
    filter(POL_TYPE == "200NM") %>%
    drop_units() %>%
    filter(Area_km2 * percentage_of_EEZ_target > MPA_cell_size) %>% # filter EEZ that are too small
    mutate(Max_MPA_by_ZEE = Area_km2 * percentage_of_EEZ_target) %>%
    mutate(nb_of_cells_by_ZEE = Area_km2 * percentage_of_EEZ_target / 400)
  
  # Plot candidate EEZs
  # EEZ_MedSea_only_3035$color[!EEZ_MedSea_only_3035$MRGID %in% Candidates_EEZ$MRGID] = "red"
  # plot <- ggplot() +
  #   geom_sf(data = EEZ_MedSea_only_3035$geometry, fill = EEZ_MedSea_only_3035$color, colour = "black") +
  #   #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
  #   theme(panel.background = element_rect(fill = "white"),
  #         panel.border = element_rect(colour = "#000000", fill=NA, size=1),
  #         plot.title = element_text(family = "Helvetica", size = (12)))
  # 
  # plot
  # 
  Med_grid_EEZ = st_intersection(Med_grid,Candidates_EEZ)
  
  Med_grid_EEZ_clean = Med_grid_EEZ %>%
    dplyr::select(1:10, 35:36) %>%
    mutate(Area_km2 = set_units(st_area(geom), km^2)) %>%
    drop_units() %>%
    filter(VALUE == 0)%>%
    st_drop_geometry()%>%
    filter(Area_km2 == MPA_cell_size)
  
  EEZ_by_country = Candidates_EEZ %>%
    st_drop_geometry() %>%
    dplyr::select(c(1:7, 32)) %>%
    mutate(Area_protected = 0) %>%
    mutate(Area_MedSea_km2 = 6229*400) %>%
    mutate(Nb_of_cells_by_pct_step= 0) %>%
    mutate(MPA_in_country= FALSE)
  
  graine_AMP_cells = c()
  
  for (pct in pct_coverage){
    EEZ_by_country$Nb_of_cells_by_pct_step = round(EEZ_by_country$Area_km2 * pct/400) - round(EEZ_by_country$Area_km2 * (pct-0.01)/400) # calculate nb of cells to reach 1% of the ZEE in no-take area 
    EEZ_by_country$Area_protected = 0 # set area protected to 0 after changing % coverage 
    sum(EEZ_by_country$Nb_of_cells_by_pct_step)
    
    for (i in EEZ_by_country$MRGID){
      if (EEZ_by_country$Nb_of_cells_by_pct_step[EEZ_by_country$MRGID == i] > 0){ # if candidate ZEE (i.e, ZEE that has space for implementing MPAs)
        
        while(EEZ_by_country$Area_protected[EEZ_by_country$MRGID == i] < EEZ_by_country$Nb_of_cells_by_pct_step[EEZ_by_country$MRGID == i]*MPA_cell_size){ # while MPA coverage < % coverage objective
          
          if (EEZ_by_country$MPA_in_country[EEZ_by_country$MRGID == i] == FALSE){ # initialization (1st MPA cell or 1st MPA cell in a given ZEE)
            
            # Check that MPA initial cluster cells do not touch
            Candidate_MPA_cells = Med_grid_EEZ_clean$cell_ID[Med_grid_EEZ_clean$MRGID == i & Med_grid_EEZ_clean$VALUE == 0] # verifies that MPA cell = 0 before selection
            
            if (length(Candidate_MPA_cells) > 1){
              MPA_cells = sample(Candidate_MPA_cells, pas, replace = FALSE)
            } else{
              MPA_cells = Candidate_MPA_cells
            }
            
            Med_grid_EEZ_clean$VALUE[Med_grid_EEZ_clean$cell_ID %in% MPA_cells] = 1
            Med_grid$VALUE[MPA_cells] = 1
            EEZ_by_country$Area_protected[EEZ_by_country$MRGID == i] = MPA_cell_size + EEZ_by_country$Area_protected[EEZ_by_country$MRGID == i]
            count_cells = count_cells + pas
            graine_AMP = graine_AMP + pas
            graine_AMP_cells = c(graine_AMP_cells, MPA_cells)
            
            table(Med_grid$VALUE)
            EEZ_by_country$MPA_in_country[EEZ_by_country$MRGID == i] = TRUE # at least one MPA has been implented in the country's ZEE
            
            if (count_cells %% cells_by_step == 0){
              # Plot
              Med_grid$color[Med_grid$VALUE == -99] = "white"
              Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
              Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
              Med_grid$color[graine_AMP_cells] = "orange"
              
              print(round((count_cells)/6229*100,2))
              
              plot <- ggplot() +
                geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
                geom_sf(data = EEZ_MedSea_only_3035$geometry, fill = NA, colour = "black") +
                #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
                theme(panel.background = element_rect(fill = "white"),
                      panel.border = element_rect(colour = "#000000", fill=NA, size=1),
                      plot.title = element_text(family = "Helvetica", size = (12)))  +
                labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
              
              plot
              ggsave(paste0(output.Dir, "/rep", rep, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
              
              # Print csv
              MPA_rast = rast(ncol=190, 
                              nrow=83, 
                              xmin=2880851, 
                              xmax=6680851, 
                              ymin=855444, 
                              ymax=2515444)
              values(MPA_rast) = Med_grid$VALUE #assign raster cell values
              plot(MPA_rast)
              
              # Convert raster to csv file
              write.table(as.array(MPA_rast),
                          paste0(output.Dir, "/rep", rep, "/", count_cells/(pas*cells_by_step), ".csv"),
                          row.names = FALSE,
                          col.names=FALSE,
                          sep=",")
              
            }
            
          } 
          
          else if (EEZ_by_country$MPA_in_country[EEZ_by_country$MRGID == i] & graine_AMP + sum(EEZ_by_country$MPA_in_country == FALSE) < Max_nb_of_MPAs){ # add new clusters in EEZs that already have at least an MPA
            
            # potential cells to sample from (which are not neighbors of already selected MPAs)
            potential_cells = Med_grid_EEZ_clean$cell_ID[Med_grid_EEZ_clean$MRGID == i & Med_grid_EEZ_clean$VALUE == 0] # verifies that MPA cell = 0 before selection
            Med_matrix = matrix(Med_grid$VALUE, ncol = 190, nrow = 83, byrow= TRUE) # convert list of cell id to matrix
            mpa.ind = as.data.frame(which(Med_matrix == 1, arr.ind = TRUE))
            adjacent_ind = get.adjacent.indices.8ways.fn(mpa.ind, 190, 83)
            adjacent_ind_df = as.data.frame(adjacent_ind)
            adjacent_cell_id = adjacent_ind_df$adjacent_indices_col + 190* (adjacent_ind_df$adjacent_indices_row - 1) 
            
            Candidate_MPA_cells = setdiff(potential_cells, adjacent_cell_id)
            
            if (length(Candidate_MPA_cells) > 1){
              MPA_cells = sample(Candidate_MPA_cells, pas, replace = FALSE)
            } else{
              MPA_cells = Candidate_MPA_cells
              
            }
            
            Med_grid_EEZ_clean$VALUE[Med_grid_EEZ_clean$cell_ID %in% MPA_cells] = 1
            Med_grid$VALUE[MPA_cells] = 1
            EEZ_by_country$Area_protected[EEZ_by_country$MRGID == i] = MPA_cell_size + EEZ_by_country$Area_protected[EEZ_by_country$MRGID == i]
            count_cells = count_cells + pas
            graine_AMP = graine_AMP + pas
            graine_AMP_cells = c(graine_AMP_cells, MPA_cells)
            
            if (count_cells %% cells_by_step == 0){
              # Plot
              Med_grid$color[Med_grid$VALUE == -99] = "white"
              Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
              Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
              Med_grid$color[graine_AMP_cells] = "orange"
              
              
              print(round((count_cells)/6229*100,2))
              
              plot <- ggplot() +
                geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
                geom_sf(data = EEZ_MedSea_only_3035$geometry, fill = NA, colour = "black") +
                #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
                theme(panel.background = element_rect(fill = "white"),
                      panel.border = element_rect(colour = "#000000", fill=NA, size=1),
                      plot.title = element_text(family = "Helvetica", size = (12)))  +
                labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
              
              plot
              ggsave(paste0(output.Dir, "/rep", rep, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
              
              # Print csv
              MPA_rast = rast(ncol=190, 
                              nrow=83, 
                              xmin=2880851, 
                              xmax=6680851, 
                              ymin=855444, 
                              ymax=2515444)
              values(MPA_rast) = Med_grid$VALUE #assign raster cell values
              plot(MPA_rast)
              
              # Convert raster to csv file
              write.table(as.array(MPA_rast),
                          paste0(output.Dir, "/rep", rep, "/", count_cells/(pas*cells_by_step), ".csv"),
                          row.names = FALSE,
                          col.names=FALSE,
                          sep=",")
              
            }
            
          } 
          
          else if (EEZ_by_country$MPA_in_country[EEZ_by_country$MRGID == i] & graine_AMP + sum(EEZ_by_country$MPA_in_country == FALSE) >= Max_nb_of_MPAs || graine_AMP == Max_nb_of_MPAs){ # If already all clusters distributed : Expansion of MPAs 4-ways
            # If not all clusters distributed but no room for more in EEZs that already have MPAs : Expansion of MPAs 4-way
            Med_matrix = matrix(Med_grid$VALUE, ncol = 190, nrow = 83, byrow= TRUE) # convert list of cell id to matrix
            mpa.ind = as.data.frame(which(Med_matrix == 1, arr.ind = TRUE))
            adjacent_ind = get.adjacent.indices.4ways.fn(mpa.ind, 190, 83)
            adjacent_ind_df = as.data.frame(adjacent_ind)
            adjacent_cell_id = adjacent_ind_df$adjacent_indices_col + 190* (adjacent_ind_df$adjacent_indices_row - 1) 
            
            # potential cells to sample from (which are neighbours of already selected MPAs)
            potential_cells = Med_grid_EEZ_clean$cell_ID[Med_grid_EEZ_clean$MRGID == i & Med_grid_EEZ_clean$VALUE == 0] # verifies that MPA cell = 0 before selection
            Candidate_MPA_cells = adjacent_cell_id[adjacent_cell_id %in% potential_cells == TRUE] # to have repetitions --> that way more chances of selecting cells with many neighbors 
            #Candidate_MPA_cells = intersect(adjacent_cell_id, potential_cells) # to not have repetitions
            
            #length(unique(Candidate_MPA_cells)) # 139 not duplicates
            #sum(duplicated(Candidate_MPA_cells)) # 56 duplicates
            
            if (length(Candidate_MPA_cells) > 1){
              MPA_cells = sample(Candidate_MPA_cells, pas, replace = FALSE)
            } else{
              MPA_cells = Candidate_MPA_cells
            }
            
            Med_grid_EEZ_clean$VALUE[Med_grid_EEZ_clean$cell_ID %in% MPA_cells] = 1
            Med_grid$VALUE[MPA_cells] = 1
            EEZ_by_country$Area_protected[EEZ_by_country$MRGID == i] = MPA_cell_size + EEZ_by_country$Area_protected[EEZ_by_country$MRGID == i]
            count_cells = count_cells + pas
            
            if (count_cells %% cells_by_step == 0){
              # Plot
              Med_grid$color[Med_grid$VALUE == -99] = "white"
              Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
              Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
              Med_grid$color[graine_AMP_cells] = "orange"
              
              print(round((count_cells)/6229*100,2))
              
              plot <- ggplot() +
                geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
                geom_sf(data = EEZ_MedSea_only_3035$geometry, fill = NA, colour = "black") +
                #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
                theme(panel.background = element_rect(fill = "white"),
                      panel.border = element_rect(colour = "#000000", fill=NA, size=1),
                      plot.title = element_text(family = "Helvetica", size = (12)))  +
                labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
              
              plot
              ggsave(paste0(output.Dir, "/rep", rep, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
              
              # Print csv
              MPA_rast = rast(ncol=190, 
                              nrow=83, 
                              xmin=2880851, 
                              xmax=6680851, 
                              ymin=855444, 
                              ymax=2515444)
              values(MPA_rast) = Med_grid$VALUE #assign raster cell values
              plot(MPA_rast)
              
              # Convert raster to csv file
              write.table(as.array(MPA_rast),
                          paste0(output.Dir, "/rep", rep, "/", count_cells/(pas*cells_by_step), ".csv"),
                          row.names = FALSE,
                          col.names=FALSE,
                          sep=",")
              
            }
            
          }
          
          
        }
      }
    }
    
  }
}

# Random MPA scenario - 4 way expansion of clusters - fusion between clusters possible
create.mpa.mask.random.4way.expansion.fn <- function(output.Dir, pas, count_cells, Max_cells, rep, Med_grid, cells_by_step, Nb_of_MPA_clusters, scenario_name){
  
  Med_grid$cell_ID = as.numeric(rownames(Med_grid))
  graine_AMP_cells = c()
  
  # Distribute MPA initial cluster cells randomly among Med grid sea cells
  while(count_cells < Nb_of_MPA_clusters){
    
    if (count_cells == 0){
      Candidate_MPA_cells = Med_grid$cell_ID[Med_grid$VALUE == 0]
    } else{
      # check that new MPA cluster is not a neighbor of already selected MPAs
      potential_cells = Med_grid$cell_ID[Med_grid$VALUE == 0] # verifies that MPA cell = 0 before selection
      
      Med_matrix = matrix(Med_grid$VALUE, ncol = 190, nrow = 83, byrow= TRUE) # convert list of cell id to matrix
      mpa.ind = as.data.frame(which(Med_matrix == 1, arr.ind = TRUE))
      adjacent_ind = get.adjacent.indices.8ways.fn(mpa.ind, 190, 83)
      adjacent_ind_df = as.data.frame(adjacent_ind)
      adjacent_cell_id = adjacent_ind_df$adjacent_indices_col + 190* (adjacent_ind_df$adjacent_indices_row - 1) 
      
      Candidate_MPA_cells = setdiff(potential_cells, adjacent_cell_id)
    }
    
    if (length(Candidate_MPA_cells) > 1){
      MPA_cells = sample(Candidate_MPA_cells, pas, replace = FALSE)
    } else{
      MPA_cells = Candidate_MPA_cells
    }
    
    Med_grid$VALUE[MPA_cells] = 1
    print(table(Med_grid$VALUE))
    count_cells = count_cells + pas
    graine_AMP_cells = c(graine_AMP_cells, MPA_cells)
    
    if (count_cells %% cells_by_step == 0){
      # Plot
      Med_grid$color[Med_grid$VALUE == -99] = "white"
      Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
      Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
      Med_grid$color[graine_AMP_cells] = "orange"
      
      print(round((count_cells)/6229*100,2))
      
      plot <- ggplot() +
        
        geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
        geom_sf(data = EEZ_MedSea_only_3035_union, fill = NA, colour = "black") +
        
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "#000000", fill=NA, size=1),
              plot.title = element_text(family = "Helvetica", size = (12)))  +
        labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
      
      plot
      
      ggsave(paste0(output.Dir, "/rep", rep, "/Plots/plot", count_cells/(pas * cells_by_step), ".jpg"), plot)
      
      # Print csv
      MPA_rast = rast(ncol=190, 
                      nrow=83, 
                      xmin=2880851, 
                      xmax=6680851, 
                      ymin=855444, 
                      ymax=2515444)
      values(MPA_rast) = Med_grid$VALUE #assign raster cell values
      plot(MPA_rast)
      
      # Save shapefile
      st_write(Med_grid, paste0(output.Dir, "/rep", rep, "/", count_cells/(pas * cells_by_step), ".shp"))
      
      # Convert raster to csv file
      write.table(as.array(MPA_rast),
                  paste0(output.Dir, "/rep", rep, "/", count_cells/(pas*cells_by_step), ".csv"),
                  row.names = FALSE,
                  col.names=FALSE,
                  sep=",")
      
    }
  }
  
  # Expansion of MPAs to reach 30% of coverage
  while(Nb_of_MPA_clusters <= count_cells & count_cells < Max_cells){
    
    # calculate neighbouring cells of already selected MPAs in 4 directions
    Med_matrix = matrix(Med_grid$VALUE, ncol = 190, nrow = 83, byrow= TRUE) # convert list of cell id to matrix
    mpa.ind = as.data.frame(which(Med_matrix == 1, arr.ind = TRUE))
    adjacent_ind = get.adjacent.indices.4ways.fn(mpa.ind, 190, 83)
    adjacent_ind_df = as.data.frame(adjacent_ind)
    adjacent_cell_id = adjacent_ind_df$adjacent_indices_col + 190* (adjacent_ind_df$adjacent_indices_row - 1) 
    
    # potential cells to sample from (which are neighbours of already selected MPAs)
    potential_cells = Med_grid$cell_ID[Med_grid$VALUE == 0] # verifies that MPA cell = 0 before selection
    Candidate_MPA_cells = adjacent_cell_id[adjacent_cell_id %in% potential_cells == TRUE] # to have repetitions --> that way more chances of selecting cells with many neighbors 
    
    if (length(Candidate_MPA_cells) > 1){
      MPA_cells = sample(Candidate_MPA_cells, pas, replace = FALSE)
    } else{
      MPA_cells = Candidate_MPA_cells
    }
    
    Med_grid$VALUE[MPA_cells] = 1
    print(table(Med_grid$VALUE))
    count_cells = count_cells + pas
    
    if (count_cells %% cells_by_step == 0){
      # Plot
      Med_grid$color[Med_grid$VALUE == -99] = "white"
      Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
      Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
      Med_grid$color[graine_AMP_cells] = "orange"
      
      print(round((count_cells)/6229*100,2))
      
      plot <- ggplot() +
        geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
        geom_sf(data = EEZ_MedSea_only_3035_union, fill = NA, colour = "black") +
        
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "#000000", fill=NA, size=1),
              plot.title = element_text(family = "Helvetica", size = (12)))  +
        labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
      
      plot
      ggsave(paste0(output.Dir, "/rep", rep, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
      
      # Print csv
      MPA_rast = rast(ncol=190, 
                      nrow=83, 
                      xmin=2880851, 
                      xmax=6680851, 
                      ymin=855444, 
                      ymax=2515444)
      values(MPA_rast) = Med_grid$VALUE #assign raster cell values
      plot(MPA_rast)
      
      # Convert raster to csv file
      write.table(as.array(MPA_rast),
                  paste0(output.Dir, "/rep", rep, "/", count_cells/(pas*cells_by_step), ".csv"),
                  row.names = FALSE,
                  col.names=FALSE,
                  sep=",")
      
    }
  }
  
}

# Mazor -scenario 9 and 8
create.mpa.mask.Mazor.scenarios.fn <- function(output.Dir, pas, count_cells, Max_cells, Med_grid, Max_cells_plans, cells_by_step, Intersection, scenario_name){
  
  Med_grid$Plans_coverage = Intersection$Overlap_percentage
  Med_grid$cell_ID = row.names(Med_grid)
  
  # Initialisation - graines AMPs
  
  Init_cells = Intersection$fid[Intersection$Cells_38 == 2]
  
  MPA_cells = Init_cells
  Med_grid$VALUE[MPA_cells] = 1
  count_cells = length(Init_cells)
  graine_AMP_cells = Init_cells
  
  if (count_cells %% cells_by_step == 0){
    # Plot
    Med_grid$color[Med_grid$VALUE == -99] = "white"
    Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
    Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
    Med_grid$color[graine_AMP_cells] = "orange"
    
    print(round((count_cells)/6229*100,2))
    
    plot <- ggplot() +
      geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
      geom_sf(data = EEZ_MedSea_only_3035_union, fill = NA, colour = "black") +
      #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
      theme(panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "#000000", fill=NA, size=1),
            plot.title = element_text(family = "Helvetica", size = (12)))  +
      labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
    
    plot
    ggsave(paste0(output.Dir, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
    
    # Print csv
    MPA_rast = rast(ncol=190, 
                    nrow=83, 
                    xmin=2880851, 
                    xmax=6680851, 
                    ymin=855444, 
                    ymax=2515444)
    values(MPA_rast) = Med_grid$VALUE #assign raster cell values
    plot(MPA_rast)
    
    # Convert raster to csv file
    write.table(as.array(MPA_rast),
                paste0(output.Dir, "/", count_cells/(pas*cells_by_step), ".csv"),
                row.names = FALSE,
                col.names=FALSE,
                sep=",")
    
  }
  
  # Nouvelles cellules - recouvertes par une AMP existante qqsoit le % et d'abord sélection des cellules avec couverture par AMP plus importante 
  while (count_cells >= cells_by_step & count_cells < Max_cells_plans){ #Max_cells_plans = 971
    
    potential_cells = Med_grid$cell_ID[Med_grid$VALUE == 0]
    
    cov_potential_cells = Med_grid$Plans_coverage[Med_grid$cell_ID %in% potential_cells]
    table(cov_potential_cells)
    print(max(cov_potential_cells))
    
    Candidate_MPA_cells = Med_grid$cell_ID[Med_grid$Plans_coverage == max(cov_potential_cells) & Med_grid$VALUE == 0]
    print(Candidate_MPA_cells)
    if (length(Candidate_MPA_cells) > 1){
      MPA_cells = sample(Candidate_MPA_cells, pas, replace = FALSE)
      print(MPA_cells)
    } else{
      MPA_cells = Candidate_MPA_cells
      print(MPA_cells)
      
    }
    
    Med_grid$VALUE[Med_grid$cell_ID == MPA_cells] = 1
    print(table(Med_grid$VALUE))
    table(Med_grid$VALUE)
    count_cells = count_cells + pas
    
    if (count_cells %% cells_by_step == 0){
      # Plot
      Med_grid$color[Med_grid$VALUE == -99] = "white"
      Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
      Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
      Med_grid$color[graine_AMP_cells] = "orange"
      
      print(round((count_cells)/6229*100,2))
      
      plot <- ggplot() +
        geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
        geom_sf(data = EEZ_MedSea_only_3035_union, fill = NA, colour = "black") +
        #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "#000000", fill=NA, size=1),
              plot.title = element_text(family = "Helvetica", size = (12)))  +
        labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
      
      plot
      ggsave(paste0(output.Dir, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
      
      # Print csv
      MPA_rast = rast(ncol=190, 
                      nrow=83, 
                      xmin=2880851, 
                      xmax=6680851, 
                      ymin=855444, 
                      ymax=2515444)
      values(MPA_rast) = Med_grid$VALUE #assign raster cell values
      plot(MPA_rast)
      
      # Convert raster to csv file
      write.table(as.array(MPA_rast),
                  paste0(output.Dir, "/", count_cells/(pas*cells_by_step), ".csv"),
                  row.names = FALSE,
                  col.names=FALSE,
                  sep=",")
      
    }
    
  }
  
  while (count_cells >= Max_cells_plans & count_cells < Max_cells){ # Expansion 4-way
    
    # calculate neighbouring cells of already selected MPAs in 4 directions
    Med_matrix = matrix(Med_grid$VALUE, ncol = 190, nrow = 83, byrow= TRUE) # convert list of cell id to matrix
    mpa.ind = as.data.frame(which(Med_matrix == 1, arr.ind = TRUE))
    adjacent_ind = get.adjacent.indices.4ways.fn(mpa.ind, 190, 83)
    adjacent_ind_df = as.data.frame(adjacent_ind)
    adjacent_cell_id = adjacent_ind_df$adjacent_indices_col + 190* (adjacent_ind_df$adjacent_indices_row - 1) 
    
    # potential cells to sample from (which are neighbours of already selected MPAs)
    potential_cells = Med_grid$cell_ID[Med_grid$VALUE == 0] # verifies that MPA cell = 0 before selection
    Candidate_MPA_cells = adjacent_cell_id[adjacent_cell_id %in% potential_cells == TRUE] # to have repetitions --> that way more chances of selecting cells with many neighbors 
    
    if (length(Candidate_MPA_cells) > 1){
      MPA_cells = sample(Candidate_MPA_cells, pas, replace = FALSE)
    } else{
      MPA_cells = Candidate_MPA_cells
    }
    
    Med_grid$VALUE[Med_grid$cell_ID == MPA_cells] = 1
    print(table(Med_grid$VALUE))
    count_cells = count_cells + pas
    
    if (count_cells %% cells_by_step == 0){
      # Plot
      Med_grid$color[Med_grid$VALUE == -99] = "white"
      Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
      Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
      Med_grid$color[graine_AMP_cells] = "orange"
      
      print(round((count_cells)/6229*100,2))
      
      plot <- ggplot() +
        geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
        geom_sf(data = EEZ_MedSea_only_3035_union, fill = NA, colour = "black") +
        #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "#000000", fill=NA, size=1),
              plot.title = element_text(family = "Helvetica", size = (12)))  +
        labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
      
      plot
      ggsave(paste0(output.Dir, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
      
      # Print csv
      MPA_rast = rast(ncol=190, 
                      nrow=83, 
                      xmin=2880851, 
                      xmax=6680851, 
                      ymin=855444, 
                      ymax=2515444)
      values(MPA_rast) = Med_grid$VALUE #assign raster cell values
      plot(MPA_rast)
      
      # Convert raster to csv file
      write.table(as.array(MPA_rast),
                  paste0(output.Dir, "/", count_cells/(pas*cells_by_step), ".csv"),
                  row.names = FALSE,
                  col.names=FALSE,
                  sep=",")
      
    }
    
  }
  
}

# Micheli
create.mpa.mask.Micheli.fn <- function(output.Dir, pas, count_cells, Max_cells, Med_grid, Max_cells_plans_1, Max_cells_plans_2, cells_by_step, Number_of_clusters, Overlap_Medgrid_Plans10pct, Overlap_Medgrid_Plans30pct, scenario_name){
  
  # 1 - Increase MPA up to 10% following priority conservation areas in Micheli et al. 2013 - cells chosen manually in QGIS 
  for (i in 1:10){
    MPA_cells = Overlap_Medgrid_Plans10pct$fid[Overlap_Medgrid_Plans10pct$Cells_step == i]
    length(MPA_cells)
    
    Med_grid$VALUE[MPA_cells] = 1
    count_cells = count_cells + cells_by_step
    
    
    if (count_cells %% cells_by_step == 0){
      
      # Plot
      Med_grid$color[Med_grid$VALUE == -99] = "white"
      Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
      Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
      
      print(round((count_cells)/6229*100,2))
      
      plot <- ggplot() +
        geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
        geom_sf(data = EEZ_MedSea_only_3035_union, fill = NA, colour = "black") +
        #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "#000000", fill=NA, size=1),
              plot.title = element_text(family = "Helvetica", size = (12)))  +
        labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
      
      plot
      ggsave(paste0(output.Dir, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
      
      # Print csv
      MPA_rast = rast(ncol=190, 
                      nrow=83, 
                      xmin=2880851, 
                      xmax=6680851, 
                      ymin=855444, 
                      ymax=2515444)
      values(MPA_rast) = Med_grid$VALUE #assign raster cell values
      plot(MPA_rast)
      
      # Convert raster to csv file
      write.table(as.array(MPA_rast),
                  paste0(output.Dir, "/", count_cells/(pas*cells_by_step), ".csv"),
                  row.names = FALSE,
                  col.names=FALSE,
                  sep=",")
      
    }
    
  }
  
  # 2 - Increase MPAs following ~20% conservation plan - selecting first cells with highest coverage
  Med_grid$Plans_coverage = Overlap_Medgrid_Plans30pct$Overlap_percentage
  Med_grid$cell_ID = row.names(Med_grid)
  
  while (count_cells >= cells_by_step*10 & count_cells < Max_cells_plans){ #Max_cells_plans = 
    
    potential_cells = Med_grid$cell_ID[Med_grid$VALUE == 0]
    
    cov_potential_cells = Med_grid$Plans_coverage[Med_grid$cell_ID %in% potential_cells]
    table(cov_potential_cells)
    print(max(cov_potential_cells))
    
    Candidate_MPA_cells = Med_grid$cell_ID[Med_grid$Plans_coverage == max(cov_potential_cells) & Med_grid$VALUE == 0]
    print(Candidate_MPA_cells)
    if (length(Candidate_MPA_cells) > 1){
      MPA_cells = sample(Candidate_MPA_cells, pas, replace = FALSE)
      print(MPA_cells)
    } else{
      MPA_cells = Candidate_MPA_cells
      print(MPA_cells)
      
    }
    
    Med_grid$VALUE[Med_grid$cell_ID == MPA_cells] = 1
    print(table(Med_grid$VALUE))
    table(Med_grid$VALUE)
    count_cells = count_cells + pas
    
    if (count_cells %% cells_by_step == 0){
      # Plot
      Med_grid$color[Med_grid$VALUE == -99] = "white"
      Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
      Med_grid$color[Med_grid$VALUE == 1] = "darkblue"
      
      print(round((count_cells)/6229*100,2))
      
      plot <- ggplot() +
        geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
        geom_sf(data = EEZ_MedSea_only_3035_union, fill = NA, colour = "black") +
        #geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(colour = "#000000", fill=NA, size=1),
              plot.title = element_text(family = "Helvetica", size = (12)))  +
        labs(title = paste0("Scenario: ", scenario_name, ": ", round((count_cells)/6229*100,2), "%"))
      
      plot
      ggsave(paste0(output.Dir, "/Plots/plot", count_cells/(pas*cells_by_step), ".jpg"), plot)
      
      # Print csv
      MPA_rast = rast(ncol=190, 
                      nrow=83, 
                      xmin=2880851, 
                      xmax=6680851, 
                      ymin=855444, 
                      ymax=2515444)
      values(MPA_rast) = Med_grid$VALUE #assign raster cell values
      plot(MPA_rast)
      
      # Convert raster to csv file
      write.table(as.array(MPA_rast),
                  paste0(output.Dir, "/", count_cells/(pas*cells_by_step), ".csv"),
                  row.names = FALSE,
                  col.names=FALSE,
                  sep=",")
      
    }
    
  }
  
}


##################################  RUN ########################################

# load data
Med_grid = st_read(here("data/MPA_scenarios/QGIS/Med_polygon.gpkg"))
Med_grid_sea = subset(Med_grid, Med_grid$VALUE == 0)
Med_grid_union = st_union(Med_grid_sea$geom)

EEZ_MedSea = st_read(here("data/MPA_scenarios/QGIS/eez_v11.shp"))
EEZ_MedSea_3035 = st_transform(EEZ_MedSea, 3035)
EEZ_MedSea_only_3035 = st_intersection(EEZ_MedSea_3035,Med_grid_union)
EEZ_MedSea_only_3035_union = st_union(EEZ_MedSea_only_3035)

# Expansion of current MPA network ---------------------------------------------

output.Dir = here("data/MPA_scenarios/Existing_network")

Intersection = st_read(here("data/MPA_scenarios/QGIS/Overlap_Medgrid_existingMPAs_final.shp")) %>%
  rename("Overlap_km" = "Baseline_M") %>%
  rename("Overlap_percentage" = "Baseline_1") %>%
  dplyr::select(fid, VALUE, Overlap_km, Overlap_percentage, Cluster_38, Cells_38) %>%
  rename("MPA" = "Cluster_38")

pas = 1
cells_by_step = 62
count_cells = 0
Max_cells_plans = length(Intersection$VALUE[Intersection$Overlap_percentage > 0]) # 971
Max_cells = 1869 # to reach 30% of Med
scenario_name = "Expansion of existing network"

create.mpa.mask.literature.fn(output.Dir, pas, count_cells, Max_cells, Med_grid, Max_cells_plans, cells_by_step, Intersection, scenario_name)

# Plot initial coverage of MPA network
Med_grid$VALUE = Intersection$MPA
table(Med_grid$VALUE)
Med_grid$color[Med_grid$VALUE == -99] = "white"
Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
Med_grid$color[Med_grid$VALUE == 1] = "darkblue"

plot <- ggplot() +
  geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
  geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "#000000", fill=NA, size=1),
        plot.title = element_text(family = "Helvetica", size = (12))) 

plot
ggsave(paste0(output.Dir, "/Plots/intersection_initial.jpg"), plot, dpi = 300)

# EEZ-conservation scenario ----------------------------------------------------
output.Dir = here("data/MPA_scenarios/EEZ-conservation")


pas = 1
cells_by_step = 62
count_cells = 0
Max_nb_of_MPAs = 62
MPA_cell_size = 400
percentage_of_EEZ_target = 0.1
graine_AMP = 0
pct_coverage = seq(0.01,0.31,0.01)
scenario_name = "EEZ - conservation"

# with replicates
for (rep in 1:20){
  Med_grid = st_read(here("data/MPA_scenarios/QGIS/Med_polygon.gpkg")) %>%
    mutate(cell_ID = row_number())
  count_cells = 0
  graine_AMP = 0
  dir.create(paste0(output.Dir, "/rep", rep))
  dir.create(paste0(output.Dir, "/rep", rep, "/Plots"))
  create.mpa.mask.randomEEZ.fn(output.Dir, pas, count_cells, pct_coverage, Med_grid, EEZ_MedSea, cells_by_step, Max_nb_of_MPAs, MPA_cell_size, percentage_of_EEZ_target, graine_AMP, rep, scenario_name)
}

# Random scenario --------------------------------------------------------------
output.Dir = here("data/MPA_scenarios/Random")

pas = 1
cells_by_step = 62
count_cells = 0
Max_cells = 1869 # number of cells to caver 30% MPA network
Nb_of_MPA_clusters = 62
scenario_name = "Random"


for (rep in 11:20){
  Med_grid = st_read(here("data/MPA_scenarios/Med_polygon.gpkg"))
  count_cells = 0
  dir.create(paste0(output.Dir, "/rep", rep))
  dir.create(paste0(output.Dir, "/rep", rep, "/Plots"))
  create.mpa.mask.random.4way.expansion.fn(output.Dir, pas, count_cells, Max_cells, rep, Med_grid, cells_by_step, Nb_of_MPA_clusters, scenario_name)
  
}

# Mazor scenario 9 -------------------------------------------------------------
output.Dir = here("data/MPA_scenarios/Mazor_scenario_9")

Intersection = st_read(here("data/MPA_scenarios/QGIS/Overlap_scenario9_osmose.shp")) %>%
  rename("Overlap_km" = "Scenario_9") %>%
  rename("Overlap_percentage" = "Scenario_1") %>%
  dplyr::select(fid, VALUE, Overlap_km, Overlap_percentage, Cluster_38, Cells_38) %>%
  rename("MPA" = "Cluster_38")

pas = 1
cells_by_step = 62
count_cells = 0
Max_cells_plans = length(Intersection$VALUE[Intersection$Overlap_percentage > 0 & Intersection$VALUE == 0]) # 892
Max_cells = 1869 # to reach 30% of Med
scenario_name = "Mazor - scenario 9"

create.mpa.mask.Mazor.scenarios.fn(output.Dir, pas, count_cells, Max_cells, Med_grid, Max_cells_plans, cells_by_step, Intersection, scenario_name)

# Plot initial coverage of MPA network
Med_grid$VALUE = Intersection$MPA
table(Med_grid$VALUE)
Med_grid$color[Med_grid$VALUE == -99] = "white"
Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
Med_grid$color[Med_grid$VALUE == 1] = "darkblue"

plot <- ggplot() +
  geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
  geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "#000000", fill=NA, size=1),
        plot.title = element_text(family = "Helvetica", size = (12))) 

plot
ggsave(paste0(output.Dir, "/Plots/intersection_initial.jpg"), plot, dpi = 300)

# Mazor scenario 8 -------------------------------------------------------------
output.Dir = here("data/MPA_scenarios/QGIS/Mazor_scenario_8")

Intersection = st_read(here("data/MPA_scenarios/QGIS/Overlap_scenario8_osmose.shp")) %>%
  rename("Overlap_km" = "Scenario8_") %>%
  rename("Overlap_percentage" = "Scenario_1") %>%
  dplyr::select(fid, VALUE, Overlap_km, Overlap_percentage, Cluster_38, Cells_38) %>%
  rename("MPA" = "Cluster_38")

pas = 1
cells_by_step = 62
count_cells = 0
Max_cells_plans = length(Intersection$VALUE[Intersection$Overlap_percentage > 0 & Intersection$VALUE == 0]) # 600
Max_cells = 1869 # to reach 30% of Med
scenario_name = "Mazor - scenario 8"

create.mpa.mask.Mazor.scenarios.fn(output.Dir, pas, count_cells, Max_cells, Med_grid, Max_cells_plans, cells_by_step, Intersection, scenario_name)

# Plot initial coverage of MPA network
Med_grid$VALUE = Intersection$MPA
table(Med_grid$VALUE)
Med_grid$color[Med_grid$VALUE == -99] = "white"
Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
Med_grid$color[Med_grid$VALUE == 1] = "darkblue"

plot <- ggplot() +
  geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
  geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "#000000", fill=NA, size=1),
        plot.title = element_text(family = "Helvetica", size = (12))) 

plot
ggsave(paste0(output.Dir, "/Plots/intersection_initial.jpg"), plot, dpi = 300)

# Micheli ----------------------------------------------------------------------
output.Dir = here("data/MPA_scenarios/Micheli")

Overlap_Medgrid_Plans10pct = st_read(here("data/MPA_scenarios/QGIS/Overlap_Medgrid_Plans10pct.shp"))

Overlap_Medgrid_Plans30pct = st_read(here("data/MPA_scenarios/QGIS/Overlap_Medgrid_Plans.shp")) %>%
  rename("Overlap_km" = "Plans_area") %>%
  rename("Overlap_percentage" = "Plans_pc")

pas = 1
cells_by_step = 62
count_cells = 0
Max_cells_plans = length(Overlap_Medgrid_Plans30pct$VALUE[Overlap_Medgrid_Plans30pct$Overlap_percentage > 0 & Overlap_Medgrid_Plans30pct$VALUE == 0]) # 1903
Max_cells = 1869 # to reach 30% of Med
scenario_name = "Micheli"

create.mpa.mask.Micheli.fn(output.Dir, pas, count_cells, Max_cells, Med_grid, Max_cells_plans_1, Max_cells_plans_2, cells_by_step, Number_of_clusters, Overlap_Medgrid_Plans10pct, Overlap_Medgrid_Plans30pct, scenario_name)

# Plot initial coverage of MPA network
Overlap_Medgrid_Plans30pct$VALUE[Overlap_Medgrid_Plans30pct$MPA > 0] = 1
Med_grid$VALUE = Overlap_Medgrid_Plans30pct$VALUE
table(Med_grid$VALUE)
Med_grid$color[Med_grid$VALUE == -99] = "white"
Med_grid$color[Med_grid$VALUE == 0] = "lightblue"
Med_grid$color[Med_grid$VALUE == 1] = "darkblue"

plot <- ggplot() +
  geom_sf(data = Med_grid_sea$geom, fill = NA, colour = "cadetblue4", alpha = 0.5) +
  geom_sf(data = Med_grid$geom, fill = Med_grid$color, colour = NA) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "#000000", fill=NA, size=1),
        plot.title = element_text(family = "Helvetica", size = (12))) 

plot
ggsave(paste0(output.Dir, "/Plots/intersection_initial.jpg"), plot, dpi = 300)

# Plot priorization scheme -----------
Overlap_Medgrid_Plans10pct$VALUE[Overlap_Medgrid_Plans10pct$Cells_1pct == 1] = 1
Overlap_Medgrid_Plans10pct$VALUE[Overlap_Medgrid_Plans10pct$Cells_2pct == 1] = 2
Overlap_Medgrid_Plans10pct$VALUE[Overlap_Medgrid_Plans10pct$Cells_3pct == 1] = 3
Overlap_Medgrid_Plans10pct$VALUE[Overlap_Medgrid_Plans10pct$Cells_4pct == 1] = 4
Overlap_Medgrid_Plans10pct$VALUE[Overlap_Medgrid_Plans10pct$Cells_5pct == 1] = 5
Overlap_Medgrid_Plans10pct$VALUE[Overlap_Medgrid_Plans10pct$Cells_6pct == 1] = 6
Overlap_Medgrid_Plans10pct$VALUE[Overlap_Medgrid_Plans10pct$Cells_7pct == 1] = 7
Overlap_Medgrid_Plans10pct$VALUE[Overlap_Medgrid_Plans10pct$Cells_8pct == 1] = 8
Overlap_Medgrid_Plans10pct$VALUE[Overlap_Medgrid_Plans10pct$Cells_9pct == 1] = 9
Overlap_Medgrid_Plans10pct$VALUE[Overlap_Medgrid_Plans10pct$Cells_10pc == 1] = 10


table(Overlap_Medgrid_Plans10pct$VALUE)
Overlap_Medgrid_Plans10pct_subset = Overlap_Medgrid_Plans10pct %>%
  filter(VALUE > 0) %>%
  mutate(VALUE = as.character(VALUE))

class(Overlap_Medgrid_Plans10pct_subset$VALUE)
Overlap_Medgrid_Plans10pct_subset$VALUE <- factor(Overlap_Medgrid_Plans10pct_subset$VALUE, levels = c("10", "9", "8", "7", "6", "5", "4", "3", "2", "1"))

plot <- ggplot() +
  geom_sf(data = Med_grid_sea$geom, fill = "lightblue", col = "lightblue") +
  geom_sf(data = Overlap_Medgrid_Plans10pct_subset$geom, aes(fill = Overlap_Medgrid_Plans10pct_subset$VALUE), colour = NA) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "#000000", fill=NA, size=1),
        plot.title = element_text(family = "Helvetica", size = (12)),
        legend.title = element_blank())+
  scale_fill_viridis_d(option = "turbo") 

plot
ggsave(paste0(output.Dir, "/Plots/priorization_Micheli.jpg"), plot, dpi = 300)