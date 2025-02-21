#' Function to plot alpha-taxonomic divesity inside MPAs before and after their establishment
#'
#' 
#' @param data OSMOSE-MED simulation outputs alpha-taxonomic divesity
#' 
#' @return Figure A7.1 (appendix)
#'
#' @export

plot.figA7.1 <- function(save = TRUE){
  # Abundance map before MPAs
  nc_abundance_spatial  = nc_open(here("data/spatial/Abundance_nompa_scenario.nc"))
  abundance_spatial  = ncvar_get(nc_abundance_spatial, "Abundance")
  
  abundance_spatial_mean = apply(abundance_spatial, c(1,2,3), mean, na.rm = T)
  abundance_spatial_mean_sp = apply(abundance_spatial, c(1,2), sum, na.rm = T) # nombre tot d'individus
  
  for (i in 1:101){
    
    if (i == 1){
      abundance_spatial_sp1 = abundance_spatial_mean[1:190,1:83,i]
      p_sp1 = abundance_spatial_sp1/abundance_spatial_mean_sp
      H_sp1 = -(p_sp1*log(p_sp1))
      
      abundance_spatial_sp2 = abundance_spatial_mean[1:190,1:83,i+1]
      p_sp2 = abundance_spatial_sp2/abundance_spatial_mean_sp
      H_sp2 = -(p_sp2*log(p_sp2))
      
      sum_matrix <- mapply(function(x, y) sum(c(x, y), na.rm = TRUE), H_sp2, H_sp1)
      sum_matrix <- matrix(sum_matrix, nrow = nrow(H_sp2))
    }
    
    else{
      if (i == 2){
        i = 3
      }
      abundance_spatial_sp1 = abundance_spatial_mean[1:190,1:83,i]
      p_sp1 = abundance_spatial_sp1/abundance_spatial_mean_sp
      H_sp1 = -(p_sp1*log(p_sp1))
      sum_matrix <- mapply(function(x, y) sum(c(x, y), na.rm = TRUE), sum_matrix, H_sp1)
      sum_matrix <- matrix(sum_matrix, nrow = nrow(H_sp1))
      
    }
    
  }
  
  sum_matrix[sum_matrix == 0] = NA
  exp_H_spatial = exp(sum_matrix)
  mean_exp_H_spatial = mean(exp_H_spatial, na.rm = T)
  
  
  # before implementing MPAs -----------------------------
  number_lit_scenarios = length(lit.scenarios.name)
  for (mpa_scenario in 1:number_lit_scenarios){
    for (mpa_coverage in 1:30){
      
      # Mazor scenario 8
      mpa = read.csv(paste0(lit.mpa.mask.dir[mpa_scenario], "/", mpa_coverage, ".csv"), sep = ",", header=FALSE)
      mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
      mpa = t(as.matrix(mpa))
      
      exp_H_spatial_copy = exp_H_spatial
      exp_H_spatial_copy[mpa == 0] = NA # keep only inside MPAs
      
      # average exp_H between all cells in areas that will become MPAs (map before MPAs)
      mean_exp_H_inside = mean(exp_H_spatial_copy, na.rm = T)
      
      if (mpa_coverage == 1){
        mean_exp_H_inside_all_coverages = mean_exp_H_inside
      } else{
        mean_exp_H_inside_all_coverages = c(mean_exp_H_inside_all_coverages, mean_exp_H_inside)
      }
      
      
    } 
    Mean_diversity_inside = data.frame(mean_exp_H_inside_all_coverages) %>%
      rename(mean_exp_H_inside = mean_exp_H_inside_all_coverages) %>%
      mutate(mpa_coverage = seq(1,30,1)) %>%
      mutate(mpa_scenario = lit.scenarios.name[mpa_scenario])
    
    if (mpa_scenario == 1){
      Mean_diversity_inside_lit_scenarios = Mean_diversity_inside
    } else{
      Mean_diversity_inside_lit_scenarios = rbind(Mean_diversity_inside_lit_scenarios, Mean_diversity_inside)
    }
    
  }
  
  
  # add randoms
  number_random_scenarios = length(random.scenarios.name)
  for (mpa_scenario in 1:number_random_scenarios){
    for (mpa_coverage in 1:30){
      
      mpa = read.csv(paste0(random.mpa.mask.dir[mpa_scenario], "/", mpa_coverage, ".csv"), sep = ",", header=FALSE)
      mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
      mpa = t(as.matrix(mpa))
      
      exp_H_spatial_copy = exp_H_spatial
      exp_H_spatial_copy[mpa == 0] = NA # keep only inside MPAs
      
      # average exp_H between all cells in areas that will become MPAs (map before MPAs)
      mean_exp_H_inside = mean(exp_H_spatial_copy, na.rm = T)
      
      if (mpa_coverage == 1){
        mean_exp_H_inside_all_coverages = mean_exp_H_inside
      } else{
        mean_exp_H_inside_all_coverages = c(mean_exp_H_inside_all_coverages, mean_exp_H_inside)
      }
      
      
    } 
    Mean_diversity_inside = data.frame(mean_exp_H_inside_all_coverages) %>%
      rename(mean_exp_H_inside = mean_exp_H_inside_all_coverages) %>%
      mutate(mpa_coverage = seq(1,30,1)) %>%
      mutate(mpa_scenario = random.scenarios.name[mpa_scenario])
    
    if (mpa_scenario == 1){
      Mean_diversity_inside_random_scenarios = Mean_diversity_inside
    } else{
      Mean_diversity_inside_random_scenarios = rbind(Mean_diversity_inside_random_scenarios, Mean_diversity_inside)
    }
    
  }
  
  Mean_diversity_ref_inside_lit_scenarios = Mean_diversity_inside_lit_scenarios %>%
    mutate(before_after = "Before MPAs") %>%
    rename(mean_exp_H_inside_ref = mean_exp_H_inside)
  
  
  Mean_diversity_ref_inside_random_basin = Mean_diversity_inside_random_scenarios %>%
    filter(grepl('Random', mpa_scenario)) %>%
    group_by(mpa_coverage) %>%
    mutate(max = max(mean_exp_H_inside, na.rm = T),
           min = min(mean_exp_H_inside, na.rm = T)) %>%
    mutate(scenario_group = "Random") %>%
    mutate(before_after = "Before MPAs") %>%
    rename(mean_exp_H_inside_ref = mean_exp_H_inside) 
  
  
  Mean_diversity_ref_inside_random_EEZ = Mean_diversity_inside_random_scenarios %>%
    filter(grepl('EEZ', mpa_scenario)) %>%
    group_by(mpa_coverage) %>%
    mutate(max = max(mean_exp_H_inside, na.rm = T),
           min = min(mean_exp_H_inside, na.rm = T)) %>%
    mutate(scenario_group = "EEZ-conservation") %>%
    mutate(before_after = "Before MPAs") %>%
    rename(mean_exp_H_inside_ref = mean_exp_H_inside)
  
  
  ### AFTER mpas
  Mean_diversity_inside_GSA_prop_lit = read.csv(here("data/diversity/Mean_diversity_inside_lit_scenarios.csv")) %>%
    #dplyr::select(2:4) %>%
    mutate(before_after = "After MPAs")
  
  Mean_diversity_inside_GSA_prop_random_basin = read.csv(here("data/diversity/Mean_diversity_inside_random_basin_scenarios.csv")) %>%
    group_by(mpa_coverage) %>%
    mutate(high = max(mean_exp_H_inside, na.rm = T),
           low = min(mean_exp_H_inside, na.rm = T)) %>%
    ungroup() %>%
    mutate(scenario_group = "Random") %>%
    mutate(before_after = "After MPAs")
  Mean_diversity_inside_GSA_prop_random_basin$mpa_scenario <- gsub("-rep", " ", Mean_diversity_inside_GSA_prop_random_basin$mpa_scenario)
  
  
  Mean_diversity_inside_GSA_prop_random_EEZ = read.csv(here("data/diversity/Mean_diversity_inside_random_EEZ_scenarios.csv")) %>%
    group_by(mpa_coverage) %>%
    mutate(high = max(mean_exp_H_inside, na.rm = T),
           low = min(mean_exp_H_inside, na.rm = T)) %>%
    ungroup() %>%
    mutate(scenario_group = "EEZ-conservation") %>%
    mutate(before_after = "After MPAs")
  Mean_diversity_inside_GSA_prop_random_EEZ$mpa_scenario <- gsub("-rep", " ", Mean_diversity_inside_GSA_prop_random_EEZ$mpa_scenario)
  
  ### BIND both
  Diversity_before_after_lit = Mean_diversity_inside_GSA_prop_lit %>%
    left_join(Mean_diversity_ref_inside_lit_scenarios, by = c("mpa_coverage", "mpa_scenario"))
  
  Diversity_before_after_random_basin = Mean_diversity_inside_GSA_prop_random_basin %>%
    left_join(Mean_diversity_ref_inside_random_basin, by = c("mpa_coverage", "mpa_scenario"))
  
  Diversity_before_after_random_EEZ = Mean_diversity_inside_GSA_prop_random_EEZ %>%
    left_join(Mean_diversity_ref_inside_random_EEZ, by = c("mpa_coverage", "mpa_scenario"))
  
  #####------ PLOT
  mpa_colors <- c("Existing network" = "gold", 
                  "Micheli" = "darkorange", 
                  "Mazor scenario 8" = "darkslateblue", 
                  "Mazor scenario 9" = "darkorchid")
  
  
  legend_labels <- c("S1-Current",  "S5-Mazor GFCM", "S6-Mazor SAUP", "S4-Micheli")
  
  Diversity_before_after_lit_GSAprop <- ggplot() +
    
    geom_line(data = Mean_diversity_ref_inside_lit_scenarios, aes(x = mpa_coverage, y = mean_exp_H_inside_ref, colour = mpa_scenario, lty = before_after), lwd = 0.8, alpha=1) +
    geom_line(data = Mean_diversity_inside_GSA_prop_lit, aes(x = mpa_coverage, y = mean_exp_H_inside, colour = mpa_scenario, lty = before_after), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Diversity_before_after_lit, aes(x = mpa_coverage, ymin = mean_exp_H_inside_ref, ymax = mean_exp_H_inside, fill = mpa_scenario), alpha = 0.2) +
    scale_colour_manual(values = mpa_colors, labels = legend_labels) +
    scale_fill_manual(values = mpa_colors, labels = legend_labels) +
    theme_bw() +
    labs(x ="MPA coverage (%)", y = "Taxonomic diversity", title = "Current & Science-informed") +
    
    theme(legend.title = element_blank(),
          legend.position = "bottom") +
    ylim(1.137, 1.163)
  
  
  # Random-basin scenarios
  Diversity_before_after_random_basin$mpa_scenario <- gsub("Random", "", Diversity_before_after_random_basin$mpa_scenario)
  
  legend_labels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  
  Diversity_before_after_random_basin_GSAprop <- ggplot() +
    geom_line(data = Mean_diversity_ref_inside_random_basin, aes(x = mpa_coverage, y = mean_exp_H_inside_ref, colour = mpa_scenario, lty = before_after), lwd = 0.8, alpha=1) +
    geom_line(data = Mean_diversity_inside_GSA_prop_random_basin, aes(x = mpa_coverage, y = mean_exp_H_inside, colour = mpa_scenario, lty = before_after), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Diversity_before_after_random_basin, aes(x = mpa_coverage, ymin = mean_exp_H_inside_ref, ymax = mean_exp_H_inside, fill = mpa_scenario), alpha = 0.5) +
    labs(x ="MPA coverage (%)", y = "Taxonomic diversity", title = "S2-Random basin") +
    scale_colour_grey(start = 0, end = 0.8, labels = legend_labels) +
    scale_fill_grey(start = 0, end = 0.8,labels = legend_labels) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    ylim(1.137, 1.163)
  
  
  
  
  # Random-EEZ scenarios
  # Random-basin scenarios
  Diversity_before_after_random_EEZ$mpa_scenario <- gsub("EEZ-conservation", "", Diversity_before_after_random_EEZ$mpa_scenario)
  
  legend_labels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  
  Diversity_before_after_random_EEZ_GSAprop <- ggplot() +
    geom_line(data = Mean_diversity_ref_inside_random_EEZ, aes(x = mpa_coverage, y = mean_exp_H_inside_ref, colour = mpa_scenario, lty = before_after), lwd = 0.8, alpha=1) +
    geom_line(data = Mean_diversity_inside_GSA_prop_random_EEZ, aes(x = mpa_coverage, y = mean_exp_H_inside, colour = mpa_scenario, lty = before_after), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Diversity_before_after_random_EEZ, aes(x = mpa_coverage, ymin = mean_exp_H_inside_ref, ymax = mean_exp_H_inside, fill = mpa_scenario), alpha = 0.5) +
    labs(x ="MPA coverage (%)", y = "Taxonomic diversity", title = "S3-Random EEZ") +
    scale_colour_grey(start = 0, end = 0.8, labels = legend_labels) +
    scale_fill_grey(start = 0, end = 0.8,labels = legend_labels) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "none") +
    ylim(1.137, 1.163)
  
  
  
  
  ###Join together
  
  figA7.2 <- plot_grid(Diversity_before_after_lit_GSAprop, Diversity_before_after_random_basin_GSAprop, Diversity_before_after_random_EEZ_GSAprop,
                     ncol=3,
                     nrow=1,
                     align = "hv",
                     rel_widths = c(1, 1, 1),   # Adjust relative widths (set equal to remove spacing)
                     rel_heights = c(1,1, 1))
  
  
  ggsave(here("figures/appendix/FigA7.1_4__taxonomic_diversity_inside_NTMPAs_before_and_after_establishment_GSAprop.png"), height = 7, width = 10,  dpi = 1000)
  
}
