#' Function to plot LFI and alpha-taxonomic divesity inside MPAs
#'
#' 
#' @param data OSMOSE-MED simulation outputs of Large Fish Indicator and alpha-taxonomic divesity
#' 
#' @return Figure 4 (main text)
#'
#' @export
#' 

plot.LFI.inside.MPAs <- function(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline){

  LFI_all_fishing_scenarios_lit = rbind(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline) %>%
    filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
    mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
    group_by(mpa_coverage, mpa, scenario, Fishing_scenario) %>%
    summarise(mean_LFI_reps = mean(mean_LFI_60_years))
  
  LFI_all_fishing_scenarios_Random = rbind(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline) %>%
    filter(grepl('Random', scenario)) %>%
    mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
    group_by(mpa_coverage, mpa, scenario, Fishing_scenario) %>%
    summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
    ungroup() %>%
    group_by(mpa_coverage, mpa, Fishing_scenario) %>%
    mutate(high = max(mean_LFI_reps),
           low = min(mean_LFI_reps),
           mean = mean(mean_LFI_reps)) %>%
    mutate(scenario_group = "Random")
  
  LFI_all_fishing_scenarios_EEZ = rbind(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline) %>%
    filter(grepl('EEZ', scenario)) %>%
    mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
    group_by(mpa_coverage, mpa, scenario, Fishing_scenario) %>%
    summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
    ungroup() %>%
    group_by(mpa_coverage, mpa, Fishing_scenario) %>%
    mutate(high = max(mean_LFI_reps),
           low = min(mean_LFI_reps),
           mean = mean(mean_LFI_reps)) %>%
    mutate(scenario_group = "EEZ-conservation")  
  ## Inside MPAs
  LFI_all_fishing_scenarios_lit_inside = LFI_all_fishing_scenarios_lit %>%
    filter(mpa == "Inside MPAs")
  
  LFI_all_fishing_scenarios_Random_inside = LFI_all_fishing_scenarios_Random %>%
    filter(mpa == "Inside MPAs")
  
  LFI_all_fishing_scenarios_EEZ_inside = LFI_all_fishing_scenarios_EEZ %>%
    filter(mpa == "Inside MPAs")
  
  
  ### PLOT
  
  # Proportional by GSA
  # GSA prop
  LFI_all_fishing_scenarios_lit_inside_GSAprop = LFI_all_fishing_scenarios_lit %>%
    filter(mpa == "Inside MPAs") %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  LFI_all_fishing_scenarios_Random_inside_GSAprop = LFI_all_fishing_scenarios_Random %>%
    filter(mpa == "Inside MPAs") %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  
  LFI_all_fishing_scenarios_EEZ_inside_GSAprop = LFI_all_fishing_scenarios_EEZ %>%
    filter(mpa == "Inside MPAs") %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  LFI_inside_GSAprop <- ggplot() +
    
    geom_line(data = LFI_all_fishing_scenarios_Random_inside_GSAprop, aes(x = mpa_coverage, y = mean_LFI_reps, colour = scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_Random_inside_GSAprop, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = LFI_all_fishing_scenarios_EEZ_inside_GSAprop, aes(x = mpa_coverage, y = mean_LFI_reps, colour = scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_EEZ_inside_GSAprop, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = LFI_all_fishing_scenarios_lit_inside_GSAprop, aes(x = mpa_coverage, y = mean_LFI_reps, colour = scenario), lwd = 0.8, alpha=1) +
    
    
    #geom_line(data = Biom_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.5, lty = "solid") +
    
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    scale_fill_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    labs(x ="MPA coverage (%)", y = "LFI (> 20 cm)", title = "Proportional by GSA") +
    
    theme(legend.position = "bottom") +
    #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(0.278, 0.43)
  
  return(LFI_inside_GSAprop)
  
}

plot.LFI.outside <- function(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline){
  
  LFI_all_fishing_scenarios_lit = rbind(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline) %>%
    filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
    mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
    group_by(mpa_coverage, mpa, scenario, Fishing_scenario) %>%
    summarise(mean_LFI_reps = mean(mean_LFI_60_years))
  
  LFI_all_fishing_scenarios_Random = rbind(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline) %>%
    filter(grepl('Random', scenario)) %>%
    mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
    group_by(mpa_coverage, mpa, scenario, Fishing_scenario) %>%
    summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
    ungroup() %>%
    group_by(mpa_coverage, mpa, Fishing_scenario) %>%
    mutate(high = max(mean_LFI_reps),
           low = min(mean_LFI_reps),
           mean = mean(mean_LFI_reps)) %>%
    mutate(scenario_group = "Random")
  
  LFI_all_fishing_scenarios_EEZ = rbind(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline) %>%
    filter(grepl('EEZ', scenario)) %>%
    mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
    group_by(mpa_coverage, mpa, scenario, Fishing_scenario) %>%
    summarise(mean_LFI_reps = mean(mean_LFI_60_years)) %>%
    ungroup() %>%
    group_by(mpa_coverage, mpa, Fishing_scenario) %>%
    mutate(high = max(mean_LFI_reps),
           low = min(mean_LFI_reps),
           mean = mean(mean_LFI_reps)) %>%
    mutate(scenario_group = "EEZ-conservation")

  ## Outside MPAs
  
  LFI_all_fishing_scenarios_lit_outside = LFI_all_fishing_scenarios_lit %>%
    filter(mpa == "Outside MPAs")
  
  LFI_all_fishing_scenarios_Random_outside = LFI_all_fishing_scenarios_Random %>%
    filter(mpa == "Outside MPAs")
  
  LFI_all_fishing_scenarios_EEZ_outside = LFI_all_fishing_scenarios_EEZ %>%
    filter(mpa == "Outside MPAs")
  ## Outside
  # GSA prop
  LFI_all_fishing_scenarios_lit_outside_GSAprop = LFI_all_fishing_scenarios_lit %>%
    filter(mpa == "Outside MPAs") %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  LFI_all_fishing_scenarios_Random_outside_GSAprop = LFI_all_fishing_scenarios_Random %>%
    filter(mpa == "Outside MPAs") %>%
    filter(Fishing_scenario == "Proportional by GSA") 
  
  
  LFI_all_fishing_scenarios_EEZ_outside_GSAprop = LFI_all_fishing_scenarios_EEZ %>%
    filter(mpa == "Outside MPAs") %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  LFI_outside_GSAprop <- ggplot() +
    
    geom_line(data = LFI_all_fishing_scenarios_Random_outside_GSAprop, aes(x = mpa_coverage, y = mean_LFI_reps, colour = scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_Random_outside_GSAprop, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = LFI_all_fishing_scenarios_EEZ_outside_GSAprop, aes(x = mpa_coverage, y = mean_LFI_reps, colour = scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_EEZ_outside_GSAprop, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = LFI_all_fishing_scenarios_lit_outside_GSAprop, aes(x = mpa_coverage, y = mean_LFI_reps, colour = scenario), lwd = 0.8, alpha=1) +
    
    
    #geom_line(data = Biom_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.5, lty = "solid") +
    
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    scale_fill_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    labs(x ="MPA coverage (%)", y = "Large Fish Indicator (> 20 cm)", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(0.278, 0.43)
  
  return(LFI_outside_GSAprop)
}

calculate.exp.H <- function(){
  # add 0%:
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
  
  return(exp_H_spatial)
}

plot.diversity.inside <- function(Mean_diversity_inside_GSA_prop_lit, Mean_diversity_inside_GSA_prop_random_basin, Mean_diversity_inside_GSA_prop_random_EEZ){
  
  # calculate Hill-Shannon diversity index
  exp_H_spatial = calculate.exp.H()
  

  mean_exp_H_spatial = rep(mean(exp_H_spatial, na.rm = T),4)
  
  zero_pct_lit = data.frame(mean_exp_H_spatial) %>%
    rename(mean_exp_H_inside = mean_exp_H_spatial) %>%
    mutate(mpa_coverage = 0) %>%
    mutate(mpa_scenario = c("Mazor scenario 8", "Mazor scenario 9", "Micheli", "Existing network"))
  
  Mean_diversity_inside_GSA_prop_lit_with_zero = rbind(Mean_diversity_inside_GSA_prop_lit, zero_pct_lit)
  
  Mean_diversity_inside_GSA_prop_random_basin = Mean_diversity_inside_GSA_prop_random_basin %>%
    group_by(mpa_coverage) %>%
    mutate(high = max(mean_exp_H_inside, na.rm = T),
           low = min(mean_exp_H_inside, na.rm = T)) %>%
    ungroup() %>%
    mutate(scenario_group = "Random")
  
  mean_exp_H_spatial = rep(mean(exp_H_spatial, na.rm = T),10)
  
  zero_pct_random_basin = data.frame(mean_exp_H_spatial) %>%
    rename(mean_exp_H_inside = mean_exp_H_spatial) %>%
    mutate(mpa_coverage = 0) %>%
    mutate(mpa_scenario = c("Random-rep11", "Random-rep12", "Random-rep13", "Random-rep14", "Random-rep15", "Random-rep16", "Random-rep17", "Random-rep18", "Random-rep19", "Random-rep20")) %>%
    mutate(high = mean(exp_H_spatial, na.rm = T),
           low = mean(exp_H_spatial, na.rm = T),
           scenario_group = "Random")
  
  
  Mean_diversity_inside_GSA_prop_random_basin_with_zero = rbind(Mean_diversity_inside_GSA_prop_random_basin, zero_pct_random_basin)
  Mean_diversity_inside_GSA_prop_random_basin_with_zero$mpa_scenario <- gsub("-rep", " ", Mean_diversity_inside_GSA_prop_random_basin_with_zero$mpa_scenario)
  
  Mean_diversity_inside_GSA_prop_random_EEZ = Mean_diversity_inside_GSA_prop_random_EEZ %>%
    group_by(mpa_coverage) %>%
    mutate(high = max(mean_exp_H_inside, na.rm = T),
           low = min(mean_exp_H_inside, na.rm = T)) %>%
    ungroup() %>%
    mutate(scenario_group = "EEZ-conservation")
  
  zero_pct_random_EEZ = data.frame(mean_exp_H_spatial) %>%
    rename(mean_exp_H_inside = mean_exp_H_spatial) %>%
    mutate(mpa_coverage = 0) %>%
    mutate(mpa_scenario = c("EEZ-conservation-rep1", "EEZ-conservation-rep2", "EEZ-conservation-rep3", "EEZ-conservation-rep4", "EEZ-conservation-rep5", "EEZ-conservation-rep6", "EEZ-conservation-rep7", "EEZ-conservation-rep8", "EEZ-conservation-rep9", "EEZ-conservation-rep10")) %>%
    mutate(high = mean(exp_H_spatial, na.rm = T),
           low = mean(exp_H_spatial, na.rm = T),
           scenario_group = "EEZ-conservation")
  
  
  Mean_diversity_inside_GSA_prop_random_EEZ_with_zero = rbind(Mean_diversity_inside_GSA_prop_random_EEZ, zero_pct_random_EEZ)
  Mean_diversity_inside_GSA_prop_random_EEZ_with_zero$mpa_scenario <- gsub("-rep", " ", Mean_diversity_inside_GSA_prop_random_EEZ_with_zero$mpa_scenario)
  
  
  # Inside MPAs -------------------------------------------------------------
  
  ### PLOT

  
  Diversity_inside_after_MPAs_GSAprop <- ggplot() +
    
    geom_line(data = Mean_diversity_inside_GSA_prop_random_basin_with_zero, aes(x = mpa_coverage, y = mean_exp_H_inside, colour = mpa_scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = Mean_diversity_inside_GSA_prop_random_basin_with_zero, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Mean_diversity_inside_GSA_prop_random_EEZ_with_zero, aes(x = mpa_coverage, y = mean_exp_H_inside, colour = mpa_scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = Mean_diversity_inside_GSA_prop_random_EEZ_with_zero, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Mean_diversity_inside_GSA_prop_lit_with_zero, aes(x = mpa_coverage, y = mean_exp_H_inside, colour = mpa_scenario), lwd = 0.8, alpha=1) +
    
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    scale_fill_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    labs(x ="MPA coverage (%)", y = "Taxonomic diversity", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) 
  
  return(Diversity_inside_after_MPAs_GSAprop)

}

plot.diversity.outside <- function(Mean_diversity_outside_GSA_prop_lit, Mean_diversity_outside_GSA_prop_random_basin, Mean_diversity_outside_GSA_prop_random_EEZ){
  
  # calculate Hill-Shannon diversity index
  exp_H_spatial = calculate.exp.H()
  
  
  mean_exp_H_spatial = rep(mean(exp_H_spatial, na.rm = T),4)
  
  zero_pct_lit = data.frame(mean_exp_H_spatial) %>%
    rename(mean_exp_H_inside = mean_exp_H_spatial) %>%
    mutate(mpa_coverage = 0) %>%
    mutate(mpa_scenario = c("Mazor scenario 8", "Mazor scenario 9", "Micheli", "Existing network"))
  

  mean_exp_H_spatial = rep(mean(exp_H_spatial, na.rm = T),10)
  
  zero_pct_random_basin = data.frame(mean_exp_H_spatial) %>%
    rename(mean_exp_H_inside = mean_exp_H_spatial) %>%
    mutate(mpa_coverage = 0) %>%
    mutate(mpa_scenario = c("Random-rep11", "Random-rep12", "Random-rep13", "Random-rep14", "Random-rep15", "Random-rep16", "Random-rep17", "Random-rep18", "Random-rep19", "Random-rep20")) %>%
    mutate(high = mean(exp_H_spatial, na.rm = T),
           low = mean(exp_H_spatial, na.rm = T),
           scenario_group = "Random")
  
  zero_pct_random_EEZ = data.frame(mean_exp_H_spatial) %>%
    rename(mean_exp_H_inside = mean_exp_H_spatial) %>%
    mutate(mpa_coverage = 0) %>%
    mutate(mpa_scenario = c("EEZ-conservation-rep1", "EEZ-conservation-rep2", "EEZ-conservation-rep3", "EEZ-conservation-rep4", "EEZ-conservation-rep5", "EEZ-conservation-rep6", "EEZ-conservation-rep7", "EEZ-conservation-rep8", "EEZ-conservation-rep9", "EEZ-conservation-rep10")) %>%
    mutate(high = mean(exp_H_spatial, na.rm = T),
           low = mean(exp_H_spatial, na.rm = T),
           scenario_group = "EEZ-conservation")
  
  # Outside ----- 

  Mean_diversity_outside_GSA_prop_lit_with_zero = rbind(Mean_diversity_outside_GSA_prop_lit, zero_pct_lit)
  
  Mean_diversity_outside_GSA_prop_random_basin = Mean_diversity_outside_GSA_prop_random_basin %>%
    group_by(mpa_coverage) %>%
    mutate(high = max(mean_exp_H_inside, na.rm = T),
           low = min(mean_exp_H_inside, na.rm = T)) %>%
    ungroup() %>%
    mutate(scenario_group = "Random")
  
  Mean_diversity_outside_GSA_prop_random_basin_with_zero = rbind(Mean_diversity_outside_GSA_prop_random_basin, zero_pct_random_basin)
  Mean_diversity_outside_GSA_prop_random_basin_with_zero$mpa_scenario <- gsub("-rep", " ", Mean_diversity_outside_GSA_prop_random_basin_with_zero$mpa_scenario)
  
  Mean_diversity_outside_GSA_prop_random_EEZ = Mean_diversity_outside_GSA_prop_random_EEZ %>%
    group_by(mpa_coverage) %>%
    mutate(high = max(mean_exp_H_inside, na.rm = T),
           low = min(mean_exp_H_inside, na.rm = T)) %>%
    ungroup() %>%
    mutate(scenario_group = "EEZ-conservation")
  
  Mean_diversity_outside_GSA_prop_random_EEZ_with_zero = rbind(Mean_diversity_outside_GSA_prop_random_EEZ, zero_pct_random_EEZ)
  Mean_diversity_outside_GSA_prop_random_EEZ_with_zero$mpa_scenario <- gsub("-rep", " ", Mean_diversity_outside_GSA_prop_random_EEZ_with_zero$mpa_scenario)
  
  Diversity_outside_after_MPAs_GSAprop <- ggplot() +
    
    geom_line(data = Mean_diversity_outside_GSA_prop_random_basin_with_zero, aes(x = mpa_coverage, y = mean_exp_H_inside, colour = mpa_scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = Mean_diversity_outside_GSA_prop_random_basin_with_zero, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Mean_diversity_outside_GSA_prop_random_EEZ_with_zero, aes(x = mpa_coverage, y = mean_exp_H_inside, colour = mpa_scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = Mean_diversity_outside_GSA_prop_random_EEZ_with_zero, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Mean_diversity_outside_GSA_prop_lit_with_zero, aes(x = mpa_coverage, y = mean_exp_H_inside, colour = mpa_scenario), lwd = 0.8, alpha=1) +
  
    
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    scale_fill_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    
    labs(x ="MPA coverage (%)", y = "Taxonomic diversity", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(1.137, 1.163)
  
  return(Diversity_outside_after_MPAs_GSAprop)
}

plot.fig4 <- function(height_value, width_value, save){
  ## Fig 4A - LFI -------------------------------
  
  LFI_inside_GSAprop = plot.LFI.inside.MPAs(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline)
  
  LFI_inside_mod <- LFI_inside_GSAprop + theme(axis.title.x = element_blank(),
                                               plot.title = element_blank(),
                                               legend.position = "none",
                                               plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic")) + 
    labs(subtitle = "Inside NT MPAs") +
    theme(plot.margin = margin(0,0,0,0.5, "cm")) # 2 -right, 4 - left
  
  LFI_outside_GSAprop = plot.LFI.outside(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline)
  
  LFI_outside_mod <- LFI_outside_GSAprop + theme(axis.title.y = element_blank(),
                                                 #axis.ticks.x = element_blank(),
                                                 axis.title.x = element_blank(),
                                                 plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic"),
                                                 legend.position = "none",
                                                 plot.title = element_blank()) +
    labs(subtitle = "Outside NT MPAs") +
    theme(plot.margin = margin(0,0.5,0,0, "cm")) # 2 -right, 4 - left
  
  
  
  fig4A <- ggarrange(LFI_inside_mod, LFI_outside_mod,
                     ncol=2,
                     nrow=1,
                     align = "v")  
  
  ## Fig 4B ---------------------------------
  
  Diversity_inside_after_MPAs_GSAprop = plot.diversity.inside(Mean_diversity_inside_GSA_prop_lit, Mean_diversity_inside_GSA_prop_random_basin, Mean_diversity_inside_GSA_prop_random_EEZ)
  
  Diversity_inside_mod <- Diversity_inside_after_MPAs_GSAprop + theme(

    legend.text = element_text(size=14),
    legend.title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic")) + 
    labs(subtitle = "Inside NT MPAs") +
    theme(plot.margin = margin(0,0,0.5,0.5, "cm")) # 2 -right, 4 - left
  
  Diversity_outside_after_MPAs_GSAprop = plot.diversity.outside(Mean_diversity_outside_GSA_prop_lit, Mean_diversity_outside_GSA_prop_random_basin, Mean_diversity_outside_GSA_prop_random_EEZ)
  
  Diversity_outside_mod <- Diversity_outside_after_MPAs_GSAprop + theme(axis.title.y = element_blank(),
                                                                        legend.title = element_blank(),
                                                                        legend.text = element_text(size=14),
                                                                        plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic"),
                                                                        plot.title = element_blank()) +
    labs(subtitle = "Outside NT MPAs") +
    theme(plot.margin = margin(0,0.5,0.5,0, "cm")) # 2 -right, 4 - left
  
  

  fig4B <- ggarrange(Diversity_inside_mod, Diversity_outside_mod,
                     ncol=2,
                     nrow=1,
                     common.legend = T,
                     legend = "bottom",
                     align = "v") 
  
  fig4 <- plot_grid(fig4A, fig4B, ncol = 1, nrow = 2, rel_heights = c(0.85, 1), labels = c("A", "B"), label_size = 20)
  
  
  
  if (save == TRUE){
    ggsave(here("figures/Fig_4_LFI_diversity.png"), dpi = 1000, height = height_value, width = width_value)
  }
}

