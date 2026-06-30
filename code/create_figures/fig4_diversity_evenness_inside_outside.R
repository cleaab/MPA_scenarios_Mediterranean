#' Function to plot LFI and alpha-taxonomic divesity inside MPAs
#'
#' 
#' @param data OSMOSE-MED simulation outputs of Large Fish Indicator and alpha-taxonomic divesity
#' 
#' @return Figure 4 (main text)
#'
#' @export
#' 

# corrected exp H function - calculated on replicates and then averaged
calculate.exp.H <- function(){
  # add 0%:
  # Abundance map before MPAs
  nc_abundance_spatial  = nc_open(here("data/spatial/Abundance_nompa_scenario.nc"))
  abundance_spatial  = ncvar_get(nc_abundance_spatial, "Abundance")
  nc_close(nc_abundance_spatial)  # ← close immediately after reading
  
  abundance_spatial_tot_sp = apply(abundance_spatial, c(1,2,4), sum, na.rm = T) # nombre tot d'individus
  
  # Shannon index: sum over all species of -(p * log(p))
  # Initialize H_sum [190, 83, 30] with zeros
  H_sum = array(0, dim = c(190, 83, 30))
  
  for (i in 1:101){
    # Extract species i: [190, 83, 30], drop=FALSE not needed since dim 3 = 1 will auto-drop
    sp_abundance = abundance_spatial[1:190, 1:83, i, 1:30]  # [190, 83, 30]
    
    p_sp = sp_abundance / abundance_spatial_tot_sp           # [190, 83, 30]
    
    # Suppress log(0): 0*log(0) = 0 by convention in Shannon
    H_sp = ifelse(p_sp > 0, -(p_sp * log(p_sp)), 0)         # [190, 83, 30]
    
    H_sum = H_sum + H_sp
  }
      
  # Set cells with no abundance to NA
  H_sum[abundance_spatial_tot_sp == 0] = NA
  
  exp_H_spatial = exp(H_sum)
  
  return(exp_H_spatial)
}

calculate.evenness <- function(){
  # add 0%:
  # Abundance map before MPAs
  nc_abundance_spatial  = nc_open(here("data/spatial/Abundance_nompa_scenario.nc"))
  abundance_spatial  = ncvar_get(nc_abundance_spatial, "Abundance")
  nc_close(nc_abundance_spatial)  # ← close immediately after reading
  
  abundance_spatial_tot_sp = apply(abundance_spatial, c(1,2,4), sum, na.rm = T) # nombre tot d'individus
  
  # Shannon index: sum over all species of -(p * log(p))
  # Initialize H_sum [190, 83, 30] with zeros
  H_sum = array(0, dim = c(190, 83, 30))
  
  for (i in 1:101){
    # Extract species i: [190, 83, 30], drop=FALSE not needed since dim 3 = 1 will auto-drop
    sp_abundance = abundance_spatial[1:190, 1:83, i, 1:30]  # [190, 83, 30]
    
    p_sp = sp_abundance / abundance_spatial_tot_sp           # [190, 83, 30]
    
    # Suppress log(0): 0*log(0) = 0 by convention in Shannon
    H_sp = ifelse(p_sp > 0, -(p_sp * log(p_sp)), 0)         # [190, 83, 30]
    
    H_sum = H_sum + H_sp
  }
  
  # Set cells with no abundance to NA
  H_sum[abundance_spatial_tot_sp == 0] = NA
  
  exp_H_spatial = exp(H_sum)
  
  # Species richness
  Hill0_spatial <- apply(abundance_spatial > 0, c(1,2, 4), sum, na.rm = TRUE)
  
  #evenness
  evenness_spatial = H_sum/log(Hill0_spatial)
  
  return(evenness_spatial)
}

plot.diversity.inside.outside <- function(Mean_diversity_Med_GSA_prop_lit, Mean_diversity_Med_GSA_prop_random_basin, Mean_diversity_Med_GSA_prop_random_EEZ){
  
  # calculate Hill-Shannon diversity index - before MPAs
  exp_H_spatial = calculate.exp.H()
  exp_H_noMPA = apply(exp_H_spatial, c(3), mean, na.rm = T)
  length(exp_H_noMPA)

  zero_pct = data.frame(exp_H_noMPA) %>%
    rename(mean_exp_H = exp_H_noMPA) %>%
    mutate(rep = seq_along(exp_H_noMPA)) %>%
    mutate(mpa_coverage = 0)

  # Lit scenarios
  zero_pct_lit_ed <- bind_rows(replicate(4, zero_pct, simplify = FALSE)) %>%
    mutate(mpa_scenario = rep(c("Mazor scenario 8", "Mazor scenario 9", "Micheli", "Existing network"), each = nrow(zero_pct)))
  
  Mean_diversity_Med_GSA_prop_lit_ed = Mean_diversity_Med_GSA_prop_lit %>%
    dplyr::select(mean_exp_H, rep, mpa_coverage, mpa_scenario)
  
  Mean_diversity_Med_GSA_prop_lit_with_zero = rbind(zero_pct_lit_ed, Mean_diversity_Med_GSA_prop_lit_ed) %>%
    group_by(mpa_coverage, mpa_scenario) %>%
    summarize(exp_H_mean = mean(mean_exp_H, na.rm = T),
              exp_H_sd = sd(mean_exp_H, na.rm = T))
  
  # Random - basin
  zero_pct_random_ed <- bind_rows(replicate(10, zero_pct, simplify = FALSE)) %>%
    mutate(mpa_scenario = rep(c("Random-rep11", "Random-rep12", "Random-rep13", "Random-rep14", "Random-rep15", "Random-rep16", "Random-rep17", "Random-rep18", "Random-rep19", "Random-rep20"), each = nrow(zero_pct)))
  
  Mean_diversity_Med_GSA_prop_random_basin_ed = Mean_diversity_Med_GSA_prop_random_basin %>%
    dplyr::select(mean_exp_H, rep, mpa_coverage, mpa_scenario)
  
  Mean_diversity_Med_GSA_prop_random_basin_with_zero = rbind(zero_pct_random_ed, Mean_diversity_Med_GSA_prop_random_basin_ed) %>%
    group_by(mpa_coverage, mpa_scenario) %>%
    summarize(exp_H_mean = mean(mean_exp_H, na.rm = T),
              exp_H_sd = sd(mean_exp_H, na.rm = T)) %>%
    ungroup() %>%
    group_by(mpa_coverage) %>%
    mutate(high = max(exp_H_mean, na.rm = T),
           low = min(exp_H_mean, na.rm = T)) %>%
    ungroup() %>%
    mutate(scenario_group = "Random")
  
  Mean_diversity_Med_GSA_prop_random_basin_with_zero$mpa_scenario <- gsub("-rep", " ", Mean_diversity_Med_GSA_prop_random_basin_with_zero$mpa_scenario)
  
  # Random - EEZ
  zero_pct_random_EEZ_ed <- bind_rows(replicate(10, zero_pct, simplify = FALSE)) %>%
    mutate(mpa_scenario = rep(c("EEZ-conservation-rep1", "EEZ-conservation-rep2", "EEZ-conservation-rep3", "EEZ-conservation-rep4", "EEZ-conservation-rep5", "EEZ-conservation-rep6", "EEZ-conservation-rep7", "EEZ-conservation-rep8", "EEZ-conservation-rep9", "EEZ-conservation-rep10"), each = nrow(zero_pct)))
  
  Mean_diversity_Med_GSA_prop_random_EEZ_ed = Mean_diversity_Med_GSA_prop_random_EEZ %>%
    dplyr::select(mean_exp_H, rep, mpa_coverage, mpa_scenario)
  
  Mean_diversity_Med_GSA_prop_random_EEZ_with_zero = rbind(zero_pct_random_EEZ_ed, Mean_diversity_Med_GSA_prop_random_EEZ_ed) %>%
    group_by(mpa_coverage, mpa_scenario) %>%
    summarize(exp_H_mean = mean(mean_exp_H, na.rm = T),
              exp_H_sd = sd(mean_exp_H, na.rm = T)) %>%
    ungroup() %>%
    group_by(mpa_coverage) %>%
    mutate(high = max(exp_H_mean, na.rm = T),
           low = min(exp_H_mean, na.rm = T)) %>%
    ungroup() %>%
    mutate(scenario_group = "EEZ-conservation")
  
  Mean_diversity_Med_GSA_prop_random_EEZ_with_zero$mpa_scenario <- gsub("-rep", " ", Mean_diversity_Med_GSA_prop_random_EEZ_with_zero$mpa_scenario)
  
  # Entire Med  -------------------------------------------------------------
  
  ### PLOT
  
  Diversity_Med_after_MPAs_GSAprop <- ggplot() +
    
    geom_line(data = Mean_diversity_Med_GSA_prop_random_basin_with_zero, aes(x = mpa_coverage, y = exp_H_mean, colour = mpa_scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = Mean_diversity_Med_GSA_prop_random_basin_with_zero, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Mean_diversity_Med_GSA_prop_random_EEZ_with_zero, aes(x = mpa_coverage, y = exp_H_mean, colour = mpa_scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = Mean_diversity_Med_GSA_prop_random_EEZ_with_zero, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Mean_diversity_Med_GSA_prop_lit_with_zero, aes(x = mpa_coverage, y = exp_H_mean, colour = mpa_scenario), lwd = 0.8, alpha=1) +
    geom_vline(xintercept = 10, lty = "dashed", colour = "gray40", lwd = 0.6) +
    
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    scale_fill_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    labs(x ="FPA coverage (%)", y = "Taxonomic diversity", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) 
  
  return(Diversity_Med_after_MPAs_GSAprop)

}

plot.evenness.inside.outside <- function(Mean_diversity_Med_GSA_prop_lit, Mean_diversity_Med_GSA_prop_random_basin, Mean_diversity_Med_GSA_prop_random_EEZ){
  
  # calculate evenness - before MPAs
  evenness_spatial = calculate.evenness()
  evenness_noMPA = apply(evenness_spatial, c(3), mean, na.rm = T)
  length(evenness_noMPA)
  
  zero_pct = data.frame(evenness_noMPA) %>%
    rename(mean_evenness = evenness_noMPA) %>%
    mutate(rep = seq_along(evenness_noMPA)) %>%
    mutate(mpa_coverage = 0)
  
  # Lit scenarios
  zero_pct_lit_ed <- bind_rows(replicate(4, zero_pct, simplify = FALSE)) %>%
    mutate(mpa_scenario = rep(c("Mazor scenario 8", "Mazor scenario 9", "Micheli", "Existing network"), each = nrow(zero_pct)))
  
  Mean_evenness_Med_GSA_prop_lit_ed = Mean_diversity_Med_GSA_prop_lit %>%
    dplyr::select(mean_evenness, rep, mpa_coverage, mpa_scenario)
  
  Mean_evenness_Med_GSA_prop_lit_with_zero = rbind(zero_pct_lit_ed, Mean_evenness_Med_GSA_prop_lit_ed) %>%
    group_by(mpa_coverage, mpa_scenario) %>%
    summarize(evenness_mean = mean(mean_evenness, na.rm = T),
              evenness_sd = sd(mean_evenness, na.rm = T))
  
  # Random - basin
  zero_pct_random_ed <- bind_rows(replicate(10, zero_pct, simplify = FALSE)) %>%
    mutate(mpa_scenario = rep(c("Random-rep11", "Random-rep12", "Random-rep13", "Random-rep14", "Random-rep15", "Random-rep16", "Random-rep17", "Random-rep18", "Random-rep19", "Random-rep20"), each = nrow(zero_pct)))
  
  Mean_evenness_Med_GSA_prop_random_basin_ed = Mean_diversity_Med_GSA_prop_random_basin %>%
    dplyr::select(mean_evenness, rep, mpa_coverage, mpa_scenario)
  
  Mean_evenness_Med_GSA_prop_random_basin_with_zero = rbind(zero_pct_random_ed, Mean_evenness_Med_GSA_prop_random_basin_ed) %>%
    group_by(mpa_coverage, mpa_scenario) %>%
    summarize(evenness_mean = mean(mean_evenness, na.rm = T),
              evenness_sd = sd(mean_evenness, na.rm = T)) %>%
    ungroup() %>%
    group_by(mpa_coverage) %>%
    mutate(high = max(evenness_mean, na.rm = T),
           low = min(evenness_mean, na.rm = T)) %>%
    ungroup() %>%
    mutate(scenario_group = "Random")
  
  Mean_evenness_Med_GSA_prop_random_basin_with_zero$mpa_scenario <- gsub("-rep", " ", Mean_evenness_Med_GSA_prop_random_basin_with_zero$mpa_scenario)
  
  # Random - EEZ
  zero_pct_random_EEZ_ed <- bind_rows(replicate(10, zero_pct, simplify = FALSE)) %>%
    mutate(mpa_scenario = rep(c("EEZ-conservation-rep1", "EEZ-conservation-rep2", "EEZ-conservation-rep3", "EEZ-conservation-rep4", "EEZ-conservation-rep5", "EEZ-conservation-rep6", "EEZ-conservation-rep7", "EEZ-conservation-rep8", "EEZ-conservation-rep9", "EEZ-conservation-rep10"), each = nrow(zero_pct)))
  
  Mean_evenness_Med_GSA_prop_random_EEZ_ed = Mean_diversity_Med_GSA_prop_random_EEZ %>%
    dplyr::select(mean_evenness, rep, mpa_coverage, mpa_scenario)
  
  Mean_evenness_Med_GSA_prop_random_EEZ_with_zero = rbind(zero_pct_random_EEZ_ed, Mean_evenness_Med_GSA_prop_random_EEZ_ed) %>%
    group_by(mpa_coverage, mpa_scenario) %>%
    summarize(evenness_mean = mean(mean_evenness, na.rm = T),
              evenness_sd = sd(mean_evenness, na.rm = T)) %>%
    ungroup() %>%
    group_by(mpa_coverage) %>%
    mutate(high = max(evenness_mean, na.rm = T),
           low = min(evenness_mean, na.rm = T)) %>%
    ungroup() %>%
    mutate(scenario_group = "EEZ-conservation")
  
  Mean_evenness_Med_GSA_prop_random_EEZ_with_zero$mpa_scenario <- gsub("-rep", " ", Mean_evenness_Med_GSA_prop_random_EEZ_with_zero$mpa_scenario)
  
  # Entire Med  -------------------------------------------------------------
  
  ### PLOT
  
  Evenness_Med_after_MPAs_GSAprop <- ggplot() +
    
    geom_line(data = Mean_evenness_Med_GSA_prop_random_basin_with_zero, aes(x = mpa_coverage, y = evenness_mean, colour = mpa_scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = Mean_evenness_Med_GSA_prop_random_basin_with_zero, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Mean_evenness_Med_GSA_prop_random_EEZ_with_zero, aes(x = mpa_coverage, y = evenness_mean, colour = mpa_scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = Mean_evenness_Med_GSA_prop_random_EEZ_with_zero, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Mean_evenness_Med_GSA_prop_lit_with_zero, aes(x = mpa_coverage, y = evenness_mean, colour = mpa_scenario), lwd = 0.8, alpha=1) +
    geom_vline(xintercept = 10, lty = "dashed", colour = "gray40", lwd = 0.6) +
    
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    scale_fill_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    labs(x ="FPA coverage (%)", y = "Evenness", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) 
  
  return(Evenness_Med_after_MPAs_GSAprop)
  
}

plot.fig4.suppmat <- function(height_value, width_value, save){
  

  ## Diversity ---------------------------------
  
  Diversity_inside_GSAprop = plot.diversity.inside.outside(Mean_diversity_inside_GSA_prop_lit, Mean_diversity_inside_GSA_prop_random_basin, Mean_diversity_inside_GSA_prop_random_EEZ)
  
  Diversity_inside_GSAprop_mod <- Diversity_inside_GSAprop +
    labs(title = "Inside MPAs") + theme(

    legend.text = element_text(size=14),
    legend.title = element_blank(),
    plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) + 
    
    theme(plot.margin = margin(0,0,0.5,0.5, "cm")) # 2 -right, 4 - left
  
  Diversity_outside_GSAprop = plot.diversity.inside.outside(Mean_diversity_outside_GSA_prop_lit, Mean_diversity_outside_GSA_prop_random_basin, Mean_diversity_outside_GSA_prop_random_EEZ)
  
  Diversity_outside_GSAprop_mod <- Diversity_outside_GSAprop + 
    labs(title = "Outside MPAs") + theme(
      
    legend.text = element_text(size=14),
    legend.title = element_blank(),
    plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) + 
    theme(plot.margin = margin(0,0,0.5,0.5, "cm")) # 2 -right, 4 - left
  
  ## Evenness ---------------------------------
  
  Evenness_inside_GSAprop = plot.evenness.inside.outside(Mean_diversity_inside_GSA_prop_lit, Mean_diversity_inside_GSA_prop_random_basin, Mean_diversity_inside_GSA_prop_random_EEZ)
  
  Evenness_inside_GSAprop_mod <- Evenness_inside_GSAprop + theme(
    
    legend.text = element_text(size=14),
    legend.title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic")) + 
    theme(plot.margin = margin(0,0,0.5,0.5, "cm")) # 2 -right, 4 - left
  
  Evenness_outside_GSAprop = plot.evenness.inside.outside(Mean_diversity_outside_GSA_prop_lit, Mean_diversity_outside_GSA_prop_random_basin, Mean_diversity_outside_GSA_prop_random_EEZ)
  
  Evenness_outside_GSAprop_mod <- Evenness_outside_GSAprop + theme(
    
    legend.text = element_text(size=14),
    legend.title = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic")) + 
    theme(plot.margin = margin(0,0,0.5,0.5, "cm")) # 2 -right, 4 - left
  

  fig4_supmat <- ggarrange(Diversity_inside_GSAprop_mod, Diversity_outside_GSAprop_mod,
                           Evenness_inside_GSAprop_mod,Evenness_outside_GSAprop_mod,
                     ncol=2,
                     nrow=2,
                     common.legend = T,
                     legend = "bottom",
                     align = "v",
                    labels = c("A", "B", "C", "D"),      # ← add this
                    font.label = list(size = 14, face = "bold"))  # ← optional styling
  
  
  
  if (save == TRUE){
    ggsave(here("figures/Fig_4_LFI_diversity_evenness_inside_outside_with_sd.png"), dpi = 1000, height = 11, width = 11)
  }
}


