#' Function to plot changes in biomass and catch relative to before MPA establishment for small pelagic fish
#'
#' 
#' @param data OSMOSE-MED simulation outputs of total biomass and total catch by groups for each fishing redistribution strategy
#' 
#' @return Supplementary figure S6. 7 - small pelagics change with sd for S1, S4-S6
#'
#' @export
#' 


plot.small.pelagics.biomass.change.inside.FPAs.with.sd <- function(biomass_by_groups, f = 2){

  # Without MPAs
  Biom_small_pelagics_beforeMPAs = biomass_by_groups %>%
    filter(mpa == "Inside MPAs")
  
# Lit scenarios
  Biom_small_pelagics_lit_current = biomass_by_groups %>%
    filter(mpa == "Inside MPAs") %>%
    filter(group == "Small-sized pelagic fish") %>%
    filter(scenario %in% c("Micheli", "Existing network", "Mazor scenario 8", "Mazor scenario 9")) %>%
    mutate(biom_diff = tot.biomass - tot.biomass.ref, 
           biom_rel_change = (tot.biomass - tot.biomass.ref)/ tot.biomass.ref * 100) %>%
    
    group_by(mpa.coverage, scenario) %>%
    summarise(mean_rel_change = mean(biom_rel_change),
              sd_rel_change = sd(biom_rel_change))
  
  
  # Random - basin
  Biom_small_pelagics_random = biomass_by_groups %>%
    filter(grepl('Random', scenario)) %>%
    filter(mpa == "Inside MPAs") %>%
    filter(group == "Small-sized pelagic fish") %>%
    mutate(biom_diff = tot.biomass - tot.biomass.ref, 
           biom_rel_change = (tot.biomass - tot.biomass.ref)/ tot.biomass.ref * 100,
           sd_rel_change = sd(biom_rel_change)) %>%
    
    group_by(mpa.coverage, scenario) %>%
    summarise(mean_rel_change = mean(biom_rel_change),
              mean_sd_rel_change = mean(sd_rel_change),
              shapiro_p_value = shapiro.test(biom_rel_change)$p.value,  # Extract p-value - not normal
              shapiro_statistic = shapiro.test(biom_rel_change)$statistic) %>%
    
    ungroup() %>%
    group_by(mpa.coverage) %>%
    mutate(high = max(mean_rel_change),
           low = min(mean_rel_change),
           mean = mean(mean_rel_change)) %>%
    mutate(scenario_group = "Random")
  
  
  Biom_small_pelagics_EEZ = biomass_by_groups %>%
    filter(grepl('EEZ', scenario)) %>%
    filter(mpa == "Inside MPAs") %>%
    filter(group == "Small-sized pelagic fish") %>%
    mutate(biom_diff = tot.biomass - tot.biomass.ref, 
           biom_rel_change = (tot.biomass - tot.biomass.ref)/ tot.biomass.ref * 100,
           sd_rel_change = sd(biom_rel_change)) %>%
    
    group_by(mpa.coverage, scenario) %>%
    summarise(mean_rel_change = mean(biom_rel_change),
              mean_sd_rel_change = mean(sd_rel_change),
              shapiro_p_value = shapiro.test(biom_rel_change)$p.value,  # Extract p-value - not normal
              shapiro_statistic = shapiro.test(biom_rel_change)$statistic) %>%
    
    ungroup() %>%
    group_by(mpa.coverage) %>%
    mutate(high = max(mean_rel_change),
           low = min(mean_rel_change),
           mean = mean(mean_rel_change)) %>%
    mutate(scenario_group = "EEZ-scale conservation")
  
  
  ### PLOT
  
  seuil_small_pel_inside_rel_change <- ggplot() +
    
    # geom_line(data = Biom_small_pelagics_random, aes(x = mpa.coverage, y = mean_rel_change, colour = scenario), lwd = 0.1, alpha=1) +
    # geom_ribbon(data = Biom_small_pelagics_random, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    # 
    # geom_line(data = Biom_small_pelagics_EEZ, aes(x = mpa.coverage, y = mean_rel_change, colour = scenario), lwd = 0.1, alpha=1) +
    # geom_ribbon(data = Biom_small_pelagics_EEZ, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    # 
    geom_line(data = Biom_small_pelagics_lit_current, aes(x = mpa.coverage, y = mean_rel_change, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Biom_small_pelagics_lit_current, aes(x = mpa.coverage, ymin = mean_rel_change - sd_rel_change, ymax = mean_rel_change + sd_rel_change, fill = scenario), alpha = 0.2) +
    
    
    geom_line(data = Biom_small_pelagics_EEZ, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.5, lty = "dashed") +

    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) +  #change legend text font size 
    scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    scale_fill_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    labs(x ="FPA coverage (%)", y = "Biomass change (%)", title = "Small pelagic fish", subtitle = "Inside FPAs") +
    theme(legend.position = "bottom") +
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10))
  
  
  return(seuil_small_pel_inside_rel_change)
  
  if (save == TRUE){
    ggsave(here("figures/appendix/Fig_3B_small_pelagic_fish_with_variability_S1_S4.png"), dpi = 1000, height = 10, width = 10)
    
  }
}

