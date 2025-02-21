#' Function to plot total biomass and catch changes across the Mediterranean Sea
#'
#' 
#' @param data OSMOSE-MED simulation outputs of total biomass and total catch for each fishing redistribution strategy
#' 
#' @return Figure 1 (main text)
#'
#' @export
#' 

plot.biomass.catch.change.Med = function(Biom_NoCC_GSA_prop, Biom_NoCC_GSA_uniform, Biom_NoCC_fline, Catch_NoCC_GSA_prop, Catch_NoCC_GSA_uniform, Catch_NoCC_fline, save = TRUE){
  
  # Biomass change at Med Sea scale --------------------------------------------
  
  Biom_lit_scenarios = rbind(Biom_NoCC_GSA_prop, Biom_NoCC_GSA_uniform, Biom_NoCC_fline) %>%
    filter(scenario %in% c("Mazor scenario 8", "Micheli", "Mazor scenario 9", "Existing network")) %>%
    dplyr::select(tot.biomass, mpa.coverage, scenario_rep, scenario, Fishing_scenario) %>%
    group_by(mpa.coverage, scenario, Fishing_scenario) %>%
    summarise(mean_biom_mpa = mean(tot.biomass))
  
  Biomass_noMPA = Biom_lit_scenarios$mean_biom_mpa[1]
  
  Biom_lit_scenarios_noCC = Biom_lit_scenarios %>%
    mutate(mean_biom_nompa = Biomass_noMPA,
           rel_biom_change = (mean_biom_mpa - mean_biom_nompa)/mean_biom_nompa * 100)
  
  
  # Random ------------
  Biom_random_scenarios_noCC = rbind(Biom_NoCC_GSA_prop, Biom_NoCC_GSA_uniform, Biom_NoCC_fline) %>%
    filter(grepl('Random', scenario)) %>%
    dplyr::select(tot.biomass, mpa.coverage, scenario_rep, scenario, Fishing_scenario) %>%
    group_by(mpa.coverage, scenario, Fishing_scenario) %>%
    summarise(mean_biom_mpa = mean(tot.biomass)) %>%
    ungroup() %>%
    mutate(mean_biom_nompa = Biomass_noMPA,
           rel_biom_change = (mean_biom_mpa - mean_biom_nompa)/mean_biom_nompa * 100) %>%
    group_by(mpa.coverage, Fishing_scenario) %>%
    mutate(high = max(rel_biom_change),
           low = min(rel_biom_change),
           mean = mean(rel_biom_change)) %>%
    mutate(scenario_group = "Random")
  
  
  # EEZ ------------
  Biom_EEZ_scenarios_noCC = rbind(Biom_NoCC_GSA_prop, Biom_NoCC_GSA_uniform, Biom_NoCC_fline) %>%
    filter(grepl('EEZ', scenario)) %>%
    dplyr::select(tot.biomass, mpa.coverage, scenario_rep, scenario, Fishing_scenario) %>%
    group_by(mpa.coverage, scenario, Fishing_scenario) %>%
    summarise(mean_biom_mpa = mean(tot.biomass)) %>%
    ungroup() %>%
    mutate(mean_biom_nompa = Biomass_noMPA,
           rel_biom_change = (mean_biom_mpa - mean_biom_nompa)/mean_biom_nompa * 100) %>%
    group_by(mpa.coverage, Fishing_scenario) %>%
    mutate(high = max(rel_biom_change),
           low = min(rel_biom_change),
           mean = mean(rel_biom_change)) %>%
    mutate(scenario_group = "EEZ-scale conservation")
  
  
  # Catch change at Med Sea scale --------------------------------------------
  
  Catch_lit_scenarios = rbind(Catch_NoCC_GSA_prop, Catch_NoCC_GSA_uniform, Catch_NoCC_fline) %>%
    filter(scenario %in% c("Mazor scenario 8", "Micheli", "Mazor scenario 9", "Existing network")) %>%
    dplyr::select(tot.catches, mpa.coverage, scenario_rep, scenario, Fishing_scenario) %>%
    group_by(mpa.coverage, scenario, Fishing_scenario) %>%
    summarise(mean_catch_mpa = mean(tot.catches))
  
  Catches_noMPA = Catch_lit_scenarios$mean_catch_mpa[1]
  
  Catch_lit_scenarios_noCC = Catch_lit_scenarios %>%
    mutate(mean_catch_nompa = Catches_noMPA,
           rel_catch_change = (mean_catch_mpa - mean_catch_nompa)/mean_catch_nompa * 100)
  
  
  # Random ------------
  Catch_random_scenarios_noCC = rbind(Catch_NoCC_GSA_prop, Catch_NoCC_GSA_uniform, Catch_NoCC_fline) %>%
    filter(grepl('Random', scenario)) %>%
    dplyr::select(tot.catches, mpa.coverage, scenario_rep, scenario, Fishing_scenario) %>%
    group_by(mpa.coverage, scenario, Fishing_scenario) %>%
    summarise(mean_catch_mpa = mean(tot.catches)) %>%
    ungroup() %>%
    mutate(mean_catch_nompa = Catches_noMPA,
           rel_catch_change = (mean_catch_mpa - mean_catch_nompa)/mean_catch_nompa * 100) %>%
    group_by(mpa.coverage, Fishing_scenario) %>%
    mutate(high = max(rel_catch_change),
           low = min(rel_catch_change),
           mean = mean(rel_catch_change)) %>%
    mutate(scenario_group = "Random")
  
  
  # EEZ ------------
  Catch_EEZ_scenarios_noCC = rbind(Catch_NoCC_GSA_prop, Catch_NoCC_GSA_uniform, Catch_NoCC_fline) %>%
    filter(grepl('EEZ', scenario)) %>%
    dplyr::select(tot.catches, mpa.coverage, scenario_rep, scenario, Fishing_scenario) %>%
    group_by(mpa.coverage, scenario, Fishing_scenario) %>%
    summarise(mean_catch_mpa = mean(tot.catches)) %>%
    ungroup() %>%
    mutate(mean_catch_nompa = Catches_noMPA,
           rel_catch_change = (mean_catch_mpa - mean_catch_nompa)/mean_catch_nompa * 100) %>%
    group_by(mpa.coverage, Fishing_scenario) %>%
    mutate(high = max(rel_catch_change),
           low = min(rel_catch_change),
           mean = mean(rel_catch_change)) %>%
    mutate(scenario_group = "EEZ-scale conservation")
  
  
  # FIGURES --------------------------------------------------------------------
  # TOTAL BIOMASS --------------------------------------------------------------
  
  # Uniform by GSA
  Biom_random_scenarios_noCC_GSAprop = Biom_random_scenarios_noCC %>%
    filter(Fishing_scenario == "Proportional by GSA")
  Biom_EEZ_scenarios_noCC_GSAprop = Biom_EEZ_scenarios_noCC %>%
    filter(Fishing_scenario == "Proportional by GSA")
  Biom_lit_scenarios_noCC_GSAprop = Biom_lit_scenarios_noCC %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  biom_change_GSAprop <- ggplot() +
    
    geom_line(data = Biom_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Biom_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Biom_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Biom_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Biom_lit_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = rel_biom_change, colour = scenario), lwd = 0.8, alpha=1) +
    
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
    labs(x ="MPA coverage (%)", y = "Biomass change (%)", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(-1, 10)
  
  # Uniform by GSA
  Biom_random_scenarios_noCC_GSAprop = Biom_random_scenarios_noCC %>%
    filter(Fishing_scenario == "Uniform by GSA")
  Biom_EEZ_scenarios_noCC_GSAprop = Biom_EEZ_scenarios_noCC %>%
    filter(Fishing_scenario == "Uniform by GSA")
  Biom_lit_scenarios_noCC_GSAprop = Biom_lit_scenarios_noCC %>%
    filter(Fishing_scenario == "Uniform by GSA")
  
  biom_change_GSAuniform <- ggplot() +
    
    geom_line(data = Biom_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), linetype = "dashed", lwd = 0.8, alpha=1) +
    geom_ribbon(data = Biom_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Biom_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "dashed") +
    geom_ribbon(data = Biom_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Biom_lit_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = rel_biom_change, colour = scenario), lwd = 0.8, alpha=1, linetype = "dashed") +
    
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
    labs(x ="MPA coverage (%)", y = "Biomass change (%)", title = "Uniform by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(-1, 10)
  
  
  Biom_random_scenarios_noCC_GSAprop = Biom_random_scenarios_noCC %>%
    filter(Fishing_scenario == "Fishing-the-line")
  Biom_EEZ_scenarios_noCC_GSAprop = Biom_EEZ_scenarios_noCC %>%
    filter(Fishing_scenario == "Fishing-the-line")
  Biom_lit_scenarios_noCC_GSAprop = Biom_lit_scenarios_noCC %>%
    filter(Fishing_scenario == "Fishing-the-line")
  
  biom_change_fline <- ggplot() +
    
    geom_line(data = Biom_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), linetype = "dotted", lwd = 0.8, alpha=1) +
    geom_ribbon(data = Biom_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Biom_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "dotted") +
    geom_ribbon(data = Biom_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Biom_lit_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = rel_biom_change, colour = scenario), lwd = 0.8, alpha=1, linetype = "dotted") +
    
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
    labs(x ="MPA coverage (%)", y = "Biomass change (%)", title = "Fishing-the-line") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(-1, 10)
  
  # TOTAL CATCHES ----------------------------------------------------------------
  
  # Proportional by GSA
  Catch_random_scenarios_noCC_GSAprop = Catch_random_scenarios_noCC %>%
    filter(Fishing_scenario == "Proportional by GSA")
  Catch_EEZ_scenarios_noCC_GSAprop = Catch_EEZ_scenarios_noCC %>%
    filter(Fishing_scenario == "Proportional by GSA")
  Catch_lit_scenarios_noCC_GSAprop = Catch_lit_scenarios_noCC %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  catch_change_GSAprop <- ggplot() +
    
    geom_line(data = Catch_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Catch_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Catch_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Catch_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Catch_lit_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = rel_catch_change, colour = scenario), lwd = 0.8, alpha=1) +
    
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
    labs(x ="MPA coverage (%)", y = "Catch change (%)", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
    ylim(-26.5, 1)
  
  
  # Uniform by GSA
  Catch_random_scenarios_noCC_GSAprop = Catch_random_scenarios_noCC %>%
    filter(Fishing_scenario == "Uniform by GSA")
  Catch_EEZ_scenarios_noCC_GSAprop = Catch_EEZ_scenarios_noCC %>%
    filter(Fishing_scenario == "Uniform by GSA")
  Catch_lit_scenarios_noCC_GSAprop = Catch_lit_scenarios_noCC %>%
    filter(Fishing_scenario == "Uniform by GSA")
  
  catch_change_GSAuniform <- ggplot() +
    
    geom_line(data = Catch_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), linetype = "dashed", lwd = 0.8, alpha=1) +
    geom_ribbon(data = Catch_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Catch_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "dashed") +
    geom_ribbon(data = Catch_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Catch_lit_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = rel_catch_change, colour = scenario), lwd = 0.8, alpha=1, linetype = "dashed") +
    
    #geom_line(data = Catch_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.5, lty = "solid") +
    
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    scale_fill_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    labs(x ="MPA coverage (%)", y = "Catch change (%)", title = "Uniform by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(-26.5, 1)
  
  
  
  Catch_random_scenarios_noCC_GSAprop = Catch_random_scenarios_noCC %>%
    filter(Fishing_scenario == "Fishing-the-line")
  Catch_EEZ_scenarios_noCC_GSAprop = Catch_EEZ_scenarios_noCC %>%
    filter(Fishing_scenario == "Fishing-the-line")
  Catch_lit_scenarios_noCC_GSAprop = Catch_lit_scenarios_noCC %>%
    filter(Fishing_scenario == "Fishing-the-line")
  
  catch_change_fline <- ggplot() +
    
    geom_line(data = Catch_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), linetype = "dotted", lwd = 0.8, alpha=1) +
    geom_ribbon(data = Catch_random_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Catch_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "dotted") +
    geom_ribbon(data = Catch_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Catch_lit_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = rel_catch_change, colour = scenario), lwd = 0.8, alpha=1, linetype = "dotted") +
    
    #geom_line(data = Catch_EEZ_scenarios_noCC_GSAprop, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.5, lty = "solid") +
    
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    scale_fill_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) +
    labs(x ="MPA coverage (%)", y = "Catch change (%)", title = "Fishing-the-line") +
    theme(legend.position = "bottom") +

    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(-26.5, 1)
  
  
  
  ## Figure 1 (main text) --------------------------------------------------------
  biom_change_GSAprop_nolegend = biom_change_GSAprop + theme(legend.position = "none", axis.title.x=element_blank())
  biom_change_GSAuniform_nolegend = biom_change_GSAuniform + theme(legend.position = "none", axis.title.y=element_blank(), axis.title.x=element_blank())
  biom_change_fline_nolegend = biom_change_fline + theme(legend.position = "none", axis.title.y=element_blank(), axis.title.x=element_blank())
  
  
  # Fig 1B
  catch_change_GSAprop_nolegend = catch_change_GSAprop + theme(legend.position = "none", plot.title = element_blank())
  catch_change_GSAuniform_nolegend = catch_change_GSAuniform + theme(legend.position = "none", axis.title.y=element_blank(), plot.title = element_blank())
  catch_change_fline_nolegend = catch_change_fline + theme(legend.position = "none", axis.title.y=element_blank(), plot.title = element_blank())
  
  
  legend <- get_legend(biom_change_GSAprop)
  
  
  
  # Fig 1
  catch_change_GSAprop_m = catch_change_GSAprop + theme(legend.title = element_blank(), plot.title = element_blank(), legend.text = element_text(size=14))
  catch_change_GSAuniform_m = catch_change_GSAuniform + theme(legend.title = element_blank(), plot.title = element_blank(), legend.text = element_text(size=14), axis.title.y=element_blank())
  catch_change_fline_m = catch_change_fline + theme(legend.title = element_blank(), plot.title = element_blank(), legend.text = element_text(size=14), axis.title.y=element_blank())
  
  fig1A <- ggarrange(biom_change_GSAprop_nolegend, biom_change_GSAuniform_nolegend, biom_change_fline_nolegend,
                     ncol=3,
                     nrow=1,
                     align = "hv") 
  
  fig1B <- ggarrange(catch_change_GSAprop_m, catch_change_GSAuniform_m, catch_change_fline_m,
                     ncol=3,
                     nrow=1,
                     common.legend = T,
                     legend = "bottom",
                     align = "hv") 
  
  fig1 <- plot_grid(fig1A, fig1B, ncol = 1, nrow = 2, rel_heights = c(0.85, 1), labels = c("A", "B"), label_size = 20)
  
  if (save == TRUE){
    ggsave(here("figures/Fig_1_total_biomass_catch.png"), dpi = 1000, height = 10, width = 10)
    
  }
  
}








