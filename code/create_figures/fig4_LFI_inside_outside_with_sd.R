#' Function to plot LFI and alpha-taxonomic divesity inside MPAs
#'
#' 
#' @param data OSMOSE-MED simulation outputs of Large Fish Indicator and alpha-taxonomic divesity
#' 
#' @return Figure 4 (main text)
#'
#' @export
#' 

plot.LFI.Med.with.sd <- function(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline){
  
  LFI_all_fishing_scenarios_lit = rbind(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline) %>%
    filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
    mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
    group_by(mpa_coverage, mpa, scenario, Fishing_scenario) %>%
    summarise(mean_LFI_reps = mean(mean_LFI_60_years),
              sd_LFI_reps = sd(mean_LFI_60_years))
  
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
  
  ## Med
  
  LFI_all_fishing_scenarios_lit_Med = LFI_all_fishing_scenarios_lit %>%
    filter(mpa == "Entire Mediterranean Sea")
  
  LFI_all_fishing_scenarios_Random_Med = LFI_all_fishing_scenarios_Random %>%
    filter(mpa == "Entire Mediterranean Sea")
  
  LFI_all_fishing_scenarios_EEZ_Med = LFI_all_fishing_scenarios_EEZ %>%
    filter(mpa == "Entire Mediterranean Sea")
  ## Outside
  # GSA prop
  LFI_all_fishing_scenarios_lit_Med_GSAprop = LFI_all_fishing_scenarios_lit %>%
    filter(mpa == "Entire Mediterranean Sea") %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  LFI_all_fishing_scenarios_Random_Med_GSAprop = LFI_all_fishing_scenarios_Random %>%
    filter(mpa == "Entire Mediterranean Sea") %>%
    filter(Fishing_scenario == "Proportional by GSA") 
  
  
  LFI_all_fishing_scenarios_EEZ_Med_GSAprop = LFI_all_fishing_scenarios_EEZ %>%
    filter(mpa == "Entire Mediterranean Sea") %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  LFI_Med_GSAprop <- ggplot() +
    
    geom_line(data = LFI_all_fishing_scenarios_Random_Med_GSAprop, aes(x = mpa_coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_Random_Med_GSAprop, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = LFI_all_fishing_scenarios_EEZ_Med_GSAprop, aes(x = mpa_coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_EEZ_Med_GSAprop, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = LFI_all_fishing_scenarios_lit_Med_GSAprop, aes(x = mpa_coverage, y = mean_LFI_reps, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_lit_Med_GSAprop, aes(x = mpa_coverage, ymin = mean_LFI_reps - sd_LFI_reps, ymax = mean_LFI_reps + sd_LFI_reps, fill = scenario), alpha = 0.2) +
    
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
    labs(x ="FPA coverage (%)", y = "Large Fish Indicator (> 20 cm)", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(0.278, 0.45)
  
  return(LFI_Med_GSAprop)
}


plot.LFI.inside.FPAs.with.sd <- function(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline){

  LFI_all_fishing_scenarios_lit = rbind(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline) %>%
    filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
    mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
    group_by(mpa_coverage, mpa, scenario, Fishing_scenario) %>%
    summarise(mean_LFI_reps = mean(mean_LFI_60_years),
              sd_LFI_reps = sd(mean_LFI_60_years))
  
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
    
    geom_line(data = LFI_all_fishing_scenarios_Random_inside_GSAprop, aes(x = mpa_coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_Random_inside_GSAprop, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = LFI_all_fishing_scenarios_EEZ_inside_GSAprop, aes(x = mpa_coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_EEZ_inside_GSAprop, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = LFI_all_fishing_scenarios_lit_inside_GSAprop, aes(x = mpa_coverage, y = mean_LFI_reps, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_lit_inside_GSAprop, aes(x = mpa_coverage, ymin = mean_LFI_reps - sd_LFI_reps, ymax = mean_LFI_reps + sd_LFI_reps, fill = scenario), alpha = 0.2) +
    
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
    labs(x ="FPA coverage (%)", y = "LFI (> 20 cm)", title = "Proportional by GSA") +
    
    theme(legend.position = "bottom") +
    #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(0.278, 0.45)
  
  return(LFI_inside_GSAprop)
  
}

plot.LFI.outside.FPAs.with.sd <- function(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline){
  
  LFI_all_fishing_scenarios_lit = rbind(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline) %>%
    filter(scenario %in% c("Micheli", "Mazor scenario 8", "Mazor scenario 9", "Existing network")) %>%
    mutate(x_scale = mpa_coverage * pas/6229 * 100) %>%
    group_by(mpa_coverage, mpa, scenario, Fishing_scenario) %>%
    summarise(mean_LFI_reps = mean(mean_LFI_60_years),
              sd_LFI_reps = sd(mean_LFI_60_years))
  
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
    
    geom_line(data = LFI_all_fishing_scenarios_Random_outside_GSAprop, aes(x = mpa_coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_Random_outside_GSAprop, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = LFI_all_fishing_scenarios_EEZ_outside_GSAprop, aes(x = mpa_coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_EEZ_outside_GSAprop, aes(x = mpa_coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = LFI_all_fishing_scenarios_lit_outside_GSAprop, aes(x = mpa_coverage, y = mean_LFI_reps, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = LFI_all_fishing_scenarios_lit_outside_GSAprop, aes(x = mpa_coverage, ymin = mean_LFI_reps - sd_LFI_reps, ymax = mean_LFI_reps + sd_LFI_reps, fill = scenario), alpha = 0.2) +
    
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
    labs(x ="FPA coverage (%)", y = "Large Fish Indicator (> 20 cm)", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    #theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(0.278, 0.45)
  
  return(LFI_outside_GSAprop)
}


plot.fig.suppmat.LFI.inside.outside.Med.with.sd <- function(height_value, width_value, save){
  ## Fig 4A - LFI -------------------------------
  
  LFI_Med_GSAprop = plot.LFI.Med.with.sd(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline)
  
  
  LFI_Med_mod <- LFI_Med_GSAprop + theme(
                                               plot.title = element_blank(),
                                               legend.position = "none",
                                               plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic")) + 
    labs(subtitle = "Entire Mediterranean Sea") +
    
    theme(plot.margin = margin(0,0,0,0.5, "cm")) # 2 -right, 4 - left
  
  LFI_inside_GSAprop = plot.LFI.inside.FPAs.with.sd(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline)
  
  LFI_inside_mod <- LFI_inside_GSAprop + theme(axis.title.y = element_blank(),
                                                #axis.ticks.x = element_blank(),
                                                plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic"),
                                                legend.position = "none",
                                                plot.title = element_blank()) +
    labs(subtitle = "Inside MPAs") +
    theme(plot.margin = margin(0,0.5,0,0, "cm")) # 2 -right, 4 - left
  
  LFI_outside_GSAprop = plot.LFI.outside.FPAs.with.sd(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline)
  
  LFI_outside_mod <- LFI_outside_GSAprop + theme(axis.title.y = element_blank(),
                                                 #axis.ticks.x = element_blank(),
                                                 plot.subtitle = element_text(vjust = -8, hjust = 0.9, size = 12, face = "italic"),
                                                 legend.position = "none",
                                                 plot.title = element_blank()) +
    labs(subtitle = "Outside MPAs") +
    theme(plot.margin = margin(0,0.5,0,0, "cm")) # 2 -right, 4 - left
  
  
  
  fig4A <- ggarrange(LFI_Med_mod, LFI_inside_mod, LFI_outside_mod,
                     ncol=3,
                     nrow=1,
                     align = "v",
                     labels = c("A", "B", "C"))  
  if (save == TRUE){
    ggsave(here("figures/appendix/Fig_4_LFI_inside_outside_Med_with_sd.png"), dpi = 300, height = 5, width = 10)
  }
}

