#' Function to plot total biomass and catch changes across the Mediterranean Sea
#'
#' 
#' @param data OSMOSE-MED simulation outputs of total biomass and total catch for each fishing redistribution strategy
#' 
#' @return Figure 1 (main text)
#'
#' @export
#' 

plot.biomass.catch.change.Med = function(Biom_GSA_prop, Biom_GSA_uniform, Biom_fline, Catch_GSA_prop, Catch_GSA_uniform, Catch_fline, save = TRUE){
  
  # Biomass change at Med Sea scale --------------------------------------------
  
  Biom_all_scenarios = rbind(Biom_GSA_prop, Biom_GSA_uniform, Biom_fline) %>%
    dplyr::select(tot.biomass, tot.biomass.ref, mpa.coverage, scenario_rep, scenario, Fishing_scenario) %>%
    mutate(rel_biom_change = (tot.biomass - tot.biomass.ref)/tot.biomass.ref * 100) %>%
    group_by(mpa.coverage, scenario, Fishing_scenario) %>%
    summarize(rel_biom_change_mean = mean(rel_biom_change, na.rm = T),
              rel_biom_change_sd = sd(rel_biom_change, na.rm = T)) 
  
  # Lit scenarios -------------------
  Biom_lit_scenarios = Biom_all_scenarios %>%
    filter(scenario %in% c("Mazor scenario 8", "Micheli", "Mazor scenario 9", "Existing network"))
  
  # Random ------------
  Biom_random_scenarios = Biom_all_scenarios %>%
    filter(grepl('Random', scenario)) %>%
    group_by(mpa.coverage, Fishing_scenario) %>%
    mutate(high = max(rel_biom_change_mean),
           low = min(rel_biom_change_mean),
           mean = mean(rel_biom_change_mean),
           sd = mean(rel_biom_change_sd)) %>%
    mutate(scenario_group = "Random")
  
  
  # EEZ ------------
  Biom_EEZ_scenarios = Biom_all_scenarios %>%
    filter(grepl('EEZ', scenario)) %>%
    group_by(mpa.coverage, Fishing_scenario) %>%
    mutate(high = max(rel_biom_change_mean),
           low = min(rel_biom_change_mean),
           mean = mean(rel_biom_change_mean),
           sd = mean(rel_biom_change_sd)) %>%
    mutate(scenario_group = "EEZ-scale conservation")
  
  # Catch change at Med Sea scale --------------------------------------------
  
  Catch_all_scenarios = rbind(Catch_GSA_prop, Catch_GSA_uniform, Catch_fline) %>%
    dplyr::select(tot.catches, tot.catches.ref, mpa.coverage, scenario_rep, scenario, Fishing_scenario) %>%
    mutate(rel_catch_change = (tot.catches - tot.catches.ref)/tot.catches.ref * 100) %>%
    group_by(mpa.coverage, scenario, Fishing_scenario) %>%
    summarize(rel_catch_change_mean = mean(rel_catch_change, na.rm = T),
              rel_catch_change_sd = sd(rel_catch_change, na.rm = T)) 
  
  # Lit scenarios -------------------
  Catch_lit_scenarios = Catch_all_scenarios %>%
    filter(scenario %in% c("Mazor scenario 8", "Micheli", "Mazor scenario 9", "Existing network"))
  
  # Random ------------
  Catch_random_scenarios = Catch_all_scenarios %>%
    filter(grepl('Random', scenario)) %>%
    group_by(mpa.coverage, Fishing_scenario) %>%
    mutate(high = max(rel_catch_change_mean),
           low = min(rel_catch_change_mean),
           mean = mean(rel_catch_change_mean),
           sd = mean(rel_catch_change_sd)) %>%
    mutate(scenario_group = "Random")
  
  
  # EEZ ------------
  Catch_EEZ_scenarios = Catch_all_scenarios %>%
    filter(grepl('EEZ', scenario)) %>%
    group_by(mpa.coverage, Fishing_scenario) %>%
    mutate(high = max(rel_catch_change_mean),
           low = min(rel_catch_change_mean),
           mean = mean(rel_catch_change_mean),
           sd = mean(rel_catch_change_sd)) %>%
    mutate(scenario_group = "EEZ-scale conservation")
  
  
  # FIGURES --------------------------------------------------------------------
  # TOTAL BIOMASS --------------------------------------------------------------
  
  # Uniform by GSA
  Biom_random_scenarios_GSAprop = Biom_random_scenarios %>%
    filter(Fishing_scenario == "Proportional by GSA")
  Biom_EEZ_scenarios_GSAprop = Biom_EEZ_scenarios %>%
    filter(Fishing_scenario == "Proportional by GSA")
  Biom_lit_scenarios_GSAprop = Biom_lit_scenarios %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  biom_change_GSAprop <- ggplot() +
    
    geom_line(data = Biom_random_scenarios_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Biom_random_scenarios_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Biom_EEZ_scenarios_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Biom_EEZ_scenarios_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Biom_lit_scenarios_GSAprop, aes(x = mpa.coverage, y = rel_biom_change_mean, colour = scenario), lwd = 0.8, alpha=1) +
    
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
    labs(x ="FPA coverage (%)", y = "Biomass change (%)", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(-1, 10)
  
  # Uniform by GSA
  Biom_random_scenarios_GSAuniform = Biom_random_scenarios %>%
    filter(Fishing_scenario == "Uniform by GSA")
  Biom_EEZ_scenarios_GSAuniform = Biom_EEZ_scenarios %>%
    filter(Fishing_scenario == "Uniform by GSA")
  Biom_lit_scenarios_GSAuniform = Biom_lit_scenarios %>%
    filter(Fishing_scenario == "Uniform by GSA")
  
  biom_change_GSAuniform <- ggplot() +
    
    geom_line(data = Biom_random_scenarios_GSAuniform, aes(x = mpa.coverage, y = mean, colour = scenario), linetype = "solid", lwd = 0.8, alpha=1) +
    geom_ribbon(data = Biom_random_scenarios_GSAuniform, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Biom_EEZ_scenarios_GSAuniform, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "solid") +
    geom_ribbon(data = Biom_EEZ_scenarios_GSAuniform, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Biom_lit_scenarios_GSAuniform, aes(x = mpa.coverage, y = rel_biom_change_mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "solid") +
    
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
    labs(x ="FPA coverage (%)", y = "Biomass change (%)", title = "Uniform by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(-1, 10)
  
  
  Biom_random_scenarios_fline = Biom_random_scenarios %>%
    filter(Fishing_scenario == "Fishing-the-line")
  Biom_EEZ_scenarios_fline = Biom_EEZ_scenarios %>%
    filter(Fishing_scenario == "Fishing-the-line")
  Biom_lit_scenarios_fline = Biom_lit_scenarios %>%
    filter(Fishing_scenario == "Fishing-the-line")
  
  biom_change_fline <- ggplot() +
    
    geom_line(data = Biom_random_scenarios_fline, aes(x = mpa.coverage, y = mean, colour = scenario), linetype = "solid", lwd = 0.8, alpha=1) +
    geom_ribbon(data = Biom_random_scenarios_fline, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Biom_EEZ_scenarios_fline, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "solid") +
    geom_ribbon(data = Biom_EEZ_scenarios_fline, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Biom_lit_scenarios_fline, aes(x = mpa.coverage, y = rel_biom_change_mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "solid") +
    
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
    labs(x ="FPA coverage (%)", y = "Biomass change (%)", title = "Fishing-the-line") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(-1, 10)
  
  # TOTAL CATCHES ----------------------------------------------------------------
  
  # Proportional by GSA
  Catch_random_scenarios_GSAprop = Catch_random_scenarios %>%
    filter(Fishing_scenario == "Proportional by GSA")
  Catch_EEZ_scenarios_GSAprop = Catch_EEZ_scenarios %>%
    filter(Fishing_scenario == "Proportional by GSA")
  Catch_lit_scenarios_GSAprop = Catch_lit_scenarios %>%
    filter(Fishing_scenario == "Proportional by GSA")
  
  catch_change_GSAprop <- ggplot() +
    
    geom_line(data = Catch_random_scenarios_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Catch_random_scenarios_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Catch_EEZ_scenarios_GSAprop, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1) +
    geom_ribbon(data = Catch_EEZ_scenarios_GSAprop, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Catch_lit_scenarios_GSAprop, aes(x = mpa.coverage, y = rel_catch_change_mean, colour = scenario), lwd = 0.8, alpha=1) +
    
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
    labs(x ="FPA coverage (%)", y = "Catch change (%)", title = "Proportional by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
    ylim(-26.5, 1)
  
  
  # Uniform by GSA
  Catch_random_scenarios_GSAuniform = Catch_random_scenarios %>%
    filter(Fishing_scenario == "Uniform by GSA")
  Catch_EEZ_scenarios_GSAuniform = Catch_EEZ_scenarios %>%
    filter(Fishing_scenario == "Uniform by GSA")
  Catch_lit_scenarios_GSAuniform = Catch_lit_scenarios %>%
    filter(Fishing_scenario == "Uniform by GSA")
  
  catch_change_GSAuniform <- ggplot() +
    
    geom_line(data = Catch_random_scenarios_GSAuniform, aes(x = mpa.coverage, y = mean, colour = scenario), linetype = "solid", lwd = 0.8, alpha=1) +
    geom_ribbon(data = Catch_random_scenarios_GSAuniform, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Catch_EEZ_scenarios_GSAuniform, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "solid") +
    geom_ribbon(data = Catch_EEZ_scenarios_GSAuniform, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Catch_lit_scenarios_GSAuniform, aes(x = mpa.coverage, y = rel_catch_change_mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "solid") +
    
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
    labs(x ="FPA coverage (%)", y = "Catch change (%)", title = "Uniform by GSA") +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(-26.5, 1)
  
  
  # Fishing-the-line
  
  Catch_random_scenarios_fline = Catch_random_scenarios %>%
    filter(Fishing_scenario == "Fishing-the-line")
  Catch_EEZ_scenarios_fline = Catch_EEZ_scenarios %>%
    filter(Fishing_scenario == "Fishing-the-line")
  Catch_lit_scenarios_fline = Catch_lit_scenarios %>%
    filter(Fishing_scenario == "Fishing-the-line")
  
  catch_change_fline <- ggplot() +
    
    geom_line(data = Catch_random_scenarios_fline, aes(x = mpa.coverage, y = mean, colour = scenario), linetype = "solid", lwd = 0.8, alpha=1) +
    geom_ribbon(data = Catch_random_scenarios_fline, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Catch_EEZ_scenarios_fline, aes(x = mpa.coverage, y = mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "solid") +
    geom_ribbon(data = Catch_EEZ_scenarios_fline, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    geom_line(data = Catch_lit_scenarios_fline, aes(x = mpa.coverage, y = rel_catch_change_mean, colour = scenario), lwd = 0.8, alpha=1, linetype = "solid") +
    
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
    labs(x ="FPA coverage (%)", y = "Catch change (%)", title = "Fishing-the-line") +
    theme(legend.position = "bottom") +

    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
    ylim(-26.5, 1)
  
  
  # Fig 1C - scatterplot
  Biom_random_scenarios_m = Biom_random_scenarios %>%
    ungroup() %>%
    dplyr::select(mpa.coverage, scenario_group, Fishing_scenario, mean, sd) %>%
    group_by(mpa.coverage, scenario_group, Fishing_scenario) %>%
    summarize(rel_biom_change_mean = mean(mean),
              rel_biom_change_sd = mean(sd)) %>%
    rename(scenario = scenario_group)
  
  
  Biom_EEZ_scenarios_m = Biom_EEZ_scenarios %>%
    ungroup() %>%
    dplyr::select(mpa.coverage, scenario_group, Fishing_scenario, mean, sd) %>%
    group_by(mpa.coverage, scenario_group, Fishing_scenario) %>%
    summarize(rel_biom_change_mean = mean(mean),
              rel_biom_change_sd = mean(sd)) %>%
    rename(scenario = scenario_group)
  
  Catch_random_scenarios_m = Catch_random_scenarios %>%
    ungroup() %>%
    dplyr::select(mpa.coverage, scenario_group, Fishing_scenario, mean, sd) %>%
    group_by(mpa.coverage, scenario_group, Fishing_scenario) %>%
    summarize(rel_catch_change_mean = mean(mean),
              rel_catch_change_sd = mean(sd)) %>%
    rename(scenario = scenario_group)
  
  
  Catch_EEZ_scenarios_m = Catch_EEZ_scenarios %>%
    ungroup() %>%
    dplyr::select(mpa.coverage, scenario_group, Fishing_scenario, mean, sd) %>%
    group_by(mpa.coverage, scenario_group, Fishing_scenario) %>%
    summarize(rel_catch_change_mean = mean(mean),
              rel_catch_change_sd = mean(sd)) %>%
    rename(scenario = scenario_group)
  
  Catch_scenarios = rbind(Catch_lit_scenarios, Catch_random_scenarios_m, Catch_EEZ_scenarios_m) 
  Biom_scenarios = rbind(Biom_lit_scenarios, Biom_random_scenarios_m, Biom_EEZ_scenarios_m) 
  
  All_scenarios = Biom_scenarios %>%
    left_join(Catch_scenarios, by = c("mpa.coverage", "scenario", "Fishing_scenario")) %>%
    mutate(above_target = ifelse(mpa.coverage > 10, "above", "below"))
  

  
  All_scenarios <- All_scenarios %>%
    mutate(scenario = factor(scenario, levels = c(
      "EEZ-scale conservation",  # drawn first = bottom
      "Random",
      "Existing network",
      "Micheli",
      "Mazor scenario 8",
      "Mazor scenario 9"
    )))
  
  
  mpa_colors_group <- c("Existing network" = "gold", 
                        "Random" = "gray60", 
                        "EEZ-scale conservation" = "black", 
                        "Micheli" = "darkorange", 
                        "Mazor scenario 8" = "darkslateblue", 
                        "Mazor scenario 9" = "darkorchid")
  
  
  Scenarios_GSAprop = All_scenarios %>%
    filter(Fishing_scenario == "Proportional by GSA")

  scatterplot_GSAprop <- ggplot(Scenarios_GSAprop, aes(x = rel_biom_change_mean, y = rel_catch_change_mean, fill = scenario, color = scenario)) +
    
    # Add points
    geom_point(aes(size = mpa.coverage, alpha = above_target), shape = 21) +
    
    
    scale_fill_manual(name = "Scenario", values = mpa_colors_group) +
    scale_color_manual(name = "Scenario", values = mpa_colors_group) +
    scale_alpha_manual(values = c("above" = 0.2, "below" = 0.8), guide = "none") +

    # Labels and theme
    labs(title = "",
         x = "Biomass change (%)",
         y = "Catch change (%)",
    ) +
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    theme(plot.title = element_text(face = "bold", size = 16, hjust = 0)) 
  
  Scenarios_GSAuniform = All_scenarios %>%
    filter(Fishing_scenario == "Uniform by GSA")
  
  scatterplot_GSAuniform <- ggplot(Scenarios_GSAuniform, aes(x = rel_biom_change_mean, y = rel_catch_change_mean, fill = scenario, color = scenario)) +
    
    # Add points
    geom_point(aes(size = mpa.coverage, alpha = above_target), shape = 21) +
    
    
    scale_fill_manual(name = "Scenario", values = mpa_colors_group) +
    scale_color_manual(name = "Scenario", values = mpa_colors_group) +
    scale_alpha_manual(values = c("above" = 0.2, "below" = 0.8), guide = "none") +
    
    # Labels and theme
    labs(title = "",
         x = "Biomass change (%)",
         y = "Catch change (%)",
    ) +
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    theme(plot.title = element_text(face = "bold", size = 16, hjust = 0)) 
  
  print(scatterplot_GSAuniform)

  Scenarios_fline = All_scenarios %>%
    filter(Fishing_scenario == "Fishing-the-line")
  
  scatterplot_fline <- ggplot(Scenarios_fline, aes(x = rel_biom_change_mean, y = rel_catch_change_mean, fill = scenario, color = scenario)) +
    
    # Add points
    geom_point(aes(size = mpa.coverage, alpha = above_target), shape = 21) +
    
    
    scale_fill_manual(name = "Scenario", values = mpa_colors_group) +
    scale_color_manual(name = "Scenario", values = mpa_colors_group) +
    scale_alpha_manual(values = c("above" = 0.2, "below" = 0.8), guide = "none") +
    
    # Labels and theme
    labs(title = "",
         x = "Biomass change (%)",
         y = "Catch change (%)",
    ) +
    theme_bw() +
    theme(axis.text=element_text(size=12), axis.title = element_text(size = 14)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    theme(plot.title = element_text(face = "bold", size = 16, hjust = 0)) 

  print(scatterplot_fline)
  
  ## Figure 1 (main text) --------------------------------------------------------
  # Extract legend from one of the 1B plots
  legend_fig1 <- get_legend(
    catch_change_GSAprop + 
      theme(legend.title = element_blank(), 
            legend.text = element_text(size = 14),
            legend.position = "bottom")
  )
  
  # Fig 1A - no legends
  biom_change_GSAprop_nolegend    <- biom_change_GSAprop    + theme(legend.position = "none")
  biom_change_GSAuniform_nolegend <- biom_change_GSAuniform + theme(legend.position = "none", axis.title.y = element_blank())
  biom_change_fline_nolegend      <- biom_change_fline      + theme(legend.position = "none", axis.title.y = element_blank())
  
  # Fig 1B - no legends
  catch_change_GSAprop_nolegend    <- catch_change_GSAprop    + theme(legend.position = "none", plot.title = element_blank())
  catch_change_GSAuniform_nolegend <- catch_change_GSAuniform + theme(legend.position = "none", plot.title = element_blank(), axis.title.y = element_blank())
  catch_change_fline_nolegend      <- catch_change_fline      + theme(legend.position = "none", plot.title = element_blank(), axis.title.y = element_blank())
  
  # Fig 1C - no legends (already had none)
  scatterplot_GSAprop_m    <- scatterplot_GSAprop    + theme(legend.position = "none", plot.title = element_blank())
  scatterplot_GSAuniform_m <- scatterplot_GSAuniform + theme(legend.position = "none", plot.title = element_blank(), axis.title.y = element_blank())
  scatterplot_fline_m      <- scatterplot_fline      + theme(legend.position = "none", plot.title = element_blank(), axis.title.y = element_blank())
  
  # Assemble rows — all without legends
  fig1A <- plot_grid(biom_change_GSAprop_nolegend, biom_change_GSAuniform_nolegend, biom_change_fline_nolegend,
                     ncol = 3, nrow = 1, align = "hv")
  
  fig1B <- plot_grid(catch_change_GSAprop_nolegend, catch_change_GSAuniform_nolegend, catch_change_fline_nolegend,
                     ncol = 3, nrow = 1, align = "hv")
  
  fig1C <- plot_grid(scatterplot_GSAprop_m, scatterplot_GSAuniform_m, scatterplot_fline_m,
                     ncol = 3, nrow = 1, align = "hv")
  
  fig1_body <- plot_grid(fig1A, fig1B, fig1C,
                         ncol = 1, nrow = 3,
                         rel_heights = c(1.15, 1, 1),
                         labels = c("A", "B", "C"),
                         label_size = 20,
                         align = "v", axis = "lr")  # forces left/right axes to align across rows
  
  fig1 <- plot_grid(fig1_body, legend_fig1,
                    ncol = 1,
                    rel_heights = c(1, 0.1))

  if (save == TRUE){
    ggsave(here("figures/Fig_1_total_biomass_catch_scatterplot_ed.png"), dpi = 1000, height = 10, width = 10)
    
  }
  
}







