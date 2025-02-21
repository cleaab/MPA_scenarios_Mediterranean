#' Function to plot changes in biomass and catch relative to before MPA establishment by groups of species
#'
#' 
#' @param data OSMOSE-MED simulation outputs of total biomass and total catch by groups for each fishing redistribution strategy
#' 
#' @return Figure 3A (main text) &  Figures A6.3 - A6.6
#'
#' @export
#' 

plot.biomass.change.by.groups <- function(biomass_by_groups, scen = 1, f = 1){
  
  # BIOMASS by groups ----------------------------------------------------------
  
  Tot_biom_bygroups_noCC = biomass_by_groups %>%
    
    filter(mpa == "Entire Mediterranean Sea") %>%
    mutate(group = case_when(group %in% c("Large-sized pelagic fish", "Medium-sized pelagic fish") ~ "Medium & large pelagic",
                             group %in% c("Large-sized demersal fish", "Medium-sized demersal fish") ~ "Medium & large demersal",
                             group %in% c("Large-sized benthic fish", "Medium-sized benthic fish") ~ "Medium & large benthic",
                             group %in% c("Small-sized pelagic fish") ~ "Small pelagic",
                             group %in% c("Small-sized benthic fish") ~ "Small benthic",
                             group %in% c("Small-sized demersal fish") ~ "Small demersal",
                             TRUE ~ group)) %>%
    group_by(mpa.coverage, mpa, scenario, group, scenario_rep) %>%
    summarise(total_biomass = sum(tot.biomass), total_biomass_ref = sum(tot.biomass.ref)) %>%
    ungroup() %>%
    group_by(mpa.coverage, scenario, group) %>%
    summarise(mean_biom_reps = mean(total_biomass), mean_biom_ref_reps = mean(total_biomass_ref), biom_diff = mean(total_biomass) - mean(total_biomass_ref), biom_rel_change = (mean(total_biomass) - mean(total_biomass_ref))/ mean(total_biomass_ref) * 100) %>%
    ungroup() %>%
    
    group_by(mpa.coverage, scenario) %>%
    mutate(total_biom_diff = sum(biom_diff)) %>%
    ungroup()
  
  Tot_biom_bygroups_noCC_onescenario = Tot_biom_bygroups_noCC %>%
    filter(scenario == lit.scenarios.name[scen])
  
  # Set factor levels to ensure consistent legend order
  Tot_biom_bygroups_noCC_onescenario$group <- factor(Tot_biom_bygroups_noCC_onescenario$group, levels = sp_group_levels)
  
  biom_Mazor8_Med <- ggplot() +
    geom_bar(data = Tot_biom_bygroups_noCC_onescenario, aes(x = mpa.coverage, y = biom_diff, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(data = Tot_biom_bygroups_noCC_onescenario, aes(x = mpa.coverage, y = total_biom_diff), lwd = 0.5) +
    geom_line(data = Tot_biom_bygroups_noCC_onescenario, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    
    scale_fill_brewer(palette="BrBG", direction = -1) +
   
    labs(title= paste0("MPA Scenario ", MPA_scenarios_name[scen]), subtitle= paste0(Fishing_scenario_name[f]),
         x ="MPA coverage (%)", y = "Biomass difference (tons)") 
  # ylim(-150000, 510000) # for Mazor scenario 8

    return(biom_Mazor8_Med)
}

plot.catch.change.by.groups <- function(catch_by_groups, scen = 1, f = 1){
  
  # CATCH by groups ------------------------------------------------------------
  
    Tot_catch_bygroups_noCC = catch_by_groups %>%
      
      filter(mpa == "Entire Mediterranean Sea") %>%
      mutate(group = case_when(group %in% c("Large-sized pelagic fish", "Medium-sized pelagic fish") ~ "Medium & large pelagic",
                               group %in% c("Large-sized demersal fish", "Medium-sized demersal fish") ~ "Medium & large demersal",
                               group %in% c("Large-sized benthic fish", "Medium-sized benthic fish") ~ "Medium & large benthic",
                               group %in% c("Small-sized pelagic fish") ~ "Small pelagic",
                               group %in% c("Small-sized benthic fish") ~ "Small benthic",
                               group %in% c("Small-sized demersal fish") ~ "Small demersal",
                               TRUE ~ group)) %>%
      group_by(mpa.coverage, mpa, scenario, group, scenario_rep) %>%
      summarise(total_catches = sum(tot.catches), total_catches_ref = sum(tot.catches.ref)) %>%
      ungroup() %>%
      group_by(mpa.coverage, scenario, group) %>%
      summarise(mean_catch_reps = mean(total_catches), mean_catch_ref_reps = mean(total_catches_ref), catch_diff = mean(total_catches) - mean(total_catches_ref), catch_rel_change = (mean(total_catches) - mean(total_catches_ref))/ mean(total_catches_ref) * 100) %>%
      ungroup() %>%
      
      group_by(mpa.coverage, scenario) %>%
      
      mutate(total_catch_diff = sum(catch_diff)) %>%
      ungroup()
    
    
      Tot_catch_bygroups_noCC_onescenario = Tot_catch_bygroups_noCC %>%
        filter(scenario == lit.scenarios.name[scen])
      
      # Set factor levels to ensure consistent legend order
      Tot_catch_bygroups_noCC_onescenario$group <- factor(Tot_catch_bygroups_noCC_onescenario$group, levels = sp_group_levels)
      
      
      catch_Mazor8_Med <- ggplot() +
        geom_bar(data = Tot_catch_bygroups_noCC_onescenario, aes(x = mpa.coverage, y = catch_diff, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
        geom_line(data = Tot_catch_bygroups_noCC_onescenario, aes(x = mpa.coverage, y = total_catch_diff), lwd = 0.5) +
        geom_line(data = Tot_catch_bygroups_noCC_onescenario, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
        
        theme_bw() +
        theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
        theme(legend.key.size = unit(1, 'cm'), #change legend key size
              legend.key.height = unit(1, 'cm'), #change legend key height
              legend.key.width = unit(1, 'cm'), #change legend key width
              legend.title = element_blank(), #change legend title font size
              legend.text = element_text(size=10)) + #change legend text font size 
        scale_fill_brewer(palette="BrBG", direction = -1) +
        
        labs(title= paste0("MPA Scenario ", MPA_scenarios_name[scen]), subtitle= paste0(Fishing_scenario_name[f]),
             x ="MPA coverage (%)", y = "Catch difference (tons)") 
      #ylim(-200500,82000) # for Mazor 8
  
      return(catch_Mazor8_Med)
}

plot.small.pelagics.biomass.change.inside.MPAs <- function(biomass_by_groups, f = 1){

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
    summarise(mean_rel_change = mean(biom_rel_change))
  
  
  # Random - basin
  Biom_small_pelagics_random = biomass_by_groups %>%
    filter(grepl('Random', scenario)) %>%
    filter(mpa == "Inside MPAs") %>%
    filter(group == "Small-sized pelagic fish") %>%
    mutate(biom_diff = tot.biomass - tot.biomass.ref, 
           biom_rel_change = (tot.biomass - tot.biomass.ref)/ tot.biomass.ref * 100) %>%
    
    group_by(mpa.coverage, scenario) %>%
    summarise(mean_rel_change = mean(biom_rel_change),
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
           biom_rel_change = (tot.biomass - tot.biomass.ref)/ tot.biomass.ref * 100) %>%
    
    group_by(mpa.coverage, scenario) %>%
    summarise(mean_rel_change = mean(biom_rel_change),
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
    geom_line(data = Biom_small_pelagics_lit_current, aes(x = mpa.coverage, y = mean_rel_change, colour = scenario), lwd = 0.8, alpha=1) +
    
    geom_line(data = Biom_small_pelagics_random, aes(x = mpa.coverage, y = mean_rel_change, colour = scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = Biom_small_pelagics_random, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.3) +
    
    geom_line(data = Biom_small_pelagics_EEZ, aes(x = mpa.coverage, y = mean_rel_change, colour = scenario), lwd = 0.1, alpha=1) +
    geom_ribbon(data = Biom_small_pelagics_EEZ, aes(x = mpa.coverage, ymin = low, ymax = high, fill = scenario_group), alpha = 0.5) +
    
    
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
    labs(x ="MPA coverage (%)", y = "Biomass change (%)", title = "Small pelagic fish", subtitle = "Inside reserves") +
    theme(legend.position = "bottom") +
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10))
  
  
  return(seuil_small_pel_inside_rel_change)
}


plot.fig3 <- function(biomass_by_groups, catch_by_groups, scen, f, save = TRUE){
  
  # ------ Fig 3A
  biom_Mazor8_Med = plot.biomass.change.by.groups(biomass_by_groups, scen, f)
  
  biom_Mazor8_Med_mod <- biom_Mazor8_Med + theme(plot.title = element_blank(), plot.subtitle = element_blank(),
                                                 axis.title.x = element_blank(),
                                                 plot.margin = unit(c(5, 3, 0.5, 4), "pt"))
  
  catch_Mazor8_Med = plot.catch.change.by.groups(catch_by_groups, scen, f)
  
  catch_Mazor8_Med_mod <- catch_Mazor8_Med + theme(plot.title = element_blank(), plot.subtitle = element_blank(),
                                                   plot.margin = unit(c(0, 3, 0, 4), "pt"))
  
  fig3A <- ggarrange(biom_Mazor8_Med_mod, catch_Mazor8_Med_mod,
                     ncol=1,
                     nrow=2,
                     common.legend = T,
                     legend = "right",
                     align = "hv") +
    labs(title = "All species", subtitle = "Entire Mediterranean Sea") +
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))+
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin=margin(b=10)), plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10)) +
    theme(plot.background = element_rect(fill = "white", color = NA))
  
  
  # ------ Fig 3B
  seuil_small_pel_inside_rel_change = plot.small.pelagics.biomass.change.inside.MPAs(biomass_by_groups, f)
  
  fig3B <- seuil_small_pel_inside_rel_change + theme(plot.title = element_text(hjust = 0.5, size=14, margin=margin(b=10)), legend.title = element_blank(), legend.direction = "vertical") 
  
  
  ## fig3 ----------------------------------------------------------------------
  fig3 <- plot_grid(fig3A, fig3B, ncol = 2, rel_widths = c(2, 1), labels = c("A", "B"))
  
  if (save == TRUE){
    ggsave(here("figures/Fig_3_total_biomass_catch_by_groups.png"), dpi = 1000, height = 10, width = 10)
  }
  
}

# other MPA scenarios and fishing redistribution strategies
  
plot.fig.A6.other.scenarios.S1.S4.S5.S6 <- function(biomass_by_groups_data, catch_by_groups_data, scen, save = TRUE){
    
    for (f in 1:3){
      # Biomass plots -------
      
      biomass_scen = plot.biomass.change.by.groups(biomass_by_groups_data[[f]], scen, f)
      
      biomass_scen_mod <- biomass_scen + theme(axis.title.x = element_blank(),
                                               plot.margin = unit(c(5, 3, 0.5, 4), "pt"))
      
      # Catch plots -------
      
      catch_scen = plot.catch.change.by.groups(catch_by_groups_data[[f]], scen, f)
      
      catch_scen_mod <- catch_scen + theme(plot.title = element_blank(), plot.subtitle = element_blank(),
                                           plot.margin = unit(c(0, 3, 0, 4), "pt"))
      # Store in lists instead of using `c()`
      biomass_plots_f[[f]] <- biomass_scen_mod
      catch_plots_f[[f]] <- catch_scen_mod
      
    }
    
    # Arrange plots together
    scen_plot <- ggarrange(plotlist = c(biomass_plots_f, catch_plots_f), 
                            ncol = 3, 
                            nrow = 2, 
                            common.legend = T,
                            legend = "bottom",
                            align = "hv")
    if (save == TRUE){
      ggsave(here(paste0("figures/appendix/Fig_A6_total_biomass_catch_by_groups_", MPA_scenarios_name[scen], ".png")), dpi = 1000, height = 12, width = 12)
    }
  }
  
 ### random scenarios S2-basin

plot.biomass.change.by.groups.S2 <- function(biomass_by_groups, f = 1){

  # corrected version with reviewed classification grouped large + medium together  
  Tot_biom_bygroups_noCC = biomass_by_groups %>%
    filter(grepl('Random', scenario)) %>%
    filter(mpa == "Entire Mediterranean Sea") %>%
    mutate(group = case_when(group %in% c("Large-sized pelagic fish", "Medium-sized pelagic fish") ~ "Medium & large pelagic",
                             group %in% c("Large-sized demersal fish", "Medium-sized demersal fish") ~ "Medium & large demersal",
                             group %in% c("Large-sized benthic fish", "Medium-sized benthic fish") ~ "Medium & large benthic",
                             group %in% c("Small-sized pelagic fish") ~ "Small pelagic",
                             group %in% c("Small-sized benthic fish") ~ "Small benthic",
                             group %in% c("Small-sized demersal fish") ~ "Small demersal",
                             TRUE ~ group)) %>%
    group_by(mpa.coverage, mpa, scenario, group, scenario_rep) %>%
    summarise(total_biomass = sum(tot.biomass), total_biomass_ref = sum(tot.biomass.ref)) %>%
    ungroup() %>%
    group_by(mpa.coverage, group) %>%
    summarise(mean_biom_reps = mean(total_biomass), mean_biom_ref_reps = mean(total_biomass_ref), biom_diff = mean(total_biomass) - mean(total_biomass_ref), biom_rel_change = (mean(total_biomass) - mean(total_biomass_ref))/ mean(total_biomass_ref) * 100) %>%
    ungroup() %>%
    
    group_by(mpa.coverage) %>%
    mutate(total_biom_diff = sum(biom_diff)) %>%
    ungroup() %>%
    mutate(scenario = "S2 - Random basin")
  
  
  # Set factor levels to ensure consistent legend order
  Tot_biom_bygroups_noCC$group <- factor(Tot_biom_bygroups_noCC$group, levels = sp_group_levels)
  
  biom_S2_Med <- ggplot() +
    geom_bar(data = Tot_biom_bygroups_noCC, aes(x = mpa.coverage, y = biom_diff, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(data = Tot_biom_bygroups_noCC, aes(x = mpa.coverage, y = total_biom_diff), lwd = 0.5) +
    geom_line(data = Tot_biom_bygroups_noCC, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) +
    
    scale_fill_brewer(palette="BrBG", direction = -1) +
    
    labs(title= "MPA Scenario S2 - Random basin", subtitle= paste0(Fishing_scenario_name[f]),
         x ="MPA coverage (%)", y = "Biomass difference (tons)") 
  
  return(biom_S2_Med)

}

plot.catch.change.by.groups.S2 <- function(catch_by_groups, f = 1){
  

  Tot_catch_bygroups_noCC = catch_by_groups %>%
    filter(grepl('Random', scenario)) %>%
    
    filter(mpa == "Entire Mediterranean Sea") %>%
    mutate(group = case_when(group %in% c("Large-sized pelagic fish", "Medium-sized pelagic fish") ~ "Medium & large pelagic",
                             group %in% c("Large-sized demersal fish", "Medium-sized demersal fish") ~ "Medium & large demersal",
                             group %in% c("Large-sized benthic fish", "Medium-sized benthic fish") ~ "Medium & large benthic",
                             group %in% c("Small-sized pelagic fish") ~ "Small pelagic",
                             group %in% c("Small-sized benthic fish") ~ "Small benthic",
                             group %in% c("Small-sized demersal fish") ~ "Small demersal",
                             TRUE ~ group)) %>%
    group_by(mpa.coverage, mpa, scenario, group, scenario_rep) %>%
    summarise(total_catches = sum(tot.catches), total_catches_ref = sum(tot.catches.ref)) %>%
    ungroup() %>%
    group_by(mpa.coverage, group) %>%
    summarise(mean_catch_reps = mean(total_catches), mean_catch_ref_reps = mean(total_catches_ref), catch_diff = mean(total_catches) - mean(total_catches_ref), catch_rel_change = (mean(total_catches) - mean(total_catches_ref))/ mean(total_catches_ref) * 100) %>%
    ungroup() %>%
    
    group_by(mpa.coverage) %>%
    
    mutate(total_catch_diff = sum(catch_diff)) %>%
    ungroup() %>%
    mutate(scenario = "S2 - Random basin")
  
  
  
  # Set factor levels to ensure consistent legend order
  Tot_catch_bygroups_noCC$group <- factor(Tot_catch_bygroups_noCC$group, levels = sp_group_levels)
  
  
  catch_S2_Med <- ggplot() +
    geom_bar(data = Tot_catch_bygroups_noCC, aes(x = mpa.coverage, y = catch_diff, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(data = Tot_catch_bygroups_noCC, aes(x = mpa.coverage, y = total_catch_diff), lwd = 0.5) +
    geom_line(data = Tot_catch_bygroups_noCC, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) +
    
    scale_fill_brewer(palette="BrBG", direction = -1) +  
    
    labs(title= "MPA Scenario S2 - Random basin", subtitle= paste0(Fishing_scenario_name[f]),
         x ="MPA coverage (%)", y = "Catch difference (tons)") 

  return(catch_S2_Med)    
  
  
}


plot.fig.A6.other.scenarios.S2 <- function(biomass_by_groups_data, catch_by_groups_data, save = TRUE){
  
  for (f in 1:3){
    # Biomass plots -------
    
    biomass_scen = plot.biomass.change.by.groups.S2(biomass_by_groups_data[[f]], f)
    
    biomass_scen_mod <- biomass_scen + theme(axis.title.x = element_blank(),
                                             plot.margin = unit(c(5, 3, 0.5, 4), "pt"))
    
    # Catch plots -------
    
    catch_scen = plot.catch.change.by.groups.S2(catch_by_groups_data[[f]], f)
    
    catch_scen_mod <- catch_scen + theme(plot.title = element_blank(), plot.subtitle = element_blank(),
                                         plot.margin = unit(c(0, 3, 0, 4), "pt"))
    # Store in lists instead of using `c()`
    biomass_plots_f[[f]] <- biomass_scen_mod
    catch_plots_f[[f]] <- catch_scen_mod
    
  }
  
  # Arrange plots together
  scen_plot <- ggarrange(plotlist = c(biomass_plots_f, catch_plots_f), 
                         ncol = 3, 
                         nrow = 2, 
                         common.legend = T,
                         legend = "bottom",
                         align = "hv")
  if (save == TRUE){
    ggsave(here(paste0("figures/appendix/Fig_A6_total_biomass_catch_by_groups_S2_random.png")), dpi = 1000, height = 12, width = 12)
  }
}

### random scenarios S3-EEZ

plot.biomass.change.by.groups.S3 <- function(biomass_by_groups, f = 1){
  
  # corrected version with reviewed classification grouped large + medium together  
  Tot_biom_bygroups_noCC = biomass_by_groups %>%
    filter(grepl('EEZ', scenario)) %>%
    filter(mpa == "Entire Mediterranean Sea") %>%
    mutate(group = case_when(group %in% c("Large-sized pelagic fish", "Medium-sized pelagic fish") ~ "Medium & large pelagic",
                             group %in% c("Large-sized demersal fish", "Medium-sized demersal fish") ~ "Medium & large demersal",
                             group %in% c("Large-sized benthic fish", "Medium-sized benthic fish") ~ "Medium & large benthic",
                             group %in% c("Small-sized pelagic fish") ~ "Small pelagic",
                             group %in% c("Small-sized benthic fish") ~ "Small benthic",
                             group %in% c("Small-sized demersal fish") ~ "Small demersal",
                             TRUE ~ group)) %>%
    group_by(mpa.coverage, mpa, scenario, group, scenario_rep) %>%
    summarise(total_biomass = sum(tot.biomass), total_biomass_ref = sum(tot.biomass.ref)) %>%
    ungroup() %>%
    group_by(mpa.coverage, group) %>%
    summarise(mean_biom_reps = mean(total_biomass), mean_biom_ref_reps = mean(total_biomass_ref), biom_diff = mean(total_biomass) - mean(total_biomass_ref), biom_rel_change = (mean(total_biomass) - mean(total_biomass_ref))/ mean(total_biomass_ref) * 100) %>%
    ungroup() %>%
    
    group_by(mpa.coverage) %>%
    mutate(total_biom_diff = sum(biom_diff)) %>%
    ungroup() %>%
    mutate(scenario = "S3 - Random EEZ")
  
  
  # Set factor levels to ensure consistent legend order
  Tot_biom_bygroups_noCC$group <- factor(Tot_biom_bygroups_noCC$group, levels = sp_group_levels)
  
  biom_S3_Med <- ggplot() +
    geom_bar(data = Tot_biom_bygroups_noCC, aes(x = mpa.coverage, y = biom_diff, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(data = Tot_biom_bygroups_noCC, aes(x = mpa.coverage, y = total_biom_diff), lwd = 0.5) +
    geom_line(data = Tot_biom_bygroups_noCC, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) +
    
    scale_fill_brewer(palette="BrBG", direction = -1) +
    
    labs(title= "MPA Scenario S3 - Random EEZ", subtitle= paste0(Fishing_scenario_name[f]),
         x ="MPA coverage (%)", y = "Biomass difference (tons)") 
  
  return(biom_S3_Med)
  
}

plot.catch.change.by.groups.S3 <- function(catch_by_groups, f = 1){
  
  
  Tot_catch_bygroups_noCC = catch_by_groups %>%
    filter(grepl('EEZ', scenario)) %>%
    
    filter(mpa == "Entire Mediterranean Sea") %>%
    mutate(group = case_when(group %in% c("Large-sized pelagic fish", "Medium-sized pelagic fish") ~ "Medium & large pelagic",
                             group %in% c("Large-sized demersal fish", "Medium-sized demersal fish") ~ "Medium & large demersal",
                             group %in% c("Large-sized benthic fish", "Medium-sized benthic fish") ~ "Medium & large benthic",
                             group %in% c("Small-sized pelagic fish") ~ "Small pelagic",
                             group %in% c("Small-sized benthic fish") ~ "Small benthic",
                             group %in% c("Small-sized demersal fish") ~ "Small demersal",
                             TRUE ~ group)) %>%
    group_by(mpa.coverage, mpa, scenario, group, scenario_rep) %>%
    summarise(total_catches = sum(tot.catches), total_catches_ref = sum(tot.catches.ref)) %>%
    ungroup() %>%
    group_by(mpa.coverage, group) %>%
    summarise(mean_catch_reps = mean(total_catches), mean_catch_ref_reps = mean(total_catches_ref), catch_diff = mean(total_catches) - mean(total_catches_ref), catch_rel_change = (mean(total_catches) - mean(total_catches_ref))/ mean(total_catches_ref) * 100) %>%
    ungroup() %>%
    
    group_by(mpa.coverage) %>%
    
    mutate(total_catch_diff = sum(catch_diff)) %>%
    ungroup() %>%
    mutate(scenario = "S3 - Random EEZ")
  
  
  
  # Set factor levels to ensure consistent legend order
  Tot_catch_bygroups_noCC$group <- factor(Tot_catch_bygroups_noCC$group, levels = sp_group_levels)
  
  
  catch_S3_Med <- ggplot() +
    geom_bar(data = Tot_catch_bygroups_noCC, aes(x = mpa.coverage, y = catch_diff, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(data = Tot_catch_bygroups_noCC, aes(x = mpa.coverage, y = total_catch_diff), lwd = 0.5) +
    geom_line(data = Tot_catch_bygroups_noCC, aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) +
    
    scale_fill_brewer(palette="BrBG", direction = -1) +  
    
    labs(title= "MPA Scenario S3 - Random EEZ", subtitle= paste0(Fishing_scenario_name[f]),
         x ="MPA coverage (%)", y = "Catch difference (tons)") 
  
  return(catch_S3_Med)    
  
  
}


plot.fig.A6.other.scenarios.S3 <- function(biomass_by_groups_data, catch_by_groups_data, save = TRUE){
  
  for (f in 1:3){
    # Biomass plots -------
    
    biomass_scen = plot.biomass.change.by.groups.S3(biomass_by_groups_data[[f]], f)
    
    biomass_scen_mod <- biomass_scen + theme(axis.title.x = element_blank(),
                                             plot.margin = unit(c(5, 3, 0.5, 4), "pt"))
    
    # Catch plots -------
    
    catch_scen = plot.catch.change.by.groups.S3(catch_by_groups_data[[f]], f)
    
    catch_scen_mod <- catch_scen + theme(plot.title = element_blank(), plot.subtitle = element_blank(),
                                         plot.margin = unit(c(0, 3, 0, 4), "pt"))
    # Store in lists instead of using `c()`
    biomass_plots_f[[f]] <- biomass_scen_mod
    catch_plots_f[[f]] <- catch_scen_mod
    
  }
  
  # Arrange plots together
  scen_plot <- ggarrange(plotlist = c(biomass_plots_f, catch_plots_f), 
                         ncol = 3, 
                         nrow = 2, 
                         common.legend = T,
                         legend = "bottom",
                         align = "hv")
  if (save == TRUE){
    ggsave(here(paste0("figures/appendix/Fig_A6_total_biomass_catch_by_groups_S3_random_EEZ.png")), dpi = 1000, height = 12, width = 12)
  }
}
  
 