#' Function to plot changes in biomass and catch relative to before MPA establishment by groups of species
#'
#' 
#' @param data OSMOSE-MED simulation outputs of total biomass and total catch by groups for each fishing redistribution strategy
#' 
#' @return Figure 3A (main text) &  Figures A6.3 - A6.6
#'
#' @export
#' 

plot.biomass.change.by.groups.with.sd <- function(biomass_by_groups, scen = 1, f = 1){
  
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
    mutate(biom_diff_group = total_biomass - total_biomass_ref,
           biom_change_group = (total_biomass - total_biomass_ref)/total_biomass_ref * 100) %>%
    
    group_by(mpa.coverage, scenario, group) %>%
    summarise(biom_diff_group_mean = mean(biom_diff_group),
              biom_diff_group_sd = sd(biom_diff_group),
              biom_change_group_mean = mean(biom_change_group),
              biom_change_group_sd = sd(biom_change_group)) %>%
    ungroup()
  
  
  All_scens_10_coverage = Tot_biom_bygroups_noCC %>%
    filter(scenario == lit.scenarios.name[scen]) %>%
    filter(mpa.coverage == 10)
  
  # Set factor levels to ensure consistent legend order
  All_scens_10_coverage$group <- factor(All_scens_10_coverage$group, levels = sp_group_levels)
  
  biom_Mazor8_Med <- ggplot(data = All_scens_10_coverage) +
    geom_bar(aes(x = group, y = biom_diff_group_mean, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    geom_errorbar(
      aes(
        x = group,
        ymin = biom_diff_group_mean - biom_diff_group_sd,
        ymax = biom_diff_group_mean + biom_diff_group_sd
      ),
      width = 0.25,
      linewidth = 0.3
    ) +

    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      )
    ) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    
    scale_fill_brewer(palette="BrBG", direction = -1) +
   
    labs(title= paste0("FPA Scenario ", MPA_scenarios_name[scen]), subtitle= paste0(Fishing_scenario_name[f]),
         x ="", y = "Biomass difference (tons)") 
  # ylim(-150000, 510000) # for Mazor scenario 8

    return(biom_Mazor8_Med)
}

plot.catch.change.by.groups.with.sd <- function(catch_by_groups, scen = 1, f = 1){
  
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
    mutate(catch_diff_group = total_catches - total_catches_ref,
           catch_change_group = (total_catches - total_catches_ref)/total_catches_ref * 100) %>%
    
    group_by(mpa.coverage, scenario, group) %>%
    summarise(catch_diff_group_mean = mean(catch_diff_group),
              catch_diff_group_sd = sd(catch_diff_group),
              catch_change_group_mean = mean(catch_change_group),
              catch_change_group_sd = sd(catch_change_group)) %>%
    ungroup()
    
  All_scens_10_coverage = Tot_catch_bygroups_noCC %>%
    filter(scenario == lit.scenarios.name[scen]) %>%
    filter(mpa.coverage == 10)
  
  # Set factor levels to ensure consistent legend order
  All_scens_10_coverage$group <- factor(All_scens_10_coverage$group, levels = sp_group_levels)
  
  catch_Mazor8_Med <- ggplot(data = All_scens_10_coverage) +
    geom_bar(aes(x = group, y = catch_diff_group_mean, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    geom_errorbar(
      aes(
        x = group,
        ymin = catch_diff_group_mean - catch_diff_group_sd,
        ymax = catch_diff_group_mean + catch_diff_group_sd
      ),
      width = 0.25,
      linewidth = 0.3
    ) +
    
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      )
    ) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    
    scale_fill_brewer(palette="BrBG", direction = -1) +
    
    labs(title= paste0("FPA Scenario ", MPA_scenarios_name[scen]), subtitle= paste0(Fishing_scenario_name[f]),
         x ="", y = "Catch difference (tons)") 
  # ylim(-150000, 510000) # for Mazor scenario 8
  
      return(catch_Mazor8_Med)
}


# other MPA scenarios and fishing redistribution strategies
  
plot.fig.A6.other.scenarios.S1.S4.S5.S6.with.sd <- function(biomass_by_groups_data, catch_by_groups_data, scen, save = TRUE){
  
  biomass_plots_f <- list()
  catch_plots_f <- list()
  
  for (f in 1:3){
    # Biomass plots -------
    biomass_scen = plot.biomass.change.by.groups.with.sd(biomass_by_groups_data[[f]], scen, f)
    
    biomass_scen_mod <- biomass_scen + theme(axis.title.x = element_blank(),
                                             plot.margin = unit(c(5, 3, 0.5, 4), "pt"))
    
    # Catch plots -------
    
    catch_scen = plot.catch.change.by.groups.with.sd(catch_by_groups_data[[f]], scen, f)
    
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
                         legend = "none",
                         align = "hv")
  if (save == TRUE){
    ggsave(here(paste0("figures/appendix/Fig_A6_total_biomass_catch_by_groups_", MPA_scenarios_name[scen], "_FPA10_withsd.png")), dpi = 1000, height = 12, width = 12)
  }
}

 ### random scenarios S2-basin

plot.biomass.change.by.groups.S2.with.sd <- function(biomass_by_groups, f = 1){

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
    mutate(biom_diff_group = total_biomass - total_biomass_ref,
           biom_change_group = (total_biomass - total_biomass_ref)/total_biomass_ref * 100) %>%
    
    group_by(mpa.coverage, group) %>%
    # mean and sd over 300 simulations - 30 replicate and 10 MPA scenarios
    summarise(biom_diff_group_mean = mean(biom_diff_group),
              biom_diff_group_sd = sd(biom_diff_group),
              biom_change_group_mean = mean(biom_change_group),
              biom_change_group_sd = sd(biom_change_group)) %>%
    ungroup() %>%
    mutate(scenario = "S2 - Random basin")
  
  All_scens_10_coverage = Tot_biom_bygroups_noCC %>%
    filter(mpa.coverage == 10)
  
  # Set factor levels to ensure consistent legend order
  All_scens_10_coverage$group <- factor(All_scens_10_coverage$group, levels = sp_group_levels)
  
  biom_S2_Med <- ggplot(data = All_scens_10_coverage) +
    geom_bar(aes(x = group, y = biom_diff_group_mean, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    geom_errorbar(
      aes(
        x = group,
        ymin = biom_diff_group_mean - biom_diff_group_sd,
        ymax = biom_diff_group_mean + biom_diff_group_sd
      ),
      width = 0.25,
      linewidth = 0.3
    ) +
    
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      )
    ) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    
    scale_fill_brewer(palette="BrBG", direction = -1) +
    
    labs(title= "FPA Scenario S2 - Random basin", subtitle= paste0(Fishing_scenario_name[f]),
         x ="", y = "Biomass difference (tons)")

  return(biom_S2_Med)

}

plot.catch.change.by.groups.S2.with.sd <- function(catch_by_groups, f = 1){
  
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
    mutate(catch_diff_group = total_catches - total_catches_ref,
           catch_change_group = (total_catches - total_catches_ref)/total_catches_ref * 100) %>%
    
    group_by(mpa.coverage, group) %>%
    # mean and sd over 300 simulations - 30 replicate and 10 MPA scenarios
    summarise(catch_diff_group_mean = mean(catch_diff_group),
              catch_diff_group_sd = sd(catch_diff_group),
              catch_change_group_mean = mean(catch_change_group),
              catch_change_group_sd = sd(catch_change_group)) %>%
    ungroup() %>%
    mutate(scenario = "S2 - Random basin")
  
  All_scens_10_coverage = Tot_catch_bygroups_noCC %>%
    filter(mpa.coverage == 10)
  
  # Set factor levels to ensure consistent legend order
  All_scens_10_coverage$group <- factor(All_scens_10_coverage$group, levels = sp_group_levels)
  
  catch_S2_Med <- ggplot(data = All_scens_10_coverage) +
    geom_bar(aes(x = group, y = catch_diff_group_mean, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    geom_errorbar(
      aes(
        x = group,
        ymin = catch_diff_group_mean - catch_diff_group_sd,
        ymax = catch_diff_group_mean + catch_diff_group_sd
      ),
      width = 0.25,
      linewidth = 0.3
    ) +
    
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      )
    ) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    
    scale_fill_brewer(palette="BrBG", direction = -1) +
    
    labs(title= "FPA Scenario S2 - Random basin", subtitle= paste0(Fishing_scenario_name[f]),
         x ="", y = "Catch difference (tons)") 

  return(catch_S2_Med)

  
}


plot.fig.A6.other.scenarios.S2.with.sd <- function(biomass_by_groups_data, catch_by_groups_data, save = TRUE){
  
  biomass_plots_f <- list()
  catch_plots_f <- list()
  
  for (f in 1:3){
    # Biomass plots -------
    
    biomass_scen = plot.biomass.change.by.groups.S2.with.sd(biomass_by_groups_data[[f]], f)
    
    biomass_scen_mod <- biomass_scen + theme(axis.title.x = element_blank(),
                                             plot.margin = unit(c(5, 3, 0.5, 4), "pt"))
    
    # Catch plots -------
    
    catch_scen = plot.catch.change.by.groups.S2.with.sd(catch_by_groups_data[[f]], f)
    
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
                         legend = "none",
                         align = "hv")
  if (save == TRUE){
    ggsave(here(paste0("figures/appendix/Fig_A6_total_biomass_catch_by_groups_S2_random_FPA10_withsd.png")), dpi = 1000, height = 12, width = 12)
  }
}

### random scenarios S3-EEZ

plot.biomass.change.by.groups.S3.with.sd <- function(biomass_by_groups, f = 1){
  
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
    mutate(biom_diff_group = total_biomass - total_biomass_ref,
           biom_change_group = (total_biomass - total_biomass_ref)/total_biomass_ref * 100) %>%
    
    group_by(mpa.coverage, group) %>%
    # mean and sd over 300 simulations - 30 replicate and 10 MPA scenarios
    summarise(biom_diff_group_mean = mean(biom_diff_group),
              biom_diff_group_sd = sd(biom_diff_group),
              biom_change_group_mean = mean(biom_change_group),
              biom_change_group_sd = sd(biom_change_group)) %>%
    ungroup() %>%
    mutate(scenario = "S3 - Random EEZ")
  
  All_scens_10_coverage = Tot_biom_bygroups_noCC %>%
    filter(mpa.coverage == 10)
  
  # Set factor levels to ensure consistent legend order
  All_scens_10_coverage$group <- factor(All_scens_10_coverage$group, levels = sp_group_levels)
  
  biom_S3_Med <- ggplot(data = All_scens_10_coverage) +
    geom_bar(aes(x = group, y = biom_diff_group_mean, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    geom_errorbar(
      aes(
        x = group,
        ymin = biom_diff_group_mean - biom_diff_group_sd,
        ymax = biom_diff_group_mean + biom_diff_group_sd
      ),
      width = 0.25,
      linewidth = 0.3
    ) +
    
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      )
    ) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    
    scale_fill_brewer(palette="BrBG", direction = -1) +
    
    labs(title= "FPA Scenario S3 - Random EEZ", subtitle= paste0(Fishing_scenario_name[f]),
         x ="", y = "Biomass difference (tons)")
  
  return(biom_S3_Med)
  
}

plot.catch.change.by.groups.S3.with.sd <- function(catch_by_groups, f = 1){
  
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
    mutate(catch_diff_group = total_catches - total_catches_ref,
           catch_change_group = (total_catches - total_catches_ref)/total_catches_ref * 100) %>%
    
    group_by(mpa.coverage, group) %>%
    # mean and sd over 300 simulations - 30 replicate and 10 MPA scenarios
    summarise(catch_diff_group_mean = mean(catch_diff_group),
              catch_diff_group_sd = sd(catch_diff_group),
              catch_change_group_mean = mean(catch_change_group),
              catch_change_group_sd = sd(catch_change_group)) %>%
    ungroup() %>%
    mutate(scenario = "S3 - Random EEZ")
  
  All_scens_10_coverage = Tot_catch_bygroups_noCC %>%
    filter(mpa.coverage == 10)
  
  # Set factor levels to ensure consistent legend order
  All_scens_10_coverage$group <- factor(All_scens_10_coverage$group, levels = sp_group_levels)
  
  catch_S3_Med <- ggplot(data = All_scens_10_coverage) +
    geom_bar(aes(x = group, y = catch_diff_group_mean, fill = group), colour = "black", lwd = 0.1, stat="identity", alpha=0.8, width = 0.9, color = "gray60") +
    geom_line(aes(x = mpa.coverage, y = 0), colour = "gray5", lwd = 0.3, lty = "solid") +
    
    geom_errorbar(
      aes(
        x = group,
        ymin = catch_diff_group_mean - catch_diff_group_sd,
        ymax = catch_diff_group_mean + catch_diff_group_sd
      ),
      width = 0.25,
      linewidth = 0.3
    ) +
    
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12)) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      )
    ) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_blank(), #change legend title font size
          legend.text = element_text(size=10)) + #change legend text font size 
    
    scale_fill_brewer(palette="BrBG", direction = -1) +
    
    labs(title= "FPA Scenario S3 - Random EEZ", subtitle= paste0(Fishing_scenario_name[f]),
         x ="", y = "Catch difference (tons)") 
  
  return(catch_S3_Med)
  
}


plot.fig.A6.other.scenarios.S3.with.sd <- function(biomass_by_groups_data, catch_by_groups_data, save = TRUE){
  
  
  biomass_plots_f <- list()
  catch_plots_f <- list()
  
  for (f in 1:3){
    # Biomass plots -------
    
    biomass_scen = plot.biomass.change.by.groups.S3.with.sd(biomass_by_groups_data[[f]], f)
    
    biomass_scen_mod <- biomass_scen + theme(axis.title.x = element_blank(),
                                             plot.margin = unit(c(5, 3, 0.5, 4), "pt"))
    
    # Catch plots -------
    
    catch_scen = plot.catch.change.by.groups.S3.with.sd(catch_by_groups_data[[f]], f)
    
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
                         legend = "none",
                         align = "hv")
  if (save == TRUE){
    ggsave(here(paste0("figures/appendix/Fig_A6_total_biomass_catch_by_groups_S3_random_EEZ_FPA10_withsd.png")), dpi = 1000, height = 12, width = 12)
  }
}
  
 