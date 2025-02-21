

plot.figA5.1 <- function(Biomass, Catch, save = TRUE){
  # Variogram catches & biomass
  # BIOMASS & CATCH indicators - NO MPA all fishing scenarios
  Biom_nompa = Biomass %>%
    filter(scenario == "Micheli") %>%
    filter(mpa.coverage == 0) %>%
    dplyr::select(tot.biomass, scenario_rep)
  
  Catch_nompa = Catch %>%
    filter(scenario == "Micheli") %>%
    filter(mpa.coverage == 0) %>%
    dplyr::select(tot.catches, scenario_rep)
  
  # SEM --------------------------------------------------------------------------
  # BIOMASS
  variability <- c()
  
  for (i in 1:30){
    variance <- Biom_nompa %>%
      filter(scenario_rep < i+1)
    indic <- variance$tot.biomass
    sd_value <- sd(indic)
    sem_value <- sd_value/sqrt(i)
    if (i == 1){
      variability <- sem_value
    } else{
      variability <- c(variability, sem_value)
      
    }
  }
  
  variability_biomass_df = data.frame(variability)
  variability_biomass_df$rep = seq(1,30,1)
  variability_biomass_df$indic = "Biomass"
  
  # CATCH
  variability <- c()
  
  for (i in 1:30){
    variance <- Catch_nompa %>%
      filter(scenario_rep < i+1)
    indic <- variance$tot.catches
    sd_value <- sd(indic)
    sem_value <- sd_value/sqrt(i)
    if (i == 1){
      variability <- sem_value
    } else{
      variability <- c(variability, sem_value)
      
    }
  }
  
  variability_catch_df = data.frame(variability)
  variability_catch_df$rep = seq(1,30,1)
  variability_catch_df$indic = "Catch"
  
  variability_df = rbind(variability_biomass_df,variability_catch_df)
  
  ggplot(variability_df) +
    geom_line(aes(y = variability, x = rep, lty = indic)) +
    theme_bw() +
    theme(legend.position = "right",
          legend.title = element_blank()) +
    labs(x = "Number of Replicates",
         y = "Standard Error of Mean (SEM)")
  
  if (save == TRUE){
    ggsave(here("figures/appendix/figA5.1_variability_total_biomass_catch.png"), dpi = 300, height = 5, width = 7)
    
  }
  
}

plot.figA5.2 <- function(Biomass, Catch, nreps = 30, save = TRUE){
  
  for(scen in 1:length(lit.scenarios.name)){
    
    Biomass_indic = Biomass %>% 
      filter(scenario == all_mpa_scenarios[scen]) %>%
      dplyr::select(tot.biomass, scenario_rep, mpa.coverage)
    
    variability <- c()
    
    for (mpa_cover in 1:31){
      variance <- Biomass_indic %>%
        filter(mpa.coverage == (mpa_cover-1))
      
      indic <- variance$tot.biomass
      sd_value <- sd(indic)
      sem_value <- sd_value/sqrt(nreps)
      
      if (mpa_cover == 1){
        variability <- sem_value
      } else{
        variability <- c(variability, sem_value)
        
      }
    }
    
    variability_biomass_df = data.frame(variability)
    variability_biomass_df$mpa_coverage = seq(0,30,1)
    variability_biomass_df$indic = "Biomass"
    variability_biomass_df$scenario = all_mpa_scenarios[scen]
    
    if (scen == 1){
      variability_biomass_lit_scen = variability_biomass_df
    } else{
      variability_biomass_lit_scen = rbind(variability_biomass_lit_scen, variability_biomass_df)
    }
  }
  
  
  for(scen in 1:length(lit.scenarios.name)){
    
    Catch_indic = Catch %>%
      filter(scenario == all_mpa_scenarios[scen]) %>%
      dplyr::select(tot.catches, scenario_rep, mpa.coverage)
    
    variability <- c()
    
    for (mpa_cover in 1:31){
      variance <- Catch_indic %>%
        filter(mpa.coverage == (mpa_cover - 1))
      
      indic <- variance$tot.catches
      sd_value <- sd(indic)
      sem_value <- sd_value/sqrt(nreps)
      
      if (mpa_cover == 1){
        variability <- sem_value
      } else{
        variability <- c(variability, sem_value)
        
      }
    }
    
    variability_catch_df = data.frame(variability)
    variability_catch_df$mpa_coverage = seq(0,30,1)
    variability_catch_df$indic = "Catch"
    variability_catch_df$scenario = all_mpa_scenarios[scen]
    
    if (scen == 1){
      variability_catch_lit_scen = variability_catch_df
    } else{
      variability_catch_lit_scen = rbind(variability_catch_lit_scen, variability_catch_df)
    }
    
  }   
  
  variability_df = rbind(variability_biomass_lit_scen,variability_catch_lit_scen)
  
  mpa_colors <- c("Existing network" = "gold", 
                  "Micheli" = "darkorange", 
                  "Mazor scenario 8" = "darkslateblue", 
                  "Mazor scenario 9" = "darkorchid")
  
  legend_scenarios <- c("Existing network", "Micheli", "Mazor scenario 8", "Mazor scenario 9")
  
  legend_labels <- c("S1-Current", "S4-Micheli", "S5-Mazor GFCM", "S6-Mazor SAUP")
  
  ggplot(variability_df) +
    geom_line(aes(y = variability, x = mpa_coverage, col = scenario, lty = indic)) +
    theme_bw() +
    theme(legend.position = "right",
          legend.title = element_blank()) +
    labs(x = "MPA coverage (%)",
         y = "Standard Error of Mean (SEM)") +
    scale_colour_manual(values = mpa_colors, breaks = legend_scenarios, labels = legend_labels) 
  
  if(save == TRUE){
    ggsave(here("figures/appendix/figA5.2_variability_total_biomass_catch_30reps_with_mpa.png"), dpi = 300, height = 5, width = 7)
    
  }
  
  
  
}
  
