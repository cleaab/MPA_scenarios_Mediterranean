#' Figure appendix A6.2
#'
#' 
#' @param osmose outputs - predatorPressure
#' 
#' @return predation pressure
#'
#' @export
#' 


calculate.predation.pressure.by.groups = function(outputDir, vertical_distrib) {
  data = read_osmose(path = outputDir)
  predPressure = get_var(data, "predatorPressure")
  
  for (i in 1:101){
    # Apply over time (dim 1) and reps (dim 3), keeping them separate
    diet_mean <- apply(predPressure[[i]], c(2, 3), mean, na.rm = TRUE)  # prey x reps
    diet_mean_df = as.data.frame(diet_mean) %>%
      rownames_to_column("prey") %>%
      pivot_longer(-prey, names_to = "rep", values_to = "ingestion") %>%
      mutate(predator_species = rownames(diet_mean)[i])  # name of predator species i
    
    if (i == 1){
      diet_mean_all_species = diet_mean_df
    } else {
      diet_mean_all_species = rbind(diet_mean_all_species, diet_mean_df)
    }
  }
  return(diet_mean_all_species)
}

plot.fig.S6.2 <- function(save = TRUE){
  
  # with mpas
  diet_mean_all_species = calculate.predation.pressure.by.groups(outputDir.mpa, vertical_distrib)
  
  # without mpas
  diet_mean_all_species_NoMPA = calculate.predation.pressure.by.groups(outputDir.nompa, vertical_distrib)
  
  # plots
  # Small pelagic predation pressure ---------------------------------------
  diet_mean_all_species_ed = diet_mean_all_species %>%
    rename(ingestion_MPA = ingestion)
  
  diet_mean_all_species_NoMPA_ed = diet_mean_all_species_NoMPA %>%
    rename(ingestion_noMPA = ingestion)
  
  vertical_distrib_ed = vertical_distrib %>%
    dplyr::select(-species_id)
  
  
  diets = diet_mean_all_species_ed %>%
    left_join(diet_mean_all_species_NoMPA_ed, by = c("prey", "predator_species", "rep")) %>%
    rename(species_name = predator_species) %>%
    left_join(vertical_distrib_ed, by = "species_name") %>%
    rename(pred_name = species_name) %>%
    rename(pred_group = group, species_name = prey) %>%
    left_join(vertical_distrib_ed, by = "species_name") %>%
    rename(prey_name = species_name, prey_group = group) %>%
    mutate(prey_group = case_when(is.na(prey_group) ~ "LTL",
                                  TRUE ~ prey_group))
    
  diets_group = diets %>%
    group_by(rep, prey_group, pred_group) %>%
    summarize(ingestion_MPA = sum(ingestion_MPA, na.rm = T),
              ingestion_noMPA = sum(ingestion_noMPA, na.rm = T)) %>%
    ungroup() %>%
    mutate(ingestion_change = (ingestion_MPA - ingestion_noMPA)/ingestion_noMPA * 100,
           ingestion_diff = ingestion_MPA - ingestion_noMPA) %>%
    filter(ingestion_noMPA > 0) %>%
    group_by(prey_group, pred_group) %>%
    summarize(ingestion_change_mean = mean(ingestion_change, na.rm = T),
              ingestion_change_sd = sd(ingestion_change, na.rm = T),
              ingestion_diff_mean = mean(ingestion_diff, na.rm = T),
              ingestion_diff_sd = sd(ingestion_diff, na.rm = T)) %>%
    ungroup()
  
  
  # calculate total relative change of ingestion by prey group
  tot_ingestion_change = diets %>%
    group_by(rep, prey_group) %>%
    summarize(ingestion_MPA = sum(ingestion_MPA, na.rm = T),
              ingestion_noMPA = sum(ingestion_noMPA, na.rm = T)) %>%
    ungroup() %>%
    mutate(ingestion_change = (ingestion_MPA - ingestion_noMPA)/ingestion_noMPA * 100) %>%
    filter(ingestion_noMPA > 0) %>%
    group_by(prey_group) %>%
    summarize(ingestion_change_mean = mean(ingestion_change, na.rm = T),
              ingestion_change_sd = sd(ingestion_change, na.rm = T))
  
  tot_abs_ingestion_change = diets %>%
    group_by(rep, prey_group) %>%
    summarize(ingestion_MPA = sum(ingestion_MPA, na.rm = T),
              ingestion_noMPA = sum(ingestion_noMPA, na.rm = T)) %>%
    ungroup() %>%
    mutate(ingestion_change = ingestion_MPA - ingestion_noMPA) %>%
    filter(ingestion_noMPA > 0) %>%
    group_by(prey_group) %>%
    summarize(ingestion_change_mean = mean(ingestion_change, na.rm = T),
              ingestion_change_sd = sd(ingestion_change, na.rm = T))
  
  
  # small pelagic
  pred_pressure_small_pel = diets_group %>%
    filter(prey_group == "Small-sized pelagic fish")
  
  smallpel.rel.change <- ggplot(pred_pressure_small_pel) +
    geom_col(aes(y = pred_group, x = ingestion_change_mean), position = "stack") +
    geom_errorbar(aes(y = pred_group,
                      xmin = ingestion_change_mean - ingestion_change_sd,
                      xmax = ingestion_change_mean + ingestion_change_sd),
                  position = position_dodge(width = 0.4),
                  width = 0.25) +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 13)) +
    theme_bw() +
    labs(title = "Predation pressure on small pelagic fish", y ="", x = "Relative change in ingestion (%)")
  
  smallpel.abs.change <- ggplot(pred_pressure_small_pel) +
    geom_col(aes(y = pred_group, x = ingestion_diff_mean), position = "stack") +
    geom_errorbar(aes(y = pred_group,
                      xmin = ingestion_diff_mean - ingestion_diff_sd,
                      xmax = ingestion_diff_mean + ingestion_diff_sd),
                  position = position_dodge(width = 0.4),
                  width = 0.25) +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 13)) +
    theme_bw() +
    labs(title = "Predation pressure on small pelagic fish", y ="", x = "Ingestion difference (tons)")
  
  # crustaceans
  pred_pressure_crustacean = diets_group %>%
    filter(prey_group == "Crustacean")
  
  crustacean.rel.change <- ggplot(pred_pressure_crustacean) +
    geom_col(aes(y = pred_group, x = ingestion_change_mean), position = "stack") +
    geom_errorbar(aes(y = pred_group,
                      xmin = ingestion_change_mean - ingestion_change_sd,
                      xmax = ingestion_change_mean + ingestion_change_sd),
                  position = position_dodge(width = 0.4),
                  width = 0.25) +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 13)) +
    theme_bw() +
    labs(title = "Predation pressure on crustaceans", y ="", x = "Relative change in ingestion (%)")
  
  crustacean.abs.change <- ggplot(pred_pressure_crustacean) +
    geom_col(aes(y = pred_group, x = ingestion_diff_mean), position = "stack") +
    geom_errorbar(aes(y = pred_group,
                      xmin = ingestion_diff_mean - ingestion_diff_sd,
                      xmax = ingestion_diff_mean + ingestion_diff_sd),
                  position = position_dodge(width = 0.4),
                  width = 0.25) +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 13)) +
    theme_bw() +
    labs(title = "Predation pressure on crustaceans", y ="", x = "Ingestion difference (tons)")
  
  # cephalopods
  pred_pressure_cephalopod = diets_group %>%
    filter(prey_group == "Cephalopod")
  
  cephalopod.rel.change <- ggplot(pred_pressure_cephalopod) +
    geom_col(aes(y = pred_group, x = ingestion_change_mean), position = "stack") +
    geom_errorbar(aes(y = pred_group,
                      xmin = ingestion_change_mean - ingestion_change_sd,
                      xmax = ingestion_change_mean + ingestion_change_sd),
                  position = position_dodge(width = 0.9),
                  width = 0.25) +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 13)) +
    theme_bw() +
    labs(title = "Predation pressure on cephalopods", y ="", x = "Relative change in ingestion (%)")
  
  cephalopod.abs.change <- ggplot(pred_pressure_cephalopod) +
    geom_col(aes(y = pred_group, x = ingestion_diff_mean), position = "stack") +
    geom_errorbar(aes(y = pred_group,
                      xmin = ingestion_diff_mean - ingestion_diff_sd,
                      xmax = ingestion_diff_mean + ingestion_diff_sd),
                  position = position_dodge(width = 0.9),
                  width = 0.25) +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 13)) +
    theme_bw() +
    labs(title = "Predation pressure on cephalopods", y ="", x = "Ingestion difference (tons)")
  
  
  # -----------------------------------------------------------------------------------------
  # 3 plots - ingestion change --------------------------------------------------------------
  # figure A6.2
  
  # Join the 3 plots - rel change
  smallpel.rel.change_mod = smallpel.rel.change + theme(
    legend.position = "none") +
    #theme(plot.margin = margin(0,0.5,0,0.5, "cm")) +
    labs(title = "Small pelagic fish (+ 3.4 %)") +
    theme(plot.margin = margin(0.5,0.5,0.5,0, "cm")) # 2 -right, 4 - left
  
  cephalopod.rel.change_mod = cephalopod.rel.change + theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
    labs(title = "Cephalopods (+ 3.0 %)") +
    
    #theme(plot.margin = margin(0,0.5,0,0.5, "cm"))
    theme(plot.margin = margin(0.5,0.5,0.5,0, "cm")) # 2 -right, 4 - left
  
  crustacean.rel.change_mod = crustacean.rel.change + theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
    labs(title = "Crustaceans (+ 2.1 %)") +
    #theme(plot.margin = margin(0,0.5,0,0.5, "cm"))
    theme(plot.margin = margin(0.5,1,0.5,0, "cm")) # 2 -right, 4 - left
  
  pred_pressure_LTL <- plot_grid(smallpel.rel.change_mod, cephalopod.rel.change_mod, crustacean.rel.change_mod,
                                 ncol=3,
                                 nrow=1,
                                 rel_widths = c(1.4,0.8,0.8))
  
  # extract the legend from one of the plots
  
  legend <- get_legend(
    crustacean.rel.change + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom")
  )
  
  # add the legend underneath the row we made earlier. Give it 10%
  # of the height of one plot (via rel_heights).
  plot_grid(pred_pressure_LTL, legend, ncol = 1, rel_heights = c(1, .1))
  
  if (save == TRUE){
    ggsave(here(paste0("figures/appendix/FigS6.2_predation_pressure_S5_10pct_GSAprop_with_sd.png")), dpi = 300, height = 4, width = 9)
    
  }
  
  # Join the 3 plots - abs change
  smallpel.abs.change_mod = smallpel.abs.change + theme(
    legend.position = "none") +
    #theme(plot.margin = margin(0,0.5,0,0.5, "cm")) +
    labs(title = "Small pelagic fish (+ 3.4 %)") +
    theme(plot.margin = margin(0.5,0.5,0.5,0, "cm")) # 2 -right, 4 - left
  
  cephalopod.abs.change_mod = cephalopod.abs.change + theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
    labs(title = "Cephalopods (+ 3.0 %)") +
    
    #theme(plot.margin = margin(0,0.5,0,0.5, "cm"))
    theme(plot.margin = margin(0.5,0.5,0.5,0, "cm")) # 2 -right, 4 - left
  
  crustacean.abs.change_mod = crustacean.abs.change + theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
    labs(title = "Crustaceans (+ 2.1 %)") +
    #theme(plot.margin = margin(0,0.5,0,0.5, "cm"))
    theme(plot.margin = margin(0.5,1,0.5,0, "cm")) # 2 -right, 4 - left
  
  pred_pressure_LTL <- plot_grid(smallpel.abs.change_mod, cephalopod.abs.change_mod, crustacean.abs.change_mod,
                                 ncol=3,
                                 nrow=1,
                                 rel_widths = c(1.4,0.8,0.8))
  
  # extract the legend from one of the plots
  
  legend <- get_legend(
    crustacean.rel.change + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom")
  )
  
  # add the legend underneath the row we made earlier. Give it 10%
  # of the height of one plot (via rel_heights).
  plot_grid(pred_pressure_LTL, legend, ncol = 1, rel_heights = c(1, .1))
  
  if (save == TRUE){
    ggsave(here(paste0("figures/appendix/FigS6.2_predation_pressure_S5_10pct_GSAprop_absolute_change_with_sd.png")), dpi = 300, height = 4, width = 9)
    
  }
}


