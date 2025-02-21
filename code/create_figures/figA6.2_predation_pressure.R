#' Figure appendix A6.2
#'
#' 
#' @param osmose outputs - predatorPressure
#' 
#' @return predation pressure
#'
#' @export
#' 


calculate.predation.pressure.by.groups <- function(output.Dir, vertical_distrib){
  ## Without MPAs - scenarios S5-GFCM with fishing effort redistribution strategy proportional by GSA
  data = read_osmose(path=output.Dir)
  predPressure = get_var(data, "predatorPressure")
  
  for (i in 1:101){
    diet_mean <- apply(predPressure[[i]], c(2), mean, na.rm =TRUE) # mean over time and reps for species 1
    diet_mean_df = as.data.frame(diet_mean)
    colnames(diet_mean_df) = rownames(diet_mean_df)[i]
    if (i == 1){
      diet_mean_all_species = diet_mean_df
    } else{
      diet_mean_all_species = cbind(diet_mean_all_species, diet_mean_df)
    }
    
  }
  diet_mean_all_species$prey = rownames(diet_mean_all_species)
  
  return(diet_mean_all_species)
  
}

plot.fig.A6.2 <- function(save = TRUE){
  
  # with mpas
  diet_mean_all_species = calculate.predation.pressure.by.groups(outputDir.mpa, vertical_distrib)
  
  # without mpas
  diet_mean_all_species_NoMPA = calculate.predation.pressure.by.groups(outputDir.nompa, vertical_distrib)
  
  # plots
  # Small pelagic predation pressure ---------------------------------------
  diet_pivot_MPA = diet_mean_all_species %>%
    pivot_longer(., cols = c(1:101), names_to = "predator", values_to = "ingestion") %>%
    rename(species_name = predator) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(predator = group, species_name = prey) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(prey = group) %>%
    filter(prey == "Small-sized pelagic fish") %>%
    group_by(predator) %>%
    summarise(tot_ingestion = sum(ingestion))
  
  diet_pivot_NoMPA = diet_mean_all_species_NoMPA %>%
    pivot_longer(., cols = c(1:101), names_to = "predator", values_to = "ingestion") %>%
    rename(species_name = predator) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(predator = group, species_name = prey) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(prey = group) %>%
    filter(prey == "Small-sized pelagic fish") %>%
    group_by(predator) %>%
    summarise(tot_ingestion_NoMPA = sum(ingestion))
  
  
  pred_pressure_small_pel = diet_pivot_MPA %>%
    left_join(diet_pivot_NoMPA, by = "predator") %>%
    mutate(diff_ingestion = tot_ingestion - tot_ingestion_NoMPA) %>%
    mutate(rel_change_ingestion = (tot_ingestion - tot_ingestion_NoMPA)/tot_ingestion_NoMPA * 100) %>%
    mutate(predator = case_when(predator == "Large-sized benthic fish" ~ "Large benthic",
                                predator == "Medium-sized benthic fish" ~ "Medium benthic",
                                predator == "Medium-sized demersal fish" ~ "Medium demersal",
                                predator == "Medium-sized pelagic fish" ~ "Medium pelagic",
                                predator == "Large-sized pelagic fish" ~ "Large pelagic",
                                predator == "Large-sized demersal fish" ~ "Large demersal",
                                predator == "Small-sized pelagic fish" ~ "Small pelagic",
                                predator == "Small-sized demersal fish" ~ "Small demersal",
                                predator == "Small-sized benthic fish" ~ "Small benthic",
                                TRUE ~ predator)) %>%
    mutate(total_rel_change_ingestion = (sum(tot_ingestion) - sum(tot_ingestion_NoMPA))/sum(tot_ingestion_NoMPA) * 100)
  
  smallpel.rel.change <- ggplot(pred_pressure_small_pel) +
    geom_col(aes(y = predator, x = rel_change_ingestion), position = "stack") +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 16)) +
    labs(title = "Predation pressure on small pelagic fish", y ="", x = "Ingestion change (%)") +
    theme_bw()
  
  
  smallpel.absolute.change <- ggplot(pred_pressure_small_pel) +
    geom_col(aes(y = predator, x = diff_ingestion), position = "stack") +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 16)) +
    labs(title = "Predation pressure on small pelagic fish", y ="", x = "Ingestion difference (tons)")
  
  
  pred_pressure_small_pel = diet_pivot_MPA %>%
    left_join(diet_pivot_NoMPA, by = "predator") %>%
    rename("With MPAs 10% coverage" = "tot_ingestion", "Without MPAs" = "tot_ingestion_NoMPA") %>%
    pivot_longer(2:3, names_to = "simu", values_to = "ingestion") %>%
    mutate(predator = case_when(predator == "Large-sized benthic fish" ~ "Large benthic",
                                predator == "Medium-sized benthic fish" ~ "Medium benthic",
                                predator == "Medium-sized demersal fish" ~ "Medium demersal",
                                predator == "Medium-sized pelagic fish" ~ "Medium pelagic",
                                predator == "Large-sized pelagic fish" ~ "Large pelagic",
                                predator == "Large-sized demersal fish" ~ "Large demersal",
                                predator == "Small-sized pelagic fish" ~ "Small pelagic",
                                predator == "Small-sized demersal fish" ~ "Small demersal",
                                predator == "Small-sized benthic fish" ~ "Small benthic",
                                TRUE ~ predator)) 
  
  smallpel.absolute.change.comparison <- ggplot(pred_pressure_small_pel) +
    geom_col(aes(y = predator, x = ingestion, fill = simu), position = "dodge") +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_blank()) +
    labs(title = "Predation pressure on small pelagic fish", y ="", x = "Ingestion (tons)") +
    xlim(0,12000)
  
  
  # Cephalopod predation pressure -------------------------------------
  diet_pivot_MPA = diet_mean_all_species %>%
    pivot_longer(., cols = c(1:101), names_to = "predator", values_to = "ingestion") %>%
    rename(species_name = predator) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(predator = group, species_name = prey) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(prey = group) %>%
    filter(prey == "Cephalopod") %>%
    group_by(predator) %>%
    summarise(tot_ingestion = sum(ingestion))
  
  
  diet_pivot_NoMPA = diet_mean_all_species_NoMPA %>%
    pivot_longer(., cols = c(1:101), names_to = "predator", values_to = "ingestion") %>%
    rename(species_name = predator) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(predator = group, species_name = prey) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(prey = group) %>%
    filter(prey == "Cephalopod") %>%
    group_by(predator) %>%
    summarise(tot_ingestion_NoMPA = sum(ingestion))
  
  
  pred_pressure_cephalopod = diet_pivot_MPA %>%
    left_join(diet_pivot_NoMPA, by = "predator") %>%
    mutate(diff_ingestion = tot_ingestion - tot_ingestion_NoMPA) %>%
    mutate(rel_change_ingestion = (tot_ingestion - tot_ingestion_NoMPA)/tot_ingestion_NoMPA * 100) %>%
    mutate(predator = case_when(predator == "Large-sized benthic fish" ~ "Large benthic",
                                predator == "Medium-sized benthic fish" ~ "Medium benthic",
                                predator == "Medium-sized demersal fish" ~ "Medium demersal",
                                predator == "Medium-sized pelagic fish" ~ "Medium pelagic",
                                predator == "Large-sized pelagic fish" ~ "Large pelagic",
                                predator == "Large-sized demersal fish" ~ "Large demersal",
                                predator == "Small-sized pelagic fish" ~ "Small pelagic",
                                predator == "Small-sized demersal fish" ~ "Small demersal",
                                predator == "Small-sized benthic fish" ~ "Small benthic",
                                TRUE ~ predator)) %>%
    mutate(total_rel_change_ingestion = (sum(tot_ingestion) - sum(tot_ingestion_NoMPA))/sum(tot_ingestion_NoMPA) * 100)
  
  ceph.rel.change <- ggplot(pred_pressure_cephalopod) +
    geom_col(aes(y = predator, x = rel_change_ingestion), position = "stack") +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 16)) +
    labs(title = "Predation pressure on cephalopods", y ="", x = "Ingestion change (%)") +
    theme_bw()
  
  ceph.absolute.change <- ggplot(pred_pressure_cephalopod) +
    geom_col(aes(y = predator, x = diff_ingestion), position = "stack") +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 16)) +
    labs(title = "Predation pressure on cephalopods", y ="", x = "Ingestion difference (tons)")
  
  pred_pressure_cephalopod = diet_pivot_MPA %>%
    left_join(diet_pivot_NoMPA, by = "predator") %>%
    rename("With MPAs 10% coverage" = "tot_ingestion", "Without MPAs" = "tot_ingestion_NoMPA") %>%
    pivot_longer(2:3, names_to = "simu", values_to = "ingestion") %>%
    mutate(predator = case_when(predator == "Large-sized benthic fish" ~ "Large benthic",
                                predator == "Medium-sized benthic fish" ~ "Medium benthic",
                                predator == "Medium-sized demersal fish" ~ "Medium demersal",
                                predator == "Medium-sized pelagic fish" ~ "Medium pelagic",
                                predator == "Large-sized pelagic fish" ~ "Large pelagic",
                                predator == "Large-sized demersal fish" ~ "Large demersal",
                                predator == "Small-sized pelagic fish" ~ "Small pelagic",
                                predator == "Small-sized demersal fish" ~ "Small demersal",
                                predator == "Small-sized benthic fish" ~ "Small benthic",
                                TRUE ~ predator)) 
  
  ceph.absolute.change.comparison <- ggplot(pred_pressure_cephalopod) +
    geom_col(aes(y = predator, x = ingestion, fill = simu), position = "dodge") +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_blank()) +
    labs(title = "Predation pressure on cephalopods", y ="", x = "Ingestion (tons)") +
    xlim(0,12000)
  
  
  # Crustacean predation pressure -------------------------------------
  diet_pivot_MPA = diet_mean_all_species %>%
    pivot_longer(., cols = c(1:101), names_to = "predator", values_to = "ingestion") %>%
    rename(species_name = predator) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(predator = group, species_name = prey) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(prey = group) %>%
    filter(prey == "Crustacean") %>%
    group_by(predator) %>%
    summarise(tot_ingestion = sum(ingestion))
  
  
  diet_pivot_NoMPA = diet_mean_all_species_NoMPA %>%
    pivot_longer(., cols = c(1:101), names_to = "predator", values_to = "ingestion") %>%
    rename(species_name = predator) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(predator = group, species_name = prey) %>%
    left_join(vertical_distrib, by = "species_name") %>%
    dplyr::select(-species_name) %>%
    rename(prey = group) %>%
    filter(prey == "Crustacean") %>%
    group_by(predator) %>%
    summarise(tot_ingestion_NoMPA = sum(ingestion))
  
  
  pred_pressure_crustacean = diet_pivot_MPA %>%
    left_join(diet_pivot_NoMPA, by = "predator") %>%
    mutate(diff_ingestion = tot_ingestion - tot_ingestion_NoMPA) %>%
    mutate(rel_change_ingestion = (tot_ingestion - tot_ingestion_NoMPA)/tot_ingestion_NoMPA * 100) %>%
    mutate(predator = case_when(predator == "Large-sized benthic fish" ~ "Large benthic",
                                predator == "Medium-sized benthic fish" ~ "Medium benthic",
                                predator == "Medium-sized demersal fish" ~ "Medium demersal",
                                predator == "Medium-sized pelagic fish" ~ "Medium pelagic",
                                predator == "Large-sized pelagic fish" ~ "Large pelagic",
                                predator == "Large-sized demersal fish" ~ "Large demersal",
                                predator == "Small-sized pelagic fish" ~ "Small pelagic",
                                predator == "Small-sized demersal fish" ~ "Small demersal",
                                predator == "Small-sized benthic fish" ~ "Small benthic",
                                TRUE ~ predator)) %>%
    mutate(total_rel_change_ingestion = (sum(tot_ingestion) - sum(tot_ingestion_NoMPA))/sum(tot_ingestion_NoMPA) * 100)
  
  
  crust.rel.change <- ggplot(pred_pressure_crustacean) +
    geom_col(aes(y = predator, x = rel_change_ingestion), position = "stack") +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 16)) +
    labs(title = "Predation pressure on crustaceans", y ="", x = "Ingestion change (%)") +
    theme_bw()
  
  crust.absolute.change <- ggplot(pred_pressure_crustacean) +
    geom_col(aes(y = predator, x = diff_ingestion), position = "stack") +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 16)) +
    labs(title = "Predation pressure on crustaceans", y ="", x = "Ingestion difference (tons)")
  crust.absolute.change
  
  
  pred_pressure_crustacean = diet_pivot_MPA %>%
    left_join(diet_pivot_NoMPA, by = "predator") %>%
    rename("With MPAs 10% coverage" = "tot_ingestion", "Without MPAs" = "tot_ingestion_NoMPA") %>%
    pivot_longer(2:3, names_to = "simu", values_to = "ingestion") %>%
    mutate(predator = case_when(predator == "Large-sized benthic fish" ~ "Large benthic",
                                predator == "Medium-sized benthic fish" ~ "Medium benthic",
                                predator == "Medium-sized demersal fish" ~ "Medium demersal",
                                predator == "Medium-sized pelagic fish" ~ "Medium pelagic",
                                predator == "Large-sized pelagic fish" ~ "Large pelagic",
                                predator == "Large-sized demersal fish" ~ "Large demersal",
                                predator == "Small-sized pelagic fish" ~ "Small pelagic",
                                predator == "Small-sized demersal fish" ~ "Small demersal",
                                predator == "Small-sized benthic fish" ~ "Small benthic",
                                TRUE ~ predator))
  
  
  crust.absolute.change.comparison <- ggplot(pred_pressure_crustacean) +
    geom_col(aes(y = predator, x = ingestion, fill = simu), position = "dodge") +
    theme(axis.text=element_text(size=12),
          axis.title.x=element_text(size = 14),
          plot.title=element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.title = element_blank())+
    labs(title = "Predation pressure on crustaceans", y ="", x = "Ingestion (tons)") +
    xlim(0,12000)
  
  
  # -----------------------------------------------------------------------------------------
  # 3 plots - ingestion change --------------------------------------------------------------
  # figure A6.2
  
  # Join the 3 plots
  smallpel.rel.change_mod = smallpel.rel.change + theme(
    legend.position = "none") +
    #theme(plot.margin = margin(0,0.5,0,0.5, "cm")) +
    labs(title = "Small pelagic fish (+ 3.45 %)") +
    theme(plot.margin = margin(0.5,0.5,0.5,0, "cm")) # 2 -right, 4 - left
  
  ceph.rel.change_mod = ceph.rel.change + theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
    labs(title = "Cephalopods (+ 3.32 %)") +
    
    #theme(plot.margin = margin(0,0.5,0,0.5, "cm"))
    theme(plot.margin = margin(0.5,0.5,0.5,0, "cm")) # 2 -right, 4 - left
  
  crust.rel.change_mod = crust.rel.change + theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()) +
    labs(title = "Crustaceans (+ 1.88 %)") +
    #theme(plot.margin = margin(0,0.5,0,0.5, "cm"))
    theme(plot.margin = margin(0.5,1,0.5,0, "cm")) # 2 -right, 4 - left
  
  
  
  
  pred_pressure_LTL <- plot_grid(smallpel.rel.change_mod, ceph.rel.change_mod, crust.rel.change_mod,
                                 ncol=3,
                                 nrow=1,
                                 rel_widths = c(1.1,0.8,0.8))
  
  # extract the legend from one of the plots
  
  legend <- get_legend(
    crust.rel.change + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom")
  )
  
  # add the legend underneath the row we made earlier. Give it 10%
  # of the height of one plot (via rel_heights).
  plot_grid(pred_pressure_LTL, legend, ncol = 1, rel_heights = c(1, .1))
  
  if (save == TRUE){
    ggsave(here(paste0("figures/appendix/FigA6.2_predation_pressure_S5_10pct_GSAprop.png")), dpi = 300, height = 4, width = 9)
    
  }
  
  
}

