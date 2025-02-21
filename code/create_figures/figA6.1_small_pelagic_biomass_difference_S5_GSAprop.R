#' Figure appendix A6.1
#'
#' 
#' @param biomass output osmose
#' 
#' @return change in small pelagics biomass before and after MPA implementation
#'
#' @export
#' 

plot.fig.A6.1 <- function(outputDir.nompa, outputDir.mpa.10, outputDir.mpa.30, vertical_distrib, save = TRUE){
  
  
  # Biomass species - NoMPA
  data_biomass_noMPA = read_osmose(outputDir.nompa)
  biomass_noMPA = get_var(data_biomass_noMPA, "biomass")
  biomass_noMPA_mean =  apply(biomass_noMPA, c(2), mean, na.rm =TRUE) # mean over time and reps for species 1
  biomass_noMPA_Med_df = as.data.frame(biomass_noMPA_mean) %>%
    rename("biomass" = "biomass_noMPA_mean") %>%
    rownames_to_column("species_name") %>%
    mutate(scenario = "No MPA")
  
  # Biomass species - 10% Mazor - entire Med
  data_biomass_MPA_10_Med = read_osmose(outputDir.mpa.10)
  biomass_MPA10_Med = get_var(data_biomass_MPA_10_Med, "biomass")
  biomass_MPA10_Med_mean =  apply(biomass_MPA10_Med, c(2), mean, na.rm =TRUE) # mean over time and reps for species 1
  biomass_MPA10_Med_df = as.data.frame(biomass_MPA10_Med_mean) %>%
    rename("biomass" = "biomass_MPA10_Med_mean") %>%
    rownames_to_column("species_name") %>%
    mutate(scenario = "10% MPA")
  
  
  # Biomass species - 30% Mazor - entire Med
  data_biomass_MPA_30_Med = read_osmose(outputDir.mpa.30)
  biomass_MPA30_Med = get_var(data_biomass_MPA_30_Med, "biomass")
  biomass_MPA30_Med_mean =  apply(biomass_MPA30_Med, c(2), mean, na.rm =TRUE) # mean over time and reps for species 1
  biomass_MPA30_Med_df = as.data.frame(biomass_MPA30_Med_mean) %>%
    rename("biomass" = "biomass_MPA30_Med_mean") %>%
    rownames_to_column("species_name") %>%
    mutate(scenario = "30% MPA")
  
  # Filter small pelagics
  small_pelagics_group = vertical_distrib %>%
    filter(group == "Small-sized pelagic fish")
  small_pelagics_name = small_pelagics_group$species_name
  
  biomass_small_pel = rbind(biomass_noMPA_Med_df, biomass_MPA10_Med_df, biomass_MPA30_Med_df) %>%
    filter(species_name %in% small_pelagics_name) 
  
  # plot
  biomass_small_pel_wide = biomass_small_pel %>%
    pivot_wider(names_from = scenario, values_from = biomass) %>%
    rename("MPA_10" = "10% MPA",
           "MPA_30" = "30% MPA",
           "No_MPA" = "No MPA") %>%
    mutate(rel_change_10 = (MPA_10 - No_MPA)/No_MPA * 100,
           rel_change_30 = (MPA_30 - No_MPA)/No_MPA * 100,
           diff_10 = (MPA_10 - No_MPA),
           diff_30 = (MPA_30 - No_MPA)) %>%
    pivot_longer(7:8, names_to = "scenario", values_to = "biomass_diff") %>%
    mutate(species_name = case_when(species_name == "Atherinaboyeri" ~ "Atherina boyeri",
                                    species_name == "Engraulisencrasicolus" ~ "Engraulis encrasicolus",
                                    species_name == "Sardinapilchardus" ~ "Sardina pilchardus",
                                    species_name == "Sardinellaaurita" ~ "Sardinella aurita",
                                    species_name == "Spicarasmaris" ~ "Spicara smaris",
                                    TRUE ~ "Sprattus sprattus"))

  # plot
  labels_scenarios =  c("Between no MPAs and 10% coverage", "Between no MPAs and 30% coverage")
  legend_scenarios = c("diff_10", "diff_30")

  plot.fig.A6.1 <- ggplot(biomass_small_pel_wide) +
    geom_col(aes(y = reorder(species_name, biomass_diff), x = biomass_diff, fill = scenario), position = "dodge") +
    theme_bw() +
    
    theme(axis.text=element_text(size=10),
          legend.position = 'bottom',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.direction = "vertical") +
    labs(y = "", x = "Biomass difference (tons)", title = "Entire Mediterranean Sea") +
    scale_fill_brewer("Blues", breaks = legend_scenarios, labels = labels_scenarios) 
  
  if (save == TRUE){
    ggsave(here("figures/appendix/figA6.1_small_pelagics_biomass_difference.png"), height = 5, width = 5, dpi = 300)  
    
  }
  
}
