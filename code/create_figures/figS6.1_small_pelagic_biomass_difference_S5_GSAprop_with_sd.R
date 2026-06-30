#' Figure appendix A6.1
#'
#' 
#' @param biomass output osmose
#' 
#' @return change in small pelagics biomass before and after MPA implementation
#'
#' @export
#' 

plot.fig.S6.1 <- function(outputDir.nompa, outputDir.mpa.10, outputDir.mpa.30, vertical_distrib, save = TRUE){
  
  # Biomass species - NoMPA
  data_biomass_noMPA = read_osmose(outputDir.nompa)
  biomass_noMPA = get_var(data_biomass_noMPA, "biomass")
  biomass_noMPA_Med_df = apply(biomass_noMPA, c(2, 3), mean, na.rm = TRUE) %>%
    as.data.frame() %>%
    rownames_to_column("species_name") %>%
    pivot_longer(-species_name, names_to = "rep", values_to = "biomass") %>%
    mutate(scenario = "No MPA")
  
  # Biomass species - 10% Mazor - entire Med
  data_biomass_MPA_10_Med = read_osmose(outputDir.mpa.10)
  biomass_MPA10_Med = get_var(data_biomass_MPA_10_Med, "biomass")
  biomass_MPA10_Med_df = apply(biomass_MPA10_Med, c(2, 3), mean, na.rm = TRUE) %>%
    as.data.frame() %>%
    rownames_to_column("species_name") %>%
    pivot_longer(-species_name, names_to = "rep", values_to = "biomass") %>%
    mutate(scenario = "10% MPA")
  
  # Biomass species - 30% Mazor - entire Med
  data_biomass_MPA_30_Med = read_osmose(outputDir.mpa.30)
  biomass_MPA30_Med = get_var(data_biomass_MPA_30_Med, "biomass")
  biomass_MPA30_Med_df = apply(biomass_MPA30_Med, c(2, 3), mean, na.rm = TRUE) %>%
    as.data.frame() %>%
    rownames_to_column("species_name") %>%
    pivot_longer(-species_name, names_to = "rep", values_to = "biomass") %>%
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
    # Calculate per replicate
    mutate(rel_change_10 = (MPA_10 - No_MPA)/No_MPA * 100,
           rel_change_30 = (MPA_30 - No_MPA)/No_MPA * 100,
           diff_10 = (MPA_10 - No_MPA),
           diff_30 = (MPA_30 - No_MPA)) %>%
    # Summarize across reps
    group_by(species_name) %>%
    summarize(diff_10_mean = mean(diff_10, na.rm = TRUE),
              diff_10_sd   = sd(diff_10,   na.rm = TRUE),
              diff_30_mean = mean(diff_30, na.rm = TRUE),
              diff_30_sd   = sd(diff_30,   na.rm = TRUE)) %>%
    pivot_longer(cols = -species_name,
                 names_to = c("scenario", ".value"),
                 names_pattern = "(diff_10|diff_30)_(mean|sd)") %>%
    mutate(species_name = case_when(species_name == "Atherinaboyeri" ~ "Atherina boyeri",
                                    species_name == "Engraulisencrasicolus" ~ "Engraulis encrasicolus",
                                    species_name == "Sardinapilchardus" ~ "Sardina pilchardus",
                                    species_name == "Sardinellaaurita" ~ "Sardinella aurita",
                                    species_name == "Spicarasmaris" ~ "Spicara smaris",
                                    TRUE ~ "Sprattus sprattus"))
  
  # rel_change_mean and sd
  biomass_small_pel_rel_change_mean_sd = biomass_small_pel %>%
    pivot_wider(names_from = scenario, values_from = biomass) %>%
    rename("MPA_10" = "10% MPA",
           "MPA_30" = "30% MPA",
           "No_MPA" = "No MPA") %>%
    # Calculate per replicate
    mutate(rel_change_10 = (MPA_10 - No_MPA)/No_MPA * 100,
           rel_change_30 = (MPA_30 - No_MPA)/No_MPA * 100,
           diff_10 = (MPA_10 - No_MPA),
           diff_30 = (MPA_30 - No_MPA)) %>%
    # Summarize across reps
    group_by(species_name) %>%
    summarize(rel_change_10_mean = mean(rel_change_10, na.rm = TRUE),
              rel_change_10_sd   = sd(rel_change_10,   na.rm = TRUE),
              rel_change_30_mean = mean(rel_change_30, na.rm = TRUE),
              rel_change_30_sd   = sd(rel_change_30,   na.rm = TRUE)) %>%
    pivot_longer(cols = -species_name,
                 names_to = c("scenario", ".value"),
                 names_pattern = "(rel_change_10|rel_change_30)_(mean|sd)") %>%
    mutate(species_name = case_when(species_name == "Atherinaboyeri" ~ "Atherina boyeri",
                                    species_name == "Engraulisencrasicolus" ~ "Engraulis encrasicolus",
                                    species_name == "Sardinapilchardus" ~ "Sardina pilchardus",
                                    species_name == "Sardinellaaurita" ~ "Sardinella aurita",
                                    species_name == "Spicarasmaris" ~ "Spicara smaris",
                                    TRUE ~ "Sprattus sprattus"))
  
  # plot
  labels_scenarios =  c("Between no MPAs and 10% coverage", "Between no MPAs and 30% coverage")
  legend_scenarios = c("diff_10", "diff_30")
  
  plot.fig <- ggplot(biomass_small_pel_wide) +
    geom_col(aes(y = reorder(species_name, mean), x = mean, fill = scenario), position = "dodge") +
    geom_errorbar(aes(y = reorder(species_name, mean),
                      xmin = mean - sd, xmax = mean + sd,
                      group = scenario),
                  position = position_dodge(0.9), width = 0.3) +
    theme_bw() +
    theme(axis.text=element_text(size=10),
          legend.position = 'bottom',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.direction = "vertical") +
    labs(y = "", x = "Biomass difference (tons)", title = "Entire Mediterranean Sea") +
    scale_fill_brewer("Blues", breaks = legend_scenarios, labels = labels_scenarios)
  
  if (save == TRUE){
    ggsave(here("figures/appendix/figS6.1_small_pelagics_biomass_difference_with_sd.png"), height = 5, width = 5, dpi = 300)  
    
  }
  
}
