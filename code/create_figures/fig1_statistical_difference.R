#' Function to assess difference in catch loss between scenarios S1 and random scenarios S2, S3
#'
#' 
#' @param data OSMOSE-MED simulation outputs of total catch for each fishing redistribution strategy
#' 
#' @return Wilcox test result
#'
#' @export
#' 



Statistical.difference.scenarios <- function(Catch_NoCC_GSA_prop, Catch_NoCC_GSA_uniform, Catch_NoCC_fline, coverage_level = 10){
  
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
  
  Catch_lit_scenarios_10 = Catch_lit_scenarios_noCC %>%
    filter(mpa.coverage == coverage_level)
  
  Catch_random_scenarios_10 = Catch_random_scenarios_noCC %>%
    filter(mpa.coverage == coverage_level)
  
  Catch_EEZ_scenarios_10 = Catch_EEZ_scenarios_noCC %>%
    filter(mpa.coverage == coverage_level)
  
  Random_catch_10 = rbind(Catch_EEZ_scenarios_10, Catch_random_scenarios_10) 
  Existing_network_catch_10 = Catch_lit_scenarios_10 %>%
    filter(scenario == "Existing network")
  
  random_vect = Random_catch_10$mean
  lit_vect = Existing_network_catch_10$rel_catch_change
  
  shapiro.test(random_vect)
  shapiro.test(lit_vect)
  
  test <- wilcox.test(random_vect, lit_vect)
  
  print(test)
  
}
