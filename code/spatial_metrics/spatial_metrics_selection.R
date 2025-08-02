

# Reponse variables



# Spatial metrics
Metrics_mpa_scenarios = read_csv(here("data/spatial_metrics/Metrics_landscapemetrics.csv"))

Distance_to_coast = read_csv(here("data/spatial_metrics/Distance_to_coast_from_centroid.csv"))


All_explanatory_var = Metrics_mpa_scenarios %>%
  dplyr::select(metric, value, scenario) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  left_join(Distance_to_coast, by="scenario") %>%
  dplyr::select(-scenario,-iji) %>%
  rename("distance_to_coast" = "mean_distance_to_coast")

All_explanatory_var$scenario = as.factor(c(scenarios[1:4], rep("Random", 10), rep("EEZ", 10)))

spe.rda <- rda(Response_var ~ ., data = All_explanatory_var)

Explanatory_var = Metrics_mpa_scenarios %>%
  dplyr::select(metric, value, scenario) %>%
  filter(metric %in% c("enn_cv", "cohesion", "division", "area_cv", "ai", "gyrate_cv", "np")) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  left_join(Distance_to_coast, by="scenario") %>%
  dplyr::select(-scenario) %>%
  dplyr::select(- total_distance_to_coast) %>%
  rename("distance_to_coast" = "mean_distance_to_coast")
Explanatory_var$scenario = as.factor(c(scenarios[1:4], rep("Random", 10), rep("EEZ", 10)))
