#' Function for RDA triplot 
#'
#' 
#' @param data Spatial metrics calculated using the Rpackage lanscapemetrics & mean distance of MPA network to the coast
#'             Indicators calculated from OSMOSE (biomass, catch, LFI, Hill-Shannon alpha-diversity indicator)
#' 
#' @return Figure 5 (main text) &  Figure A8.1 and A8.2 (Appendix 8)
#'
#' @export
#' 

# Create community matrix Y

calculate.relative.biomass.change.scenarios <- function(Biom_NoCC_GSA_prop, Biom_NoCC_GSA_uniform, Biom_NoCC_fline, coverage_level = 10){
  

  Biom_scenarios = rbind(Biom_NoCC_GSA_prop, Biom_NoCC_GSA_uniform, Biom_NoCC_fline) %>%
    dplyr::select(tot.biomass, mpa.coverage, scenario_rep, scenario, Fishing_scenario) %>%
    rename(mpa_coverage = mpa.coverage) %>%
    group_by(mpa_coverage, scenario, Fishing_scenario) %>%
    summarise(mean_biom_mpa = mean(tot.biomass))
  
  Biomass_noMPA = Biom_scenarios$mean_biom_mpa[1]
  
  Relative_biomass = Biom_scenarios %>%
    mutate(mean_biom_nompa = Biomass_noMPA,
           diff_biom = (mean_biom_mpa - mean_biom_nompa),
           rel_biom_change = (mean_biom_mpa - mean_biom_nompa)/mean_biom_nompa * 100) %>%
    filter(mpa_coverage == coverage_level)
  
  return(Relative_biomass)
}


calculate.relative.catch.change.scenarios <- function(Catch_NoCC_GSA_prop, Catch_NoCC_GSA_uniform, Catch_NoCC_fline, coverage_level = 10){
  
  
  Catch_scenarios = rbind(Catch_NoCC_GSA_prop, Catch_NoCC_GSA_uniform, Catch_NoCC_fline) %>%
    dplyr::select(tot.catches, mpa.coverage, scenario_rep, scenario, Fishing_scenario) %>%
    rename(mpa_coverage = mpa.coverage) %>%
    group_by(mpa_coverage, scenario, Fishing_scenario) %>%
    summarise(mean_catch_mpa = mean(tot.catches))
  
  Catches_noMPA = Catch_scenarios$mean_catch_mpa[1]
  
  Relative_catches = Catch_scenarios %>%
    mutate(mean_catch_nompa = Catches_noMPA,
           diff_catch = (mean_catch_mpa - mean_catch_nompa),
           rel_catch_change = (mean_catch_mpa - mean_catch_nompa)/mean_catch_nompa * 100) %>%
    filter(mpa_coverage == coverage_level)
  
  return(Relative_catches)
}

calculate.LFI.scenarios <- function(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline, coverage_level = 10, inside_outside){
  
  # inside_outside = "Inside MPAs" or "Outside MPAs" or "Entire Mediterranean Sea"
  LFI_GSA_prop = LFI_GSA_prop %>%
    filter(mpa == inside_outside) %>%
    filter(mpa_coverage == coverage_level)  
  
  LFI_GSA_uniform = LFI_GSA_uniform %>%
    filter(mpa == inside_outside) %>%
    filter(mpa_coverage == coverage_level)  
  
  LFI_Fline = LFI_Fline %>%
    filter(mpa == inside_outside) %>%
    filter(mpa_coverage == coverage_level)
  
  LFI = rbind(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline) %>%
    group_by(mpa_coverage, scenario, Fishing_scenario) %>%
    summarise(mean_LFI_reps = mean(mean_LFI_60_years))
  
  if (inside_outside == "Outside MPAs"){
    LFI = LFI %>%
      rename(mean_LFI_reps_outside = mean_LFI_reps)
  }
  return(LFI)
}


calculate.diversity.scenarios <- function(diversity_output_lit, diversity_output_random_basin, diversity_output_random_EEZ, coverage_level = 10, inside_outside){
  
  # inside_outside = "Inside MPAs" or "Outside MPAs"
  Diversity = rbind(diversity_output_lit, diversity_output_random_basin, diversity_output_random_EEZ) %>%
    filter(mpa_coverage == coverage_level) %>%
    rename(scenario = mpa_scenario) 
  
  Diversity$scenario <- gsub("-rep", " ", Diversity$scenario)
  
  if (inside_outside == "Outside MPAs"){
    Diversity = Diversity %>%
      rename(mean_exp_H_outside = mean_exp_H_inside)
  }
  
  return(Diversity)
}

create.response.var.matrix <- function(coverage_level = 10){
  
  biomass = calculate.relative.biomass.change.scenarios(Biom_NoCC_GSA_prop, Biom_NoCC_GSA_uniform, Biom_NoCC_fline, coverage_level)
  
  catch = calculate.relative.catch.change.scenarios(Catch_NoCC_GSA_prop, Catch_NoCC_GSA_uniform, Catch_NoCC_fline, coverage_level)
  
  LFI_inside = calculate.LFI.scenarios(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline, coverage_level, inside_outside = "Inside MPAs")
  
  LFI_outside = calculate.LFI.scenarios(LFI_GSA_prop, LFI_GSA_uniform, LFI_Fline, coverage_level, inside_outside = "Outside MPAs")
  
  diversity_inside = calculate.diversity.scenarios(diversity_output_lit = Mean_diversity_inside_GSA_prop_lit, 
                                                   diversity_output_random_basin = Mean_diversity_inside_GSA_prop_random_basin, 
                                                   diversity_output_random_EEZ = Mean_diversity_inside_GSA_prop_random_EEZ, 
                                                   coverage_level, inside_outside = "Inside MPAs")
  
  diversity_outside = calculate.diversity.scenarios(diversity_output_lit = Mean_diversity_outside_GSA_prop_lit, 
                                                    diversity_output_random_basin = Mean_diversity_outside_GSA_prop_random_basin, 
                                                    diversity_output_random_EEZ = Mean_diversity_outside_GSA_prop_random_EEZ, 
                                                    coverage_level, inside_outside = "Outside MPAs")
  
  Response_var_matrix =  biomass %>%
    left_join(catch, by = c("scenario", "Fishing_scenario", "mpa_coverage")) %>%
    
    left_join(LFI_inside, by = c("scenario", "Fishing_scenario", "mpa_coverage")) %>%
    left_join(LFI_outside, by = c("scenario", "Fishing_scenario", "mpa_coverage")) %>%
    
    left_join(diversity_inside, c("scenario", "mpa_coverage")) %>%
    left_join(diversity_outside, c("scenario", "mpa_coverage")) %>%
    
    filter(Fishing_scenario == "Proportional by GSA") %>%
    dplyr::select("scenario", "rel_biom_change", "rel_catch_change", "mean_LFI_reps", "mean_LFI_reps_outside", "mean_exp_H_inside", "mean_exp_H_outside") %>%
    rename(LFI_inside = mean_LFI_reps, LFI_outside = mean_LFI_reps_outside, Diversity_inside = mean_exp_H_inside, Diversity_outside = mean_exp_H_outside) %>%
  
    ungroup() %>%
    column_to_rownames(var="scenario") %>%
    dplyr::select(-mpa_coverage) %>%
    mutate(rel_catch_change = -(rel_catch_change))
  
  # Scale and center variables
  Response_var_matrix <- decostand(Response_var_matrix, method = "standardize")
  
  return(Response_var_matrix)
}



create.explanatory.var.matrix <- function(Metrics_mpa_scenarios, Distance_to_coast){
  
  All_explanatory_var = Metrics_mpa_scenarios %>%
    dplyr::select(metric, value, scenario) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    left_join(Distance_to_coast, by="scenario") %>%
    dplyr::select(-iji) %>%
    rename("distance_to_coast" = "mean_distance_to_coast") %>%
    column_to_rownames(var="scenario")
  
  # Standardize metrics
  All_explanatory_var_standardized = decostand(All_explanatory_var, method = "standardize")
  
  Explanatory_var_matrix = All_explanatory_var_standardized %>%
    
    # selection of variables for RDA
    dplyr::select(np, pladj, shape_mn, distance_to_coast)
  
  return(Explanatory_var_matrix)
  
}

plot.heatmap.X.vars <- function(){
  
  X_matrix = create.explanatory.var.matrix(Metrics_mpa_scenarios, Distance_to_coast)
  # PLOT heatmap ----------------------------------------------------
  png(here("figures/appendix/figA8.1_heatmap.png"), height = 15, width = 15, units = 'cm', res = 1000)
  
  heatmap(abs(cor(X_matrix)), 
          # Compute pearson correlation (note they are absolute values)
          col = rev(heat.colors(10)), 
          Colv = NA, Rowv = NA,
          margins = c(14, 14))
  dev.off()
  
  
  png(here("figures/appendix/figA8.1_heatmap_legend.png"), height = 15, width = 15, units = 'cm', res = 1000)
  plot.new()  # Create a new plot for the legend
  
  legend("center", 
         title = "Absolute Pearson R",
         legend = round(seq(0, 1, length.out = 10), 1),
         y.intersp = 1, bty = "n",
         fill = rev(heat.colors(10)))
  dev.off()
  
  
}

compute.RDA <- function(){
  
  X_matrix = create.explanatory.var.matrix(Metrics_mpa_scenarios, Distance_to_coast)
  Y_matrix = create.response.var.matrix(coverage_level = 10)
  
  # make sure community and env matrix rownames match
  Y_matrix <- Y_matrix[match(rownames(X_matrix), rownames(Y_matrix)), ]
  
  spe.rda <- rda(Y_matrix ~ ., data = X_matrix)
  RsquareAdj = RsquareAdj(spe.rda) # R2.adj = 0.8135843
  
  return(spe.rda)
}

scaling_type = 1
plot.RDA <- function(scaling_type){
  
  # scaling_type = 1 or 2
  spe.rda = compute.RDA()
  
  X_matrix = create.explanatory.var.matrix(Metrics_mpa_scenarios, Distance_to_coast)
  Y_matrix = create.response.var.matrix(coverage_level = 10)
  
  # Scaling 1 ----
  png(here(paste0("figures/RDA_scaling_", scaling_type, ".png")), height = 12, width = 12, units = 'cm', res = 1000)

  X_matrix$scenario = as.factor(c("S5", "S6", "S4", "S1", rep("S2", 10), rep("S3", 10)))
  
  perc <- round(100*(summary(spe.rda)$cont$importance[2, 1:2]), 2)
  
  ## extract scores - these are coordinates in the RDA space
  sc_si <- scores(spe.rda, display="sites", choices=c(1,2), scaling=scaling_type)
  sc_sp <- scores(spe.rda, display="species", choices=c(1,2), scaling=scaling_type)
  sc_bp <- scores(spe.rda, display="bp", choices=c(1, 2), scaling=scaling_type)
  
  ## Custom triplot, step by step
  
  # Set up a blank plot with scaling, axes, and labels
  plot(spe.rda,
       scaling = scaling_type, # set scaling type 
       type = "n", # this excludes the plotting of any points from the results
       frame = FALSE,
       # set axis limits
       # label the plot (title, and axes)
       #main = "Triplot RDA - scaling 2",
       xlab = paste0("RDA1 (", perc[1], "%)"), 
       ylab = paste0("RDA2 (", perc[2], "%)") 
  )
  
  
  # add arrows for species scores
  arrows(0,0, # start them from (0,0)
         sc_sp[,1], sc_sp[,2], # end them at the score value
         angle = 20,
         length = 0.1,
         col = "darkblue",
         lwd = 2)
  
  
  # add text labels for species abbreviations
  #with(scrs, text(species + c(0.25, 0), labels = rownames(species),
  #                col = "darkgreen", font = 1, cex = 0.6))
  # with(All_explanatory_var, legend("topright", legend = levels(scenario), bty = "n",
  #                                  col = colvec, pch = 21, cex = 1, pt.bg = colvec))
  # add arrows for effects of the expanatory variables
  arrows(0,0, # start them from (0,0)
         sc_bp[,1], sc_bp[,2], # end them at the score value
         angle = 20,
         length = 0.1,
         col = "red",
         lwd = 2)
  
  # add points for site scores
  colvec <- c("gold", "gray60", "gray5", "darkorange", "darkslateblue", "darkorchid")
  with(X_matrix, points(spe.rda, display = "sites", col= colvec[scenario],
                                   scaling = scaling_type, pch = 21, cex = 0.7, bg = colvec[scenario]))
  
  
  dev.off()
}

