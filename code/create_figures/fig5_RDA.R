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

calculate.total.biomass.scenarios <- function(Biom_GSA_prop, coverage_level = 10){
  

  Biom_scenarios = Biom_GSA_prop %>%
    dplyr::select(tot.biomass, mpa.coverage, scenario_rep, scenario) %>%
    rename(mpa_coverage = mpa.coverage) %>%
    group_by(mpa_coverage, scenario) %>%
    summarise(mean_biom_mpa = mean(tot.biomass))
  
  Biomass_noMPA = Biom_scenarios$mean_biom_mpa[1]
  
  Relative_biomass = Biom_scenarios %>%
    mutate(mean_biom_nompa = Biomass_noMPA,
           diff_biom = (mean_biom_mpa - mean_biom_nompa),
           rel_biom_change = (mean_biom_mpa - mean_biom_nompa)/mean_biom_nompa * 100) %>%
    filter(mpa_coverage == coverage_level)
  
  return(Relative_biomass)
}

calculate.total.catch.scenarios <- function(Catch_GSA_prop, coverage_level = 10){
  
  
  Catch_scenarios = Catch_GSA_prop %>%
    dplyr::select(tot.catches, mpa.coverage, scenario_rep, scenario) %>%
    rename(mpa_coverage = mpa.coverage) %>%
    group_by(mpa_coverage, scenario) %>%
    summarise(mean_catch_mpa = mean(tot.catches))
  
  Catches_noMPA = Catch_scenarios$mean_catch_mpa[1]
  
  Relative_catches = Catch_scenarios %>%
    mutate(mean_catch_nompa = Catches_noMPA,
           diff_catch = (mean_catch_mpa - mean_catch_nompa),
           rel_catch_change = (mean_catch_mpa - mean_catch_nompa)/mean_catch_nompa * 100) %>%
    filter(mpa_coverage == coverage_level)
  
  return(Relative_catches)
}

calculate.LFI.scenarios <- function(LFI_GSA_prop, coverage_level = 10){
  
  LFI = LFI_GSA_prop %>%
    filter(mpa == "Entire Mediterranean Sea") %>%
    group_by(mpa_coverage, scenario) %>%
    summarise(LFI = mean(mean_LFI_60_years, na.rm = T)) %>%
    filter(mpa_coverage == coverage_level)  

  return(LFI)
}

calculate.diversity.scenarios <- function(Mean_diversity_Med_GSA_prop_lit, Mean_diversity_Med_GSA_prop_random_basin, Mean_diversity_Med_GSA_prop_random_EEZ, coverage_level = 10){
  
  Diversity = rbind(Mean_diversity_Med_GSA_prop_lit, Mean_diversity_Med_GSA_prop_random_basin, Mean_diversity_Med_GSA_prop_random_EEZ) %>%
    ungroup() %>%
    rename(scenario = mpa_scenario) %>%
    group_by(scenario, mpa_coverage) %>%
    summarise(diversity = mean(mean_exp_H, na.rm = T)) %>%
    filter(mpa_coverage == coverage_level)
    
  
  Diversity$scenario <- gsub("-rep", " ", Diversity$scenario)
  
  return(Diversity)
}

calculate.evenness.scenarios <- function(Mean_diversity_Med_GSA_prop_lit, Mean_diversity_Med_GSA_prop_random_basin, Mean_diversity_Med_GSA_prop_random_EEZ, coverage_level = 10){
  
  Evenness = rbind(Mean_diversity_Med_GSA_prop_lit, Mean_diversity_Med_GSA_prop_random_basin, Mean_diversity_Med_GSA_prop_random_EEZ) %>%
    ungroup() %>%
    rename(scenario = mpa_scenario) %>%
    group_by(scenario, mpa_coverage) %>%
    summarise(evenness = mean(mean_evenness, na.rm = T)) %>%
    filter(mpa_coverage == coverage_level)
  
  
  Evenness$scenario <- gsub("-rep", " ", Evenness$scenario)
  
  return(Evenness)
}

create.response.var.matrix <- function(coverage_level = 10){
  
  biomass = calculate.total.biomass.scenarios(Biom_GSA_prop, coverage_level)
  
  catch = calculate.total.catch.scenarios(Catch_GSA_prop, coverage_level)
  
  LFI = calculate.LFI.scenarios(LFI_GSA_prop, coverage_level)
  
  diversity = calculate.diversity.scenarios(Mean_diversity_Med_GSA_prop_lit, 
                                            Mean_diversity_Med_GSA_prop_random_basin, 
                                            Mean_diversity_Med_GSA_prop_random_EEZ, 
                                            coverage_level)
  evenness = calculate.evenness.scenarios(Mean_diversity_Med_GSA_prop_lit, 
                                          Mean_diversity_Med_GSA_prop_random_basin, 
                                          Mean_diversity_Med_GSA_prop_random_EEZ, 
                                          coverage_level)
  

  Response_var_matrix =  biomass %>%
    left_join(catch, by = c("scenario", "mpa_coverage")) %>%
    
    left_join(LFI, by = c("scenario", "mpa_coverage")) %>%

    left_join(diversity, c("scenario", "mpa_coverage")) %>%
    left_join(evenness, c("scenario", "mpa_coverage")) %>%
    
    #dplyr::select("scenario", "rel_biom_change", "rel_catch_change", "mean_LFI_reps", "mean_LFI_reps_outside", "mean_exp_H_inside", "mean_exp_H_outside") %>%
    ungroup() %>%
    
    dplyr::select("scenario", "mean_biom_mpa", "mean_catch_mpa", "LFI", "diversity", "evenness") %>%
    
    rename(total_biomass = mean_biom_mpa, total_catch = mean_catch_mpa) %>%
  
    column_to_rownames(var="scenario") 
    #mutate(rel_catch_change = -(rel_catch_change))
  
  # Scale and center variables
  Response_var_matrix <- decostand(Response_var_matrix, method = "standardize")
  
  return(Response_var_matrix)
}

create.response.var.matrix.v2 <- function(coverage_level = 10){
  
  biomass = calculate.relative.biomass.change.scenarios(Biom_GSA_prop, coverage_level)
  
  catch = calculate.relative.catch.change.scenarios(Catch_GSA_prop, coverage_level)
  
  LFI = calculate.LFI.scenarios(LFI_GSA_prop, coverage_level)
  
  diversity = calculate.diversity.scenarios(Mean_diversity_Med_GSA_prop_lit, 
                                            Mean_diversity_Med_GSA_prop_random_basin, 
                                            Mean_diversity_Med_GSA_prop_random_EEZ, 
                                            coverage_level)
  evenness = calculate.evenness.scenarios(Mean_diversity_Med_GSA_prop_lit, 
                                          Mean_diversity_Med_GSA_prop_random_basin, 
                                          Mean_diversity_Med_GSA_prop_random_EEZ, 
                                          coverage_level)
  
  
  Response_var_matrix =  biomass %>%
    left_join(catch, by = c("scenario", "mpa_coverage")) %>%
    
    left_join(LFI, by = c("scenario", "mpa_coverage")) %>%
    
    left_join(diversity, c("scenario", "mpa_coverage")) %>%
    left_join(evenness, c("scenario", "mpa_coverage")) %>%
    
    ungroup() %>%
    dplyr::select("scenario", "rel_biom_change", "rel_catch_change", "LFI", "diversity", "evenness") %>%
    
    rename(biomass_gains = rel_biom_change, catch_losses = rel_catch_change) %>%
    
    column_to_rownames(var="scenario") %>%
    mutate(catch_losses = -(catch_losses))
  
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
  RsquareAdj = RsquareAdj(spe.rda) # R2.adj = 0.7350566
  
  return(spe.rda)
}

plot.RDA <- function(scaling_type, rda1_range, rda2_range){
  
  # scaling_type = 1 or 2
  spe.rda = compute.RDA()
  plot(spe.rda)
  X_matrix = create.explanatory.var.matrix(Metrics_mpa_scenarios, Distance_to_coast)
  Y_matrix = create.response.var.matrix(coverage_level = 10)
  
  # Scaling 1 ----
  png(here(paste0("figures/RDA_scaling_", scaling_type, "_ed.png")), height = 12, width = 10, units = 'cm', res = 1000)

  X_matrix$scenario = as.factor(c("S5", "S6", "S4", "S1", rep("S2", 10), rep("S3", 10)))
  
  perc <- round(100*(summary(spe.rda)$cont$importance[2, 1:2]), 2)
  
  ## extract scores - these are coordinates in the RDA space
  sc_si <- scores(spe.rda, display="sites", choices=c(1,2), scaling=scaling_type)
  sc_sp <- scores(spe.rda, display="species", choices=c(1,2), scaling=scaling_type)
  sc_bp <- scores(spe.rda, display="bp", choices=c(1, 2), scaling=scaling_type)
  
  ## Custom triplot, step by step
  
  # Set up a blank plot with scaling, axes, and labels
  par(mar = c(4, 4, 0.5, 0.5))
  
  plot(spe.rda,
       scaling = scaling_type,
       type    = "n", # this excludes the plotting of any points from the results
       frame   = TRUE,
       xlim    = rda1_range,
       ylim    = rda2_range,
       xlab    = paste0("RDA1 (", perc[1], "%)"),
       ylab    = paste0("RDA2 (", perc[2], "%)"),
       cex.lab  = 0.7,   # axis label size
       cex.axis = 0.6    # tick label size
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
                                   scaling = scaling_type, pch = 21, cex = 1, bg = colvec[scenario]))
  # Legend for scenarios
  legend("bottomright",
         legend = levels(X_matrix$scenario),
         pt.bg  = colvec[seq_along(levels(X_matrix$scenario))],
         pch = 21, col = "black", pt.cex = 1,
         bty = "n", cex = 0.65, title = "FPA scenario")
  
  dev.off()
}



# ============================================================
# RDA heatmap: biomass × catch product surface to find best area along RDA axis of best compromise
# ============================================================

compute_heatmap <- function(spe.rda, scaling_type = 2, rda1_range, rda2_range, grid_res = 100) {
  
  # --- 1. Extract scores -----------------------------------------------
  # Use "lc" (linear combination) scores for sites — these are the fitted
  # coordinates in constrained space. "sites" includes residual noise.
  sc_lc <- scores(spe.rda, display = "lc",      choices = c(1,2), scaling = scaling_type)
  sc_sp <- scores(spe.rda, display = "species",  choices = c(1,2), scaling = scaling_type)
  sc_bp <- scores(spe.rda, display = "bp",       choices = c(1,2), scaling = scaling_type)
  
  # --- 3. Species score vectors for biomass and catch ------------------
  # sc_sp rows = your response variable names; adjust if named differently
  b1 <- sc_sp["total_biomass", "RDA1"]
  b2 <- sc_sp["total_biomass", "RDA2"]
  c1 <- sc_sp["total_catch", "RDA1"]
  c2 <- sc_sp["total_catch", "RDA2"]
  
  cat("Biomass equation: B̂ = ", 
     round(b1,6), "* RDA1 +", round(b2,6), "* RDA2\n")
  cat("Catch equation:   Ĉ = ", 
      round(c1,6), "* RDA1 +", round(c2,6), "* RDA2\n")
  
  # --- 4. Define grid spanning the constrained ordination space ---------
  #rda1_range <- range(sc_lc[, "RDA1"])
  #rda2_range <- range(sc_lc[, "RDA2"])
  #rda1_range <- range(-3,2)
  #rda2_range <- range(-3,2)

  
  # Add 20% padding so site points don't sit on the edge
  #pad <- function(r) r + c(-1,1) * diff(r) * 0.20
  rda1_seq <- seq(rda1_range[1], rda1_range[2], length.out = grid_res)
  rda2_seq <- seq(rda2_range[1], rda2_range[2], length.out = grid_res)
  
  grid <- expand.grid(RDA1 = rda1_seq, RDA2 = rda2_seq)
  
  # --- 5. Predict biomass and catch at every grid point ----------------
  grid$biomass <- grid$RDA1 * b1 + grid$RDA2 * b2
  grid$catch   <- grid$RDA1 * c1 + grid$RDA2 * c2
  
  # --- 6. Joint surface: product of biomass and (−catch) ---------------
  # Normalize each to [0,1] first so they contribute equally
  norm01 <- function(x) (x - min(x)) / (max(x) - min(x))
  
  grid$biomass_norm  <-  norm01(grid$biomass)
  grid$catch_norm    <-  norm01(grid$catch)       # high = more catch loss
  
  grid$joint <- grid$biomass_norm * grid$catch_norm
  
  # --- 7. Return everything for plotting --------------------------------
  list(grid = grid, sc_lc = sc_lc, sc_sp = sc_sp, sc_bp = sc_bp,
       b = c(b1, b2), c = c(c1, c2))
}

# Show area along axis 1 and 2 of the RDA that offers the best compromise to maximise biomass gains and minimize catch losses

plot_rda_heatmap <- function(spe.rda, variable = variable,
                             scaling_type = 2, grid_res = 100,
                             save_path = NULL) {
  
  res      <- compute_heatmap(spe.rda, scaling_type,rda1_range, rda2_range, grid_res)
  grid     <- res$grid
  sc_lc    <- res$sc_lc
  sc_sp    <- res$sc_sp
  sc_bp    <- res$sc_bp
  
  # Reshape to matrix for image/filled.contour
  rda1_vals <- sort(unique(grid$RDA1))
  rda2_vals <- sort(unique(grid$RDA2))
  z_mat     <- matrix(grid[[variable]],
                      nrow = length(rda1_vals),
                      ncol = length(rda2_vals))
  
  # Color palettes
  pal <- switch(variable,
                joint   = colorRampPalette(c("#0d1b2a", "#1d4e89", "#f4a261", "#e9c46a"))(100),
                biomass = colorRampPalette(c("#0d1b2a", "#1d4e89", "#f4a261", "#e9c46a"))(100),
                catch = colorRampPalette(c("#0d1b2a", "#1d4e89", "#f4a261", "#e9c46a"))(100)
  )
  
  title_str <- switch(variable,
                      joint   = "Joint optimality: biomass × (1 − catch) [normalized]",
                      total_biomass = "Predicted biomass in RDA space",
                      total_catch   = "Predicted catch in RDA space"
  )
  
  perc <- round(100 * summary(spe.rda)$cont$importance[2, 1:2], 2)
  
  if (!is.null(save_path)) {
    png(save_path, height = 12, width = 12, units = "cm", res = 600)
  }
  
  # Base heatmap
  image(rda1_vals, rda2_vals, z_mat,
        col  = pal,
        xlab = paste0("RDA1 (", perc[1], "%)"),
        ylab = paste0("RDA2 (", perc[2], "%)"),
        main = title_str,
        asp  = 1)
  
  # Zero axes
  abline(h = 0, v = 0, col = "white", lty = 2, lwd = 0.8)
  
  # Site points (lc scores = constrained positions)
  X_matrix <- create.explanatory.var.matrix(Metrics_mpa_scenarios, Distance_to_coast)
  X_matrix$scenario <- as.factor(c("S5","S6","S4","S1", rep("S2",10), rep("S3",10)))
  colvec <- c("gold","gray60","gray20","darkorange","darkslateblue","darkorchid")
  
  points(sc_lc[, "RDA1"], sc_lc[, "RDA2"],
         pch = 21, cex = 1,
         bg  = colvec[X_matrix$scenario],
         col = "white", lwd = 0.6)
  
  # Legend for scenarios
  legend("bottomright",
         legend = levels(X_matrix$scenario),
         pt.bg  = colvec[seq_along(levels(X_matrix$scenario))],
         pch = 21, col = "white", pt.cex = 1,
         bty = "n", cex = 0.65, title = "FPA scenario")
  
  if (!is.null(save_path)) dev.off()
  
  invisible(res)
  
}


