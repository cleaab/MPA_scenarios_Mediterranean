#' Function for making spatial maps
#'
#' 
#' @param data Spatial outputs from osmose-med
#' 
#' @return Figure A5 
#'
#' @export
#' 


plot.fig.A5 <- function(yield_save = TRUE, biomass_save = TRUE, size_save = FALSE, mpa_coverage = 10, mpa.mask.dir, i = 1){
  
  # Without MPAs
  output.Dir.ref.noCC = here("data/spatial/")
  
  # Yield
  ncname     <- "/Yield_nompa_scenario"
  ncfname    <- paste(output.Dir.ref.noCC, ncname, ".nc", sep="")
  nc <- nc_open(ncfname)
  yield_ref <- ncvar_get(nc, varid = "Catches")
  
  # Biomass
  ncname     <- "/Biomass_nompa_scenario"
  ncfname    <- paste(output.Dir.ref.noCC, ncname, ".nc", sep="")
  nc <- nc_open(ncfname)
  biomass_ref <- ncvar_get(nc, varid = "Biomass")
  
  # Size
  ncname     <- "/Size_nompa_scenario"
  ncfname    <- paste(output.Dir.ref.noCC, ncname, ".nc", sep="")
  nc <- nc_open(ncfname)
  size_ref <- ncvar_get(nc, varid = "Size")
  
  # Abundance
  ncname     <- "/Abundance_nompa_scenario"
  ncfname    <- paste(output.Dir.ref.noCC, ncname, ".nc", sep="")
  nc <- nc_open(ncfname)
  abundance_ref <- ncvar_get(nc, varid = "Abundance")
  
  
  yield_ref_sum_species = apply(yield_ref, c(1, 2, 4), sum, na.rm = FALSE) # sum species
  yield_ref_sum_species_mean_reps = apply(yield_ref_sum_species, c(1, 2), mean, na.rm = T) # mean over 30 simulations
  
  biom_ref_sum_species = apply(biomass_ref, c(1, 2, 4), sum, na.rm = FALSE) # sum species
  biom_ref_sum_species_mean_reps = apply(biom_ref_sum_species, c(1, 2), mean, na.rm = T) # mean over 30 simulations
  
  size_ref_sum_species = apply(size_ref, c(1, 2, 4), sum, na.rm = FALSE) # sum species
  size_ref_sum_species_mean_reps = apply(size_ref_sum_species, c(1, 2), mean, na.rm = T) # mean over 30 simulations
  
  abundance_ref_sum_species = apply(abundance_ref, c(1, 2, 4), sum, na.rm = FALSE) # sum species
  abundance_ref_sum_species_mean_reps = apply(abundance_ref_sum_species, c(1, 2), mean, na.rm = T) # mean over 30 simulations
  
  
  # With marine protected areas
  
  #MPA_scenarios = length(mpa.mask.dir)
  
  
  for (f in 1:length(Fishing_scenario)){
    
    output.Dir.mpa.noCC = here(paste0("data/spatial/", Fishing_scenario[f], "/Mazor_scenario_8/output_mpa_", mpa_coverage))
    
    # PLOTS -------------------------------------------------------------------------
    Med_grid = st_read(here("data/shapefiles/Med_polygon.gpkg"))
    MPA = read_csv(paste0(lit.mpa.mask.dir[i], "/", mpa_coverage, ".csv"), col_names = FALSE)
    MPA.cells = unlist(as.list(t(MPA))) # unlisted not on the right way
    Med_grid$MPA = MPA.cells # 0 - unprotected, 1 - no-take area
    MPA_polygons = Med_grid %>%
      filter(MPA == 1)
    MPA_polygons_union = st_union(MPA_polygons)
    
    
    if (yield_save == TRUE){
      
      # Yield 
      ncname     <- "/Yield_mpa_scenario"
      ncfname    <- paste(output.Dir.mpa.noCC, ncname, ".nc", sep="")
      nc <- nc_open(ncfname)
      yield_mpa <- ncvar_get(nc, varid = "Catches")
      
      yield_mpa_sum_species = apply(yield_mpa, c(1, 2, 4), sum, na.rm = FALSE) # sum species
      yield_mpa_sum_species_mean_reps = apply(yield_mpa_sum_species, c(1, 2), mean, na.rm = T) # mean over 30 simulations
      
      # Yield
      yield_diff = 
        yield_mpa_sum_species_mean_reps - yield_ref_sum_species_mean_reps
      
      # Yield difference ------------------------------------------------------------
      max = max(yield_diff, na.rm = T)
      min = min(yield_diff, na.rm = T)
      median(yield_diff, na.rm = T)
      mean(yield_diff, na.rm = T)
      q = quantile(yield_diff, probs = seq(.1, .9, by = .1), na.rm = T)
      brk  = c(min, q, max)
      brk  = c(q)
      max
      min
      brk = c(min, -100, -30, -20, -10, 0, 10, 20, 30, 100, max)
      #png(paste0(output.Dir.mpa.noCC, "/Catch_difference_", fishing_scenario[f], "_mpa", mpa_coverage, ".png"), height = 480, width = 994, dpi = 1000)
      dir.create(here("figures/appendix/spatial"))
      png(here(paste0("figures/appendix/spatial/Catch_difference_", Fishing_scenario[f], "_mpa", mpa_coverage, ".png")), height = 4000, width = 8000, units = 'px')
      # setEPS()
      # postscript(paste0(output.Dir.mpa.noCC, "/Catch_M8.eps"), width = 8, height = 4, res = 1000)
      # pdf(paste0(output.Dir.mpa.noCC, "/Catch_M8.pdf"), width = 8, height = 4)
      par(mar = c(2, 2, 2, 2)) # Set the margin on all sides to 2
      # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), yield_diff, main = paste0("Catch difference | ",mpa_scenarios_name[i], " | mpa : ", mpa_coverage, "% | Fishing scenario: ", fishing_scenario[f]), cex.main = 1.5, legend.cex = 1.2, breaks = brk, col = rev(RdYlBu(length(brk)-1)), xaxt = "n", yaxt = "n", xlab = " ", ylab = " ", legend.lab = "Catch difference (tons)", legend.line = 4,legend.mar = c(8.5, 0, 0, 0), legend.width = 1.8, legend.only = FALSE)
      image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), yield_diff, legend.cex = 1.2, breaks = brk, col = rev(RdYlBu(length(brk)-1)), xaxt = "n", yaxt = "n", xlab = " ", ylab = " ")
      plot(MPA_polygons_union, add = T, col = "black", border = "black", lwd =10)
      plot(worldbounds_3035$geometry,add=T, col= "#F0F0F0", lwd = 10)
      box()
      #image.save(paste0(output.Dir.mpa.noCC, "/Catch_M8.eps"), dpi = c(1000,1000))
      dev.off()
      
      # for legend:
      #setEPS()
      #postscript(paste0("figures/appendix/spatial/Catch_difference_legend.eps"))
      png(here(paste0("figures/appendix/spatial/Catch_difference_legend.png")), height = 480, width = 994)
      brk = c(-170, -100, -30, -20, -10, 0, 10, 20, 30, 100, 170)
      image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), yield_diff, main = paste0("Catch difference | ", MPA_scenarios_name[i], " | mpa : ", mpa_coverage, "% | Fishing scenario: ", Fishing_scenario[f]), cex.main = 1.5, legend.cex = 3, axis.args=list(cex.axis=2), breaks = brk, col = rev(RdYlBu(length(brk)-1)), xaxt = "n", yaxt = "n", xlab = " ", ylab = " ", legend.lab = "Catch difference (tons)", legend.line = 6,legend.mar = c(8.5, 0, 0, 0), legend.width = 1.8, horizontal = T, legend.only = T)
      dev.off()
      
    }
    
    if (biomass_save == TRUE){
      
      # Biomass
      ncname     <- "/Biomass_mpa_scenario"
      ncfname    <- paste(output.Dir.mpa.noCC, ncname, ".nc", sep="")
      nc <- nc_open(ncfname)
      biomass_mpa <- ncvar_get(nc, varid = "Biomass")
      
      
      biom_mpa_sum_species = apply(biomass_mpa, c(1, 2, 4), sum, na.rm = FALSE) # sum species
      biom_mpa_sum_species_mean_reps = apply(biom_mpa_sum_species, c(1, 2), mean, na.rm = T) # mean over 30 simulations
      
      # Biomass
      biom_diff = 
        biom_mpa_sum_species_mean_reps - biom_ref_sum_species_mean_reps
      
      
      # Biomass difference -----------------------------------------------------------
      max = max(biom_diff, na.rm = T)
      min = min(biom_diff, na.rm = T)
      median(biom_diff, na.rm = T)
      mean(biom_diff, na.rm = T)
      q = quantile(biom_diff, probs = seq(.1, .9, by = .1), na.rm = T)
      brk  = c(min, q, max)
      brk  = c(q)
      max
      min
      brk = c(min, -100, -30, -20, -10, 0, 10, 20, 30, 100, max)
      
      
      png(here(paste0("figures/appendix/spatial/Biomass_difference_", Fishing_scenario[f], "_mpa", mpa_coverage, ".png")), height = 4000, width = 8000, units = 'px')
      image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), biom_diff, legend.cex = 1.2, breaks = brk, col = rev(RdYlBu(length(brk)-1)), xaxt = "n", yaxt = "n", xlab = " ", ylab = " ")
      plot(MPA_polygons_union, add = T, border = "black", lwd =20)
      plot(worldbounds_3035$geometry,add=T, col= "#F0F0F0", lwd = 10)
      box()
      #image.save(paste0(output.Dir.mpa.noCC, "/Catch_M8.eps"), dpi = c(1000,1000))
      dev.off()
      
      
      # for legend:
      #png(paste0(output.Dir.mpa.noCC, "/Biomass_difference_legend.png"), height = 4000, width = 8000, units = 'px')
      png(here(paste0("figures/appendix/spatial/Biomass_difference_legend.png")), height = 480, width = 994)
      brk = c(-170, -100, -30, -20, -10, 0, 10, 20, 30, 100, 170)
      image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), biom_diff, main = paste0("Catch difference | ", MPA_scenarios_name[i], " | mpa : ", mpa_coverage, "% | Fishing scenario: ", Fishing_scenario[f]), cex.main = 1.5, legend.cex = 3, axis.args=list(cex.axis=2), breaks = brk, col = rev(RdYlBu(length(brk)-1)), xaxt = "n", yaxt = "n", xlab = " ", ylab = " ", legend.lab = "Biomass difference (tons)", legend.line = 6,legend.mar = c(8.5, 0, 0, 0), legend.width = 2.5, horizontal = T, legend.only = T)
      dev.off()
      
    }
    
    if (size_save == TRUE){
      
      
      # Size
      ncname     <- "/Size_mpa_scenario"
      ncfname    <- paste(output.Dir.mpa.noCC, ncname, ".nc", sep="")
      nc <- nc_open(ncfname)
      size_mpa <- ncvar_get(nc, varid = "Size")
      
      size_mpa_sum_species = apply(size_mpa, c(1, 2, 4), sum, na.rm = FALSE) # sum species
      size_mpa_sum_species_mean_reps = apply(size_mpa_sum_species, c(1, 2), mean, na.rm = T) # mean over 30 simulations
      
      # Size
      size_diff = 
        size_mpa_sum_species_mean_reps - size_ref_sum_species_mean_reps
      
      
      # Size exploited difference ---------------------------------------------------------------
      max = max(size_exploited_diff, na.rm = T)
      min = min(size_exploited_diff, na.rm = T)
      median(size_exploited_diff, na.rm = T)
      mean(size_exploited_diff, na.rm = T)
      q = quantile(size_exploited_diff, probs = seq(.1, .9, by = .1), na.rm = T)
      max
      min
      #brk = c(min, -4, -2, q[1:2],0, 0.5, 1, 1.5, 2, max)
      #brk = c(min, q, max)
      brk  =c(min, -30, -20, -10, -5, 0, 5, 10, 20, 30, max)
      col = rev(RdYlBu(length(brk)-1))
      show_col(col)
      col2 = col[3:8]  
      brk  =c(min, -10, -5, 0, 5, 10, max)
      length(brk)
      length(brk)
      png(here(paste0("figures/appendix/spatial/Size_difference_", Fishing_scenario[f], "_mpa", mpa_coverage, ".png")), height = 4000, width = 8000, units = 'px')
      par(mar = c(2, 2, 2, 2)) # Set the margin on all sides to 2
      image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), size_diff, main = paste0("Size difference | ", MPA_scenarios_name[i], " | mpa : ", mpa_coverage, "% | Fishing scenario: ", Fishing_scenario[f]), cex.main = 1.5, legend.cex = 1.2, breaks = brk, col = col2, xaxt = "n", yaxt = "n", xlab = " ", ylab = " ", legend.lab = "Exploited species size difference (tons)", legend.line = 4,legend.mar = c(8.5, 0, 0, 0), legend.width = 1.8)
      plot(MPA_polygons_union, add = T, border = "black", lwd =1)
      plot(worldbounds_3035$geometry,add=T, col= "#F0F0F0")
      box()
      dev.off()
    }
    
  }
  
}





# in Plots-article-GSAprop - scripts for calculating maps per group and differentiating exploited and non-exploited species


