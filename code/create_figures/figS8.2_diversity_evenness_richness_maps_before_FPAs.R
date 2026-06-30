#' Function to create Fig S8.2 -  alpha-taxonomic diversity, species richness and evenness maps before FPAs 
#'
#' 
#' @param data OSMOSE-MED simulation outputs of spatialized abundance
#' 
#' @return Figure S8.2 (appendix)
#'
#' @export

plot.fig.suppmat.S8.2.diversity.richness.evenness.maps <- function(save = TRUE){
  # Abundance map before MPAs
  nc_abundance_spatial  = nc_open(here("data/spatial/Abundance_nompa_scenario.nc"))
  abundance_spatial  = ncvar_get(nc_abundance_spatial, "Abundance")
  nc_close(nc_abundance_spatial)  # ← close immediately after reading
  
  abundance_spatial_tot_sp = apply(abundance_spatial, c(1,2,4), sum, na.rm = T) # nombre tot d'individus
  # species richness
  Hill0_spatial <- apply(abundance_spatial > 0, c(1,2, 4), sum, na.rm = TRUE)
  Hill0_spatial[Hill0_spatial == 0] = NA
  Hill0_spatial_mean_reps = apply(Hill0_spatial, c(1,2), mean, na.rm = T)
  
  # Shannon index: sum over all species of -(p * log(p))
  # Initialize H_sum [190, 83, 30] with zeros
  H_sum = array(0, dim = c(190, 83, 30))
  
  for (i in 1:101){
    # Extract species i: [190, 83, 30], drop=FALSE not needed since dim 3 = 1 will auto-drop
    sp_abundance = abundance_spatial[1:190, 1:83, i, 1:30]  # [190, 83, 30]
    
    p_sp = sp_abundance / abundance_spatial_tot_sp           # [190, 83, 30]
    
    # Suppress log(0): 0*log(0) = 0 by convention in Shannon
    H_sp = ifelse(p_sp > 0, -(p_sp * log(p_sp)), 0)         # [190, 83, 30]
    
    H_sum = H_sum + H_sp
  }
  
  # Set cells with no abundance to NA
  H_sum[abundance_spatial_tot_sp == 0] = NA
  
  exp_H_spatial = exp(H_sum)
  exp_H_spatial_mean_reps = apply(exp_H_spatial, c(1,2), mean, na.rm = T)
  
  #evenness
  evenness = H_sum/log(Hill0_spatial)
  evenness_mean_reps = apply(evenness, c(1,2), mean, na.rm = T)
  
  if (save == TRUE){
    # PLOT Hill-Shannon diversity index (before MPAs) --------------------------
    png(here("figures/appendix/taxonomic_diversity_map_ed.png"), height = 400, width = 800, units = 'px')
    image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), exp_H_spatial_mean_reps,
               legend.cex = 1.2, xaxt = "n", yaxt = "n", xlab = " ", ylab = " ")
    plot(worldbounds_3035$geometry,add=T, col= "#F0F0F0", lwd = 1)
    box()
    dev.off()
    
    # PLOT species richness (before MPAs) --------------------------------------
    png(here("figures/appendix/species_richness_map_ed.png"), height = 400, width = 800, units = 'px')
    image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Hill0_spatial_mean_reps,
               legend.cex = 1.2, xaxt = "n", yaxt = "n", xlab = " ", ylab = " ")
    plot(worldbounds_3035$geometry,add=T, col= "#F0F0F0", lwd = 1)
    box()
    dev.off()
    
    # PLOT eveness (before MPAs) -----------------------------------------------
    png(here("figures/appendix/evenness_map_ed.png"), height = 400, width = 800, units = 'px')
    image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), evenness_mean_reps,
               legend.cex = 1.2, xaxt = "n", yaxt = "n", xlab = " ", ylab = " ")
    plot(worldbounds_3035$geometry,add=T, col= "#F0F0F0", lwd = 1)
    box()
    dev.off()    
   
    
  }
  
}