#' Function to create Fig A7.2 -  alpha-taxonomic divesity and abundance maps before MPAs 
#'
#' 
#' @param data OSMOSE-MED simulation outputs of spatialized abundance
#' 
#' @return Figure A7.2 (appendix)
#'
#' @export

plot.figA7.2 <- function(save = TRUE){
  nc_abundance_spatial  = nc_open(here("data/spatial/Abundance_nompa_scenario.nc"))
  abundance_spatial  = ncvar_get(nc_abundance_spatial, "Abundance")
  
  abundance_spatial_mean = apply(abundance_spatial, c(1,2,3), mean, na.rm = T)
  abundance_spatial_mean_sp = apply(abundance_spatial, c(1,2), sum, na.rm = T) # nombre tot d'individus
  
  for (i in 1:101){
    
    if (i == 1){
      abundance_spatial_sp1 = abundance_spatial_mean[1:190,1:83,i]
      p_sp1 = abundance_spatial_sp1/abundance_spatial_mean_sp
      H_sp1 = -(p_sp1*log(p_sp1))
      
      abundance_spatial_sp2 = abundance_spatial_mean[1:190,1:83,i+1]
      p_sp2 = abundance_spatial_sp2/abundance_spatial_mean_sp
      H_sp2 = -(p_sp2*log(p_sp2))
      
      sum_matrix <- mapply(function(x, y) sum(c(x, y), na.rm = TRUE), H_sp2, H_sp1)
      sum_matrix <- matrix(sum_matrix, nrow = nrow(H_sp2))
    }
    
    else{
      if (i == 2){
        i = 3
      }
      abundance_spatial_sp1 = abundance_spatial_mean[1:190,1:83,i]
      p_sp1 = abundance_spatial_sp1/abundance_spatial_mean_sp
      H_sp1 = -(p_sp1*log(p_sp1))
      sum_matrix <- mapply(function(x, y) sum(c(x, y), na.rm = TRUE), sum_matrix, H_sp1)
      sum_matrix <- matrix(sum_matrix, nrow = nrow(H_sp1))
      
    }
    
  }
  
  sum_matrix[sum_matrix == 0] = NA
  exp_H_spatial = exp(sum_matrix)
  
  if (save == TRUE){
    # PLOT Hill-Shannon diversity index (before MPAs) ----------------------------------------
    # legend only
    png(here("figures/appendix/FigA7.2_Hill_Shannon_nompa_legend.png"), height = 4000, width = 8000, units = 'px')
    image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), exp_H_spatial, main = "Hill-Shannon diversity index before MPAs",cex.main = 1, col =viridis(30), xlab = "", ylab = "", xaxt='n', horizontal = T, yaxt='n', legend.only = T)
    dev.off()
    # plot
    png(here("figures/appendix/FigA7.2_Hill_Shannon_nompa.png"), height = 600, width = 800, units = 'px')
    image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), exp_H_spatial, col =viridis(4), xlab = "", ylab = "", xaxt='n', horizontal = T, yaxt='n', xaxt='n' )
    dev.off()
    
    # PLOT Abundance (before MPAs) ----------------------------------------------------------
    abundance_spatial_mean_sp[abundance_spatial_mean_sp == 0] = NA
    
    png(here("figures/appendix/FigA7.2_Abundance_nompa_viridis_legend.png"), height = 600, width = 800, units = 'px')
    max = max(abundance_spatial_mean_sp, na.rm =T)
    min = min(abundance_spatial_mean_sp, na.rm =T)
    brk = c(min, min*40, min*80, min*120, max)
    image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), abundance_spatial_mean_sp, cex.main = 1, breaks = brk, col =viridis(4), xlab = "", ylab = "", xaxt='n', horizontal = T, yaxt='n')
    dev.off()
  }
  
}