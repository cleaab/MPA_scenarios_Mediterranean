#' Function to plot changes in biomass and catch relative to before MPA establishment at 10% for each fishing redistribution strategy according to the distance to the MPA
#'
#' 
#' @param data OSMOSE-MED simulation outputs of total biomass and total catch for each fishing redistribution strategy
#' 
#' @return Figure 2 (main text)
#'
#' @export
#' 

plot.biomass.catch.change.across.MPA <- function(lit.mpa.mask.dir, scen = 1, mpa_scenario, mpa_coverage = 10, save = TRUE){
  
  # Import ncdf spatial biomass files ---------------------------------------------
  # Without mpa
  ncname     <- "/Biomass_nompa_scenario"  
  ncfname    <- paste(here("data/spatial"), ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  biom <- ncvar_get(nc, varid = "Biomass") 
  
  biom_sum_species = apply(biom, c(1, 2, 4), sum, na.rm = FALSE) # sum over species
  B_nompa = apply(biom_sum_species, c(1, 2), mean, na.rm = FALSE) # mean over 30 simulations
  
  # With mpa - fishing-the-line
  ncname     <- "/Biomass_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/Fishing-the-line/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  biom <- ncvar_get(nc, varid = "Biomass") 
  
  biom_sum_species = apply(biom, c(1, 2, 4), sum, na.rm = FALSE) # sum over species
  B_mpa_fishing_the_line = apply(biom_sum_species, c(1, 2), mean, na.rm = FALSE) # mean over 30 simulations
  
  # With mpa - uniform
  ncname     <- "/Biomass_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/GSA-uniform/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  biom <- ncvar_get(nc, varid = "Biomass") 
  
  biom_sum_species = apply(biom, c(1, 2, 4), sum, na.rm = FALSE) # sum over species
  B_mpa_uniform = apply(biom_sum_species, c(1, 2), mean, na.rm = FALSE) # mean over 30 simulations
  
  # Wih mpa - Proportional redistribution by GSA
  ncname     <- "/Biomass_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/GSA-prop/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  biom <- ncvar_get(nc, varid = "Biomass") 
  
  biom_sum_species = apply(biom, c(1, 2, 4), sum, na.rm = FALSE) # sum over species
  B_mpa_GSA_prop = apply(biom_sum_species, c(1, 2), mean, na.rm = FALSE) # mean over 30 simulations
  
  
  # Import ncdf spatial yield files --------------------------------------------------
  
  # without mpa
  ncname     <- "/Yield_nompa_scenario"  
  ncfname    <- paste(here("data/spatial"), ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  yield <- ncvar_get(nc, varid = "Catches") 
  
  yield_sum_species = apply(yield, c(1, 2, 4), sum, na.rm = FALSE) # sum over species
  Y_nompa = apply(yield_sum_species, c(1, 2), mean, na.rm = FALSE) # mean over 30 simulations
  
  # with mpa - fishing the line
  ncname     <- "/Yield_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/Fishing-the-line/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  yield <- ncvar_get(nc, varid = "Catches") 
  
  yield_sum_species = apply(yield, c(1, 2, 4), sum, na.rm = FALSE) # sum over species
  Y_mpa_fishing_the_line = apply(yield_sum_species, c(1, 2), mean, na.rm = FALSE) # mean over 30 simulations
  
  # With mpa - uniform
  ncname     <- "/Yield_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/GSA-uniform/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  yield <- ncvar_get(nc, varid = "Catches") 
  
  yield_sum_species = apply(yield, c(1, 2, 4), sum, na.rm = FALSE) # sum over species
  Y_mpa_uniform = apply(yield_sum_species, c(1, 2), mean, na.rm = FALSE) # mean over 30 simulations
  
  # With mpa - Proportional redistribution by GSA
  ncname     <- "/Yield_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/GSA-prop/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  yield <- ncvar_get(nc, varid = "Catches") 
  
  yield_sum_species = apply(yield, c(1, 2, 4), sum, na.rm = FALSE) # sum over species
  Y_mpa_GSA_prop = apply(yield_sum_species, c(1, 2), mean, na.rm = FALSE) # mean over 30 simulations
  
  
  # Total biomass inside core of MPA, edge, border, outside
  # without mpa
  border.mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/border_edge_", mpa_coverage, ".csv"), sep = ",", header=FALSE)
  mpa_border = border.mpa.file[order(nrow(border.mpa.file):1),] # reverse row order because 0 is bottom left in osmose
  mpa_border = t(as.matrix(mpa_border))
  
  B_ref_inside = B_nompa
  B_ref_inside[mpa_border!=1] = NA
  Tot_B_ref_inside = sum(B_ref_inside, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_ref_inside, main = "Biomass inside MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_ref_edge = B_nompa
  B_ref_edge[mpa_border!=2] = NA
  Tot_B_ref_edge = sum(B_ref_edge, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_ref_edge, main = "Biomass edge MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_ref_border = B_nompa
  B_ref_border[mpa_border!=3] = NA
  Tot_B_ref_border = sum(B_ref_border, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_ref_border, main = "Biomass border MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_ref_outside = B_nompa
  B_ref_outside[mpa_border>0] = NA
  Tot_B_ref_outside = sum(B_ref_outside, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_ref_outside, main = "Biomass outside MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  
  # with mpa - fishing-the-line
  border.mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/border_edge_", mpa_coverage, ".csv"), sep = ",", header=FALSE)
  mpa_border = border.mpa.file[order(nrow(border.mpa.file):1),] # reverse row order because 0 is bottom left in osmose
  mpa_border = t(as.matrix(mpa_border))
  
  B_inside = B_mpa_fishing_the_line
  B_inside[mpa_border!=1] = NA
  Tot_B_inside = sum(B_inside, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_inside, main = "Biomass inside MPAs - Fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_edge = B_mpa_fishing_the_line
  B_edge[mpa_border!=2] = NA
  Tot_B_edge = sum(B_edge, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_edge, main = "Biomass edge MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_border = B_mpa_fishing_the_line
  B_border[mpa_border!=3] = NA
  Tot_B_border = sum(B_border, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_border, main = "Biomass border MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_outside = B_mpa_fishing_the_line
  B_outside[mpa_border>0] = NA
  Tot_B_outside = sum(B_outside, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_outside, main = "Biomass outside MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  # with mpa - uniform
  B_inside_uniform = B_mpa_uniform
  B_inside_uniform[mpa_border!=1] = NA
  Tot_B_inside_uniform = sum(B_inside_uniform, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_inside_uniform, main = "Biomass inside MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_edge_uniform = B_mpa_uniform
  B_edge_uniform[mpa_border!=2] = NA
  Tot_B_edge_uniform = sum(B_edge_uniform, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_edge_uniform, main = "Biomass edge MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_border_uniform = B_mpa_uniform
  B_border_uniform[mpa_border!=3] = NA
  Tot_B_border_uniform = sum(B_border_uniform, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_border_uniform, main = "Biomass border MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_outside_uniform = B_mpa_uniform
  B_outside_uniform[mpa_border>0] = NA
  Tot_B_outside_uniform = sum(B_outside_uniform, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_outside_uniform, main = "Biomass outside MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  # with mpa - Proportional redistribution by GSA
  border.mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/border_edge_", mpa_coverage, ".csv"), sep = ",", header=FALSE)
  mpa_border = border.mpa.file[order(nrow(border.mpa.file):1),] # reverse row order because 0 is bottom left in osmose
  mpa_border = t(as.matrix(mpa_border))
  
  B_inside_GSA_prop = B_mpa_GSA_prop
  B_inside_GSA_prop[mpa_border!=1] = NA
  Tot_B_inside_GSA_prop = sum(B_inside_GSA_prop, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_inside_GSA_prop, main = "Biomass inside MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_edge_GSA_prop = B_mpa_GSA_prop
  B_edge_GSA_prop[mpa_border!=2] = NA
  Tot_B_edge_GSA_prop = sum(B_edge_GSA_prop, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_edge_GSA_prop, main = "Biomass edge MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_border_GSA_prop = B_mpa_GSA_prop
  B_border_GSA_prop[mpa_border!=3] = NA
  Tot_B_border_GSA_prop = sum(B_border_GSA_prop, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_border_GSA_prop, main = "Biomass border MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_outside_GSA_prop = B_mpa_GSA_prop
  B_outside_GSA_prop[mpa_border>0] = NA
  Tot_B_outside_GSA_prop = sum(B_outside_GSA_prop, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_outside_GSA_prop, main = "Biomass outside MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  
  # Total catches inside core of MPA, edge, border, outside --------------------------------
  # with mpa - fishing-the-line
  border.mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/border_edge_", mpa_coverage, ".csv"), sep = ",", header=FALSE)
  mpa_border = border.mpa.file[order(nrow(border.mpa.file):1),] # reverse row order because 0 is bottom left in osmose
  mpa_border = t(as.matrix(mpa_border))
  
  Y_inside = Y_mpa_fishing_the_line
  Y_inside[mpa_border!=1] = NA
  Tot_Y_inside = sum(Y_inside, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_inside, main = "Catch inside MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_edge = Y_mpa_fishing_the_line
  Y_edge[mpa_border!=2] = NA
  Tot_Y_edge = sum(Y_edge, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_edge, main = "Catch edge MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_border = Y_mpa_fishing_the_line
  Y_border[mpa_border!=3] = NA
  Tot_Y_border = sum(Y_border, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_border, main = "Catch border MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_outside = Y_mpa_fishing_the_line
  Y_outside[mpa_border>0] = NA
  Tot_Y_outside = sum(Y_outside, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_outside, main = "Catch outside MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  # with mpa - uniform
  Y_inside_uniform = Y_mpa_uniform
  Y_inside_uniform[mpa_border!=1] = NA
  Tot_Y_inside_uniform = sum(Y_inside_uniform, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_inside_uniform, main = "Catch inside MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_edge_uniform = Y_mpa_uniform
  Y_edge_uniform[mpa_border!=2] = NA
  Tot_Y_edge_uniform = sum(Y_edge_uniform, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_edge_uniform, main = "Catch edge MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_border_uniform = Y_mpa_uniform
  Y_border_uniform[mpa_border!=3] = NA
  Tot_Y_border_uniform = sum(Y_border_uniform, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_border_uniform, main = "Catch border MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_outside_uniform = Y_mpa_uniform
  Y_outside_uniform[mpa_border>0] = NA
  Tot_Y_outside_uniform = sum(Y_outside_uniform, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_outside_uniform, main = "Catch otuside MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  # with mpa - Proportional redistribution by GSA
  border.mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/border_edge_", mpa_coverage, ".csv"), sep = ",", header=FALSE)
  mpa_border = border.mpa.file[order(nrow(border.mpa.file):1),] # reverse row order because 0 is bottom left in osmose
  mpa_border = t(as.matrix(mpa_border))
  
  Y_inside_GSA_prop = Y_mpa_GSA_prop
  Y_inside_GSA_prop[mpa_border!=1] = NA
  Tot_Y_inside_GSA_prop = sum(Y_inside_GSA_prop, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_inside_GSA_prop, main = "Catch inside MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_edge_GSA_prop = Y_mpa_GSA_prop
  Y_edge_GSA_prop[mpa_border!=2] = NA
  Tot_Y_edge_GSA_prop = sum(Y_edge_GSA_prop, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_edge_GSA_prop, main = "Catch edge MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_border_GSA_prop = Y_mpa_GSA_prop
  Y_border_GSA_prop[mpa_border!=3] = NA
  Tot_Y_border_GSA_prop = sum(Y_border_GSA_prop, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_border_GSA_prop, main = "Catch border MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_outside_GSA_prop = Y_mpa_GSA_prop
  Y_outside_GSA_prop[mpa_border>0] = NA
  Tot_Y_outside_GSA_prop = sum(Y_outside_GSA_prop, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_outside_GSA_prop, main = "Catch otuside MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  
  # without mpa 
  Y_ref_inside = Y_nompa
  Y_ref_inside[mpa_border!=1] = NA
  Tot_Y_ref_inside = sum(Y_ref_inside, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_ref_inside, main = "Catch inside MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_ref_edge = Y_nompa
  Y_ref_edge[mpa_border!=2] = NA
  Tot_Y_ref_edge = sum(Y_ref_edge, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_ref_edge, main = "Catch edge MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_ref_border = Y_nompa
  Y_ref_border[mpa_border!=3] = NA
  Tot_Y_ref_border = sum(Y_ref_border, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_ref_border, main = "Catch border MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_ref_outside = Y_nompa
  Y_ref_outside[mpa_border>0] = NA
  Tot_Y_ref_outside = sum(Y_ref_outside, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_ref_outside, main = "Catch outside MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  
  Tot_Y_mpa_fline = c(Tot_Y_inside, Tot_Y_edge, Tot_Y_border, Tot_Y_outside)
  Tot_B_mpa_fline = c(Tot_B_inside, Tot_B_edge, Tot_B_border, Tot_B_outside)
  
  Tot_Y_mpa_uniform = c(Tot_Y_inside_uniform, Tot_Y_edge_uniform, Tot_Y_border_uniform, Tot_Y_outside_uniform)
  Tot_B_mpa_uniform = c(Tot_B_inside_uniform, Tot_B_edge_uniform, Tot_B_border_uniform, Tot_B_outside_uniform)
  
  Tot_Y_mpa_GSA_prop = c(Tot_Y_inside_GSA_prop, Tot_Y_edge_GSA_prop, Tot_Y_border_GSA_prop, Tot_Y_outside_GSA_prop)
  Tot_B_mpa_GSA_prop = c(Tot_B_inside_GSA_prop, Tot_B_edge_GSA_prop, Tot_B_border_GSA_prop, Tot_B_outside_GSA_prop)
  
  Tot_Y_nompa = c(Tot_Y_ref_inside, Tot_Y_ref_edge, Tot_Y_ref_border, Tot_Y_ref_outside)
  Tot_B_nompa = c(Tot_B_ref_inside, Tot_B_ref_edge, Tot_B_ref_border, Tot_B_ref_outside)
  
  
  # ------------------------- FIGURE
  Area = c("Inside - core", "Inside - edge", "Outside - edge", "Outside > 20 km")
  
  # Dataframe all values in one column
  B_Y_edge_border = data.frame(c(Tot_B_mpa_fline, Tot_B_mpa_uniform, Tot_B_mpa_GSA_prop, Tot_Y_mpa_fline, Tot_Y_mpa_uniform, Tot_Y_mpa_GSA_prop))
  colnames(B_Y_edge_border) = "value"
  B_Y_edge_border$value_nompa = c(rep(Tot_B_nompa, 3), rep(Tot_Y_nompa, 3))
  B_Y_edge_border$indic = c(rep("Biomass", 12), rep("Catches", 12))
  B_Y_edge_border$area = rep(Area, 3)
  B_Y_edge_border$scenario = rep(c(rep("Fishing-the-line", 4), rep("Uniform by GSA", 4), rep("Proportional by GSA", 4)),2)
  B_Y_edge_border$rel_value = (B_Y_edge_border$value - B_Y_edge_border$value_nompa)/B_Y_edge_border$value_nompa * 100
  
  # Barplot - both scenarios together
  # Biomass
  B_edge_border = B_Y_edge_border %>%
    filter(indic == "Biomass")
  
  B_edge_border$area<- factor(B_edge_border$area, levels = c("Inside - core", "Inside - edge", "Outside - edge", "Outside > 20 km"))
  barplot_biom <- ggplot(B_edge_border, aes(x = scenario, y = rel_value)) + geom_col(aes(fill = area), position = "dodge") +
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12), legend.text = element_text(size = 10)) +
    labs(title= paste0("Total biomass - ", mpa_scenario),
         x ="", y = "Biomass change (%)") +
    scale_fill_brewer(palette = "BuPu")
  
  
  # Catches
  Y_edge_border = B_Y_edge_border %>%
    filter(indic == "Catches") 
  
  Y_edge_border$area<- factor(Y_edge_border$area, levels = c("Inside - core", "Inside - edge", "Outside - edge", "Outside > 20 km"))
  barplot_yield <- ggplot(Y_edge_border, aes(x = scenario, y = rel_value)) + geom_col(aes(fill = area), position = "dodge") +
    theme_bw() +
    theme(axis.text=element_text(size=10), axis.title = element_text(size = 12), legend.text = element_text(size = 10)) +
    
    labs(title= paste0("Total catch - ", mpa_scenario),
         x ="", y = "Catch change (%)") +
    scale_fill_brewer(palette = "BuPu")
  
  
  # ----------------------------------------------------------------------------------------
  # Group with gg arrange
  barplot_yield_mod <- barplot_yield + theme(plot.title = element_blank(), legend.title = element_blank()) 
  barplot_biom_mod <- barplot_biom +  theme(plot.title = element_blank(), legend.title = element_blank()) 
  
  fig5 <- ggarrange(barplot_biom_mod, barplot_yield_mod,
                    ncol=2,
                    nrow=1,
                    common.legend = T,
                    legend = "top",
                    align = "hv",
                    widths = c(1,1),
                    heights = c(1,1)) +
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))
  
  if (save == TRUE){
    ggsave(here("figures/Fig_2_biomass_catch_change_through_MPA_border.png"), dpi = 1000, width = 10, height = 5)
  }
  
}
