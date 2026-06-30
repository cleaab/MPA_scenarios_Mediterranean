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
  
  B_nompa = apply(biom, c(1, 2, 4), sum, na.rm = FALSE) # sum over species

  # With mpa - fishing-the-line
  ncname     <- "/Biomass_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/Fishing-the-line/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  biom <- ncvar_get(nc, varid = "Biomass") 
  
  B_mpa_fishing_the_line = apply(biom, c(1, 2, 4), sum, na.rm = FALSE) # sum over species

  # With mpa - uniform
  ncname     <- "/Biomass_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/GSA-uniform/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  biom <- ncvar_get(nc, varid = "Biomass") 
  
  B_mpa_uniform = apply(biom, c(1, 2, 4), sum, na.rm = FALSE) # sum over species

  # Wih mpa - Proportional redistribution by GSA
  ncname     <- "/Biomass_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/GSA-prop/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  biom <- ncvar_get(nc, varid = "Biomass") 
  
  B_mpa_GSA_prop = apply(biom, c(1, 2, 4), sum, na.rm = FALSE) # sum over species

  
  # Import ncdf spatial yield files --------------------------------------------------
  
  # without mpa
  ncname     <- "/Yield_nompa_scenario"  
  ncfname    <- paste(here("data/spatial"), ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  yield <- ncvar_get(nc, varid = "Catches") 
  
  Y_nompa = apply(yield, c(1, 2, 4), sum, na.rm = FALSE) # sum over species

  # with mpa - fishing the line
  ncname     <- "/Yield_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/Fishing-the-line/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  yield <- ncvar_get(nc, varid = "Catches") 
  
  Y_mpa_fishing_the_line = apply(yield, c(1, 2, 4), sum, na.rm = FALSE) # sum over species

  # With mpa - uniform
  ncname     <- "/Yield_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/GSA-uniform/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  yield <- ncvar_get(nc, varid = "Catches") 
  
  Y_mpa_uniform = apply(yield, c(1, 2, 4), sum, na.rm = FALSE) # sum over species

  # With mpa - Proportional redistribution by GSA
  ncname     <- "/Yield_mpa_scenario"  
  ncfname    <- paste(here("data/spatial/GSA-prop/", mpa_scenario, "/output_mpa_"), mpa_coverage, ncname, ".nc", sep="") 
  nc <- nc_open(ncfname)
  yield <- ncvar_get(nc, varid = "Catches") 
  
  Y_mpa_GSA_prop = apply(yield, c(1, 2, 4), sum, na.rm = FALSE) # sum over species

  
  # Total biomass inside core of MPA, edge, border, outside
  # without mpa
  border.mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/border_edge_", mpa_coverage, ".csv"), sep = ",", header=FALSE)
  mpa_border = border.mpa.file[order(nrow(border.mpa.file):1),] # reverse row order because 0 is bottom left in osmose
  mpa_border = t(as.matrix(mpa_border))
  
  B_ref_inside = B_nompa
  B_ref_inside[mpa_border!=1] = NA
  Tot_B_ref_inside = apply(B_ref_inside, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_ref_inside, main = "Biomass inside MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_ref_edge = B_nompa
  B_ref_edge[mpa_border!=2] = NA
  Tot_B_ref_edge = apply(B_ref_edge, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_ref_edge, main = "Biomass edge MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_ref_border = B_nompa
  B_ref_border[mpa_border!=3] = NA
  Tot_B_ref_border = apply(B_ref_border, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_ref_border, main = "Biomass border MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_ref_outside = B_nompa
  B_ref_outside[mpa_border>0] = NA
  Tot_B_ref_outside = apply(B_ref_outside, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_ref_outside, main = "Biomass outside MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  
  # with mpa - fishing-the-line
  border.mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/border_edge_", mpa_coverage, ".csv"), sep = ",", header=FALSE)
  mpa_border = border.mpa.file[order(nrow(border.mpa.file):1),] # reverse row order because 0 is bottom left in osmose
  mpa_border = t(as.matrix(mpa_border))
  
  B_inside = B_mpa_fishing_the_line
  B_inside[mpa_border!=1] = NA
  Tot_B_inside = apply(B_inside, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_inside, main = "Biomass inside MPAs - Fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_edge = B_mpa_fishing_the_line
  B_edge[mpa_border!=2] = NA
  Tot_B_edge = apply(B_edge, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_edge, main = "Biomass edge MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_border = B_mpa_fishing_the_line
  B_border[mpa_border!=3] = NA
  Tot_B_border = apply(B_border, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_border, main = "Biomass border MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_outside = B_mpa_fishing_the_line
  B_outside[mpa_border>0] = NA
  Tot_B_outside = apply(B_outside, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_outside, main = "Biomass outside MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  # with mpa - uniform
  B_inside_uniform = B_mpa_uniform
  B_inside_uniform[mpa_border!=1] = NA
  Tot_B_inside_uniform = apply(B_inside_uniform, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_inside_uniform, main = "Biomass inside MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_edge_uniform = B_mpa_uniform
  B_edge_uniform[mpa_border!=2] = NA
  Tot_B_edge_uniform = apply(B_edge_uniform, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_edge_uniform, main = "Biomass edge MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_border_uniform = B_mpa_uniform
  B_border_uniform[mpa_border!=3] = NA
  Tot_B_border_uniform = apply(B_border_uniform, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_border_uniform, main = "Biomass border MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_outside_uniform = B_mpa_uniform
  B_outside_uniform[mpa_border>0] = NA
  Tot_B_outside_uniform = apply(B_outside_uniform, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_outside_uniform, main = "Biomass outside MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  # with mpa - Proportional redistribution by GSA
  border.mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/border_edge_", mpa_coverage, ".csv"), sep = ",", header=FALSE)
  mpa_border = border.mpa.file[order(nrow(border.mpa.file):1),] # reverse row order because 0 is bottom left in osmose
  mpa_border = t(as.matrix(mpa_border))
  
  B_inside_GSA_prop = B_mpa_GSA_prop
  B_inside_GSA_prop[mpa_border!=1] = NA
  Tot_B_inside_GSA_prop = apply(B_inside_GSA_prop, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_inside_GSA_prop, main = "Biomass inside MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_edge_GSA_prop = B_mpa_GSA_prop
  B_edge_GSA_prop[mpa_border!=2] = NA
  Tot_B_edge_GSA_prop = apply(B_edge_GSA_prop, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_edge_GSA_prop, main = "Biomass edge MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_border_GSA_prop = B_mpa_GSA_prop
  B_border_GSA_prop[mpa_border!=3] = NA
  Tot_B_border_GSA_prop = apply(B_border_GSA_prop, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_border_GSA_prop, main = "Biomass border MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  B_outside_GSA_prop = B_mpa_GSA_prop
  B_outside_GSA_prop[mpa_border>0] = NA
  Tot_B_outside_GSA_prop = apply(B_outside_GSA_prop, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), B_outside_GSA_prop, main = "Biomass outside MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  
  # Total catches inside core of MPA, edge, border, outside --------------------------------
  # with mpa - fishing-the-line
  border.mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/border_edge_", mpa_coverage, ".csv"), sep = ",", header=FALSE)
  mpa_border = border.mpa.file[order(nrow(border.mpa.file):1),] # reverse row order because 0 is bottom left in osmose
  mpa_border = t(as.matrix(mpa_border))
  
  Y_inside = Y_mpa_fishing_the_line
  Y_inside[mpa_border!=1] = NA
  Tot_Y_inside = apply(Y_inside, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_inside, main = "Catch inside MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_edge = Y_mpa_fishing_the_line
  Y_edge[mpa_border!=2] = NA
  Tot_Y_edge = apply(Y_edge, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_edge, main = "Catch edge MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_border = Y_mpa_fishing_the_line
  Y_border[mpa_border!=3] = NA
  Tot_Y_border = apply(Y_border, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_border, main = "Catch border MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_outside = Y_mpa_fishing_the_line
  Y_outside[mpa_border>0] = NA
  Tot_Y_outside = apply(Y_outside, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_outside, main = "Catch outside MPAs - fline", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  # with mpa - uniform
  Y_inside_uniform = Y_mpa_uniform
  Y_inside_uniform[mpa_border!=1] = NA
  Tot_Y_inside_uniform = apply(Y_inside_uniform, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_inside_uniform, main = "Catch inside MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_edge_uniform = Y_mpa_uniform
  Y_edge_uniform[mpa_border!=2] = NA
  Tot_Y_edge_uniform = apply(Y_edge_uniform, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_edge_uniform, main = "Catch edge MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_border_uniform = Y_mpa_uniform
  Y_border_uniform[mpa_border!=3] = NA
  Tot_Y_border_uniform = apply(Y_border_uniform, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_border_uniform, main = "Catch border MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_outside_uniform = Y_mpa_uniform
  Y_outside_uniform[mpa_border>0] = NA
  Tot_Y_outside_uniform = apply(Y_outside_uniform, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_outside_uniform, main = "Catch otuside MPAs - uniform", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  # with mpa - Proportional redistribution by GSA
  border.mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/border_edge_", mpa_coverage, ".csv"), sep = ",", header=FALSE)
  mpa_border = border.mpa.file[order(nrow(border.mpa.file):1),] # reverse row order because 0 is bottom left in osmose
  mpa_border = t(as.matrix(mpa_border))
  
  Y_inside_GSA_prop = Y_mpa_GSA_prop
  Y_inside_GSA_prop[mpa_border!=1] = NA
  Tot_Y_inside_GSA_prop = apply(Y_inside_GSA_prop, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_inside_GSA_prop, main = "Catch inside MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_edge_GSA_prop = Y_mpa_GSA_prop
  Y_edge_GSA_prop[mpa_border!=2] = NA
  Tot_Y_edge_GSA_prop = apply(Y_edge_GSA_prop, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_edge_GSA_prop, main = "Catch edge MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_border_GSA_prop = Y_mpa_GSA_prop
  Y_border_GSA_prop[mpa_border!=3] = NA
  Tot_Y_border_GSA_prop = apply(Y_border_GSA_prop, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_border_GSA_prop, main = "Catch border MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_outside_GSA_prop = Y_mpa_GSA_prop
  Y_outside_GSA_prop[mpa_border>0] = NA
  Tot_Y_outside_GSA_prop = apply(Y_outside_GSA_prop, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_outside_GSA_prop, main = "Catch otuside MPAs - GSA prop", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  
  # without mpa 
  Y_ref_inside = Y_nompa
  Y_ref_inside[mpa_border!=1] = NA
  Tot_Y_ref_inside = apply(Y_ref_inside, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_ref_inside, main = "Catch inside MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_ref_edge = Y_nompa
  Y_ref_edge[mpa_border!=2] = NA
  Tot_Y_ref_edge = apply(Y_ref_edge, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_ref_edge, main = "Catch edge MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_ref_border = Y_nompa
  Y_ref_border[mpa_border!=3] = NA
  Tot_Y_ref_border = apply(Y_ref_border, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_ref_border, main = "Catch border MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  Y_ref_outside = Y_nompa
  Y_ref_outside[mpa_border>0] = NA
  Tot_Y_ref_outside = apply(Y_ref_outside, c(3), sum, na.rm = T)
  # image.plot(x=seq(2880851,6680851,20000), y = seq(865443.7,2515444, 20000), Y_ref_outside, main = "Catch outside MPAs - reference", cex.main = 1, col =RdYlBu(16), xlab = "Longitude", ylab = "Latitude")
  
  
  Tot_Y_mpa_fline = c(Tot_Y_inside, Tot_Y_edge, Tot_Y_border, Tot_Y_outside)
  Tot_B_mpa_fline = c(Tot_B_inside, Tot_B_edge, Tot_B_border, Tot_B_outside)
  
  Tot_Y_mpa_uniform = c(Tot_Y_inside_uniform, Tot_Y_edge_uniform, Tot_Y_border_uniform, Tot_Y_outside_uniform)
  Tot_B_mpa_uniform = c(Tot_B_inside_uniform, Tot_B_edge_uniform, Tot_B_border_uniform, Tot_B_outside_uniform)
  
  Tot_Y_mpa_GSA_prop = c(Tot_Y_inside_GSA_prop, Tot_Y_edge_GSA_prop, Tot_Y_border_GSA_prop, Tot_Y_outside_GSA_prop)
  Tot_B_mpa_GSA_prop = c(Tot_B_inside_GSA_prop, Tot_B_edge_GSA_prop, Tot_B_border_GSA_prop, Tot_B_outside_GSA_prop)
  
  Tot_Y_nompa = c(Tot_Y_ref_inside, Tot_Y_ref_edge, Tot_Y_ref_border, Tot_Y_ref_outside)
  Tot_B_nompa = c(Tot_B_ref_inside, Tot_B_ref_edge, Tot_B_ref_border, Tot_B_ref_outside)
  
  # ------------------------- FIGURE ed
  Area = c("Inside - core", "Inside - edge", "Outside - edge", "Outside > 20 km")
  n_rep <- 30
  
  # Data is ordered: 30 reps of area1, 30 reps of area2, 30 reps of area3, 30 reps of area4
  make_long_B <- function(B_data, scenario_name) {
    data.frame(
      replicate = rep(1:n_rep, times = 4),       # 1:30 repeated 4 times
      area      = rep(Area, each = n_rep),        # 30x core, 30x edge, 30x border, 30x outside
      B_value   = B_data,
      scenario  = scenario_name
    )
  }
  
  make_long_Y <- function(Y_data, scenario_name) {
    data.frame(
      replicate = rep(1:n_rep, times = 4),
      area      = rep(Area, each = n_rep),
      Y_value   = Y_data,
      scenario  = scenario_name
    )
  }
  

  df_fline_B    <- make_long_B(Tot_B_mpa_fline, "Fishing-the-line")
  df_uniform_B  <- make_long_B(Tot_B_mpa_uniform, "Uniform by GSA")
  df_GSA_prop_B <- make_long_B(Tot_B_mpa_GSA_prop, "Proportional by GSA")
  df_nompa_B <- make_long_B(Tot_B_nompa, "No MPA") %>%
    rename(B_ref = B_value) %>%
    dplyr::select(-scenario)
  
  
  df_fline_Y    <- make_long_Y(Tot_Y_mpa_fline, "Fishing-the-line")
  df_uniform_Y  <- make_long_Y(Tot_Y_mpa_uniform, "Uniform by GSA")
  df_GSA_prop_Y <- make_long_Y(Tot_Y_mpa_GSA_prop, "Proportional by GSA")
  df_nompa_Y <- make_long_Y(Tot_Y_nompa, "No MPA") %>%
    rename(Y_ref = Y_value) %>%
    dplyr::select(-scenario)
  
  
  
  # Combine and join reference
  B_inside_edge_border_outside <- rbind(df_fline_B, df_uniform_B, df_GSA_prop_B) %>%
    left_join(df_nompa_B, by = c("replicate", "area")) %>%
    mutate(
      rel_B = (B_value - B_ref) / B_ref * 100,
    ) %>%
    group_by(area, scenario) %>%
    summarize(rel_B_mean = mean(rel_B),
              rel_B_sd = sd(rel_B))
  
  Y_inside_edge_border_outside <- rbind(df_fline_Y, df_uniform_Y, df_GSA_prop_Y) %>%
    left_join(df_nompa_Y, by = c("replicate", "area")) %>%
    mutate(
      rel_Y = (Y_value - Y_ref) / Y_ref * 100,
    ) %>%
    group_by(area, scenario) %>%
    summarize(rel_Y_mean = mean(rel_Y),
              rel_Y_sd = sd(rel_Y))
  # Plots
  # Biomass
  B_inside_edge_border_outside = B_inside_edge_border_outside %>%
    mutate(area = factor(area, levels = c("Inside - core", "Inside - edge", "Outside - edge", "Outside > 20 km")))
  
  barplot_biom <- ggplot(B_inside_edge_border_outside, aes(x = scenario, y = rel_B_mean)) + 
    geom_col(aes(fill = area), position = "dodge") +
    geom_errorbar(aes(group = area,
                      ymin = rel_B_mean - rel_B_sd,
                      ymax = rel_B_mean + rel_B_sd),
                  position = position_dodge(width = 0.9),
                  width = 0.25) +
    theme_bw() +
    theme(axis.text = element_text(size = 10), axis.title = element_text(size = 14), legend.text = element_text(size = 14)) +
    labs(title = paste0("Total biomass - ", mpa_scenario),
         x = "", y = "Biomass change (%)") +
    scale_fill_brewer(palette = "BuPu")
  
  
  # Catches
  Y_inside_edge_border_outside = Y_inside_edge_border_outside %>%
    mutate(area = factor(area, levels = c("Inside - core", "Inside - edge", "Outside - edge", "Outside > 20 km")))
  
  barplot_yield <- ggplot(Y_inside_edge_border_outside, aes(x = scenario, y = rel_Y_mean)) + 
    geom_col(aes(fill = area), position = "dodge") +
    geom_errorbar(aes(group = area,
                      ymin = rel_Y_mean - rel_Y_sd,
                      ymax = rel_Y_mean + rel_Y_sd),
                  position = position_dodge(width = 0.9),
                  width = 0.25) +
    theme_bw() +
    theme(axis.text = element_text(size = 10), axis.title = element_text(size = 14), legend.text = element_text(size = 14)) +
    labs(title = paste0("Total catch - ", mpa_scenario),
         x = "", y = "Catch change (%)") +
    scale_fill_brewer(palette = "BuPu")
  
  
  # ----------------------------------------------------------------------------------------
  
  # Group with ggarrange
  barplot_yield_mod <- barplot_yield + theme(plot.title = element_blank(), legend.title = element_blank())
  barplot_biom_mod  <- barplot_biom  + theme(plot.title = element_blank(), legend.title = element_blank())
  
  fig2 <- ggarrange(barplot_biom_mod, barplot_yield_mod,
                    ncol = 2,
                    nrow = 1,
                    common.legend = T,
                    legend = "top",
                    align = "hv",
                    widths  = c(1, 1),
                    heights = c(1, 1)) +
    theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
  
  if (save == TRUE){
    ggsave(here("figures/Fig_2_biomass_catch_change_through_MPA_border_ed.png"), dpi = 1000, width = 10, height = 5)
  }
  
}
