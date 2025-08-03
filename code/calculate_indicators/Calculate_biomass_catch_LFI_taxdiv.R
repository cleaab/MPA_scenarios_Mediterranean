library("ncdf4")
library("tidyverse")
library("data.table")
library("optparse")

option_list <- list(
    make_option(c("-c","--CC_scenario"), type="character", default=NULL, help="Climate change scenario"),
    make_option(c("-f","--fishing_strategy"), type="character", default=NULL, help="Fishing redistribution strategy after MPA implementation"));

opt_parser = OptionParser(usage="Usage: Rscript %prog -c ${CC_scenario} -f ${fishing_strategy}", option_list=option_list);

opt = parse_args(opt_parser);


#-----------------Loading packages-------------------

tbl_fread <- function(output.Folder, pattern){
  files = list.files(output.Folder, pattern, full.names = TRUE) %>% 
    map_df(~fread(.)) %>%
    bind_rows()
}

# OSMOSE configuration used - v4 without recalibrating after spatialization fishing effort
F_species = read.csv(here("data/osm_param-fishing.csv"), sep = ";", header=FALSE)
colnames(F_species) <- c("species_id", "F")
F_species  <- F_species[1:101,] # keep only fishing.rate parameter
F_species$species_id = gsub(".*\\.", "", F_species$species_id) # keep only species name
F_species$species_id = substr(F_species$species_id, 3, 7) # keep only species number

# Plot fishing mortality by species
species_name = read.table(here("data/osm_param-species.csv"), sep = ";", row.names = NULL, header= FALSE)
species_name = species_name[1:101,] # keep only species name
colnames(species_name)<- c("species_id", "species_name")
species_name$species_id = gsub(".*\\.", "", species_name$species_id)        # Apply gsub with \\
species_name$species_id = substr(species_name$species_id, 3, 7)

species = merge(x = F_species, y = species_name, by = "species_id") # merge species name with species F mortality
species_sorted = species[order(species$species_name), ] # sort species in alphabetical order

# Grouping all benthic together, blue whiting = large demersal
vertical_distri = read.csv(here("data/vertical_distribution_species.csv"))
colnames(vertical_distri) = c("species_name", "group")
species_sorted = merge(species_sorted, vertical_distri, by = "species_name")
colnames(species_sorted) = c("species_name", "species_id", "F", "group", "order")
species_id = as.numeric(species_sorted$species_id)

Step = 30 
NbofYears = 60
Nbofreps = 30
pas = 62 


group = c("Large-sized pelagic fish",
	      "Medium-sized pelagic fish",
  	      "Small-sized pelagic fish",
 	      "Large-sized demersal fish",
	      "Medium-sized demersal fish",
	      "Small-sized demersal fish",
	      "Large-sized benthic fish",
	      "Medium-sized benthic fish",
	      "Small-sized benthic fish",
	      "Crustacean",
	      "Cephalopod")

## Directories outputs (stored in HPC Datarmor) --------------------------------
# output.Dir.nompa # output baseline (Nompa)
# output.Dir.ref.noCC # output baseline (Nompa - Biomass and Catch aggregated spatial outputs) 
output.Dir.ref.noCC = here("data/spatial")
# lit.scenarios.dir # outputs of S1,S4-S6
# random.scenarios.dir # outputs of S2 and S3 (10 replicates each)

## ----------------------------------------------------------------------------- 

lit.scenarios.name <- c("Mazor scenario 8",
                    "Mazor scenario 9",
                    "Micheli",
                    "Existing network")

random.scenarios.name <- c("Random 11",
                    "Random 12",
                    "Random 13",
                    "Random 14",
                    "Random 15",
                    "Random 16",
                    "Random 17",
                    "Random 18",
                    "Random 19",
                    "Random 20",
		                "EEZ-conservation 1",
		                "EEZ-conservation 2",
	                  "EEZ-conservation 3",
                    "EEZ-conservation 4",
                    "EEZ-conservation 5",
                    "EEZ-conservation 6",
                    "EEZ-conservation 7",
                    "EEZ-conservation 8",
                    "EEZ-conservation 9",
                    "EEZ-conservation 10")


lit.mpa.mask.dir <- c(here("MPA_scenarios/Mazor_scenario_8/csv_gpkg_files"),
                      here("MPA_scenarios/Mazor_scenario_9/csv_gpkg_files"),
                      here("MPA_scenarios/Micheli/csv_gpkg_files"),
                      here("MPA_scenarios/Existing_network/csv_gpkg_files"))

random.mpa.mask.dir <- c(here("MPA_scenarios/Random/rep11/csv_gpkg_files"),
                       here("MPA_scenarios/Random/rep12/csv_gpkg_files"),
                       here("MPA_scenarios/Random/rep13/csv_gpkg_files"),
                       here("MPA_scenarios/Random/rep14/csv_gpkg_files"),
                       here("MPA_scenarios/Random/rep15/csv_gpkg_files"),
                       here("MPA_scenarios/Random/rep16/csv_gpkg_files"),
                       here("MPA_scenarios/Random/rep17/csv_gpkg_files"),
                       here("MPA_scenarios/Random/rep18/csv_gpkg_files"),
                       here("MPA_scenarios/Random/rep19/csv_gpkg_files"),
                       here("MPA_scenarios/Random/rep20/csv_gpkg_files"),
                       here("MPA_scenarios/EEZ-conservation/rep1/csv_gpkg_files"),
                       here("MPA_scenarios/EEZ-conservation/rep2/csv_gpkg_files"),
                       here("MPA_scenarios/EEZ-conservation/rep3/csv_gpkg_files"),
                       here("MPA_scenarios/EEZ-conservation/rep4/csv_gpkg_files"),
                       here("MPA_scenarios/EEZ-conservation/rep5/csv_gpkg_files"),
                       here("MPA_scenarios/EEZ-conservation/rep6/csv_gpkg_files"),
                       here("MPA_scenarios/EEZ-conservation/rep7/csv_gpkg_files"),
                       here("MPA_scenarios/EEZ-conservation/rep8/csv_gpkg_files"),
                       here("MPA_scenarios/EEZ-conservation/rep9/csv_gpkg_files"),
                       here("MPA_scenarios/EEZ-conservation/rep10/csv_gpkg_files"))


## FUNCTIONS to calculate indicators -------------------------------------------

Total.biomass.inside.outside.MPAs.notake.fn <- function(var, mpa.output.Dir, nompa.output.Dir, Step, pas){ # var : Biomass or Yield
  
  Data.all = data.frame()
  Med_sea = 6229 * 400
  
  for (i in 1:(Step+1)){
    if (i == 1){
      # for 0% MPA
      Biom = tbl_fread(nompa.output.Dir, pattern = "med_biomass_Simu")
      
      Biom$simu = rep(seq(1,Nbofreps,1), each = NbofYears)
      Biom_mean = aggregate(Biom, by=list(Biom$simu), mean) # mean over 60 last years
      Biom_mean = Biom_mean[,-c(1,2,104)]
      
      Y <- as.data.frame(do.call(rbind, Biom_mean), row.names=FALSE)
      Y <- cbind(Y, species_sorted)

      # if NoCC scenario :
      species_fished = Y
   
      species_fished = species_fished[, c(1:30)]
      species_sum = colSums(species_fished, na.rm = TRUE) # sum biomass or yield of species fished
      
      inside.mpa = as.data.frame(mean(species_sum))
      inside.mpa$sd.tot.biom = sd(species_sum)
      inside.mpa$mpa.coverage = i-1
      
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      colnames(inside.mpa)  = c("tot.biomass", "sd.tot.biomass", "mpa.coverage", "mpa", "x_scale")
      
      all.Med = as.data.frame(mean(species_sum))
      all.Med$sd.tot.biom = sd(species_sum)
      all.Med$mpa.coverage = i-1
      
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 
      colnames(all.Med)  = c("tot.biomass", "sd.tot.biomass", "mpa.coverage","mpa", "x_scale")
      
      outside.mpa = as.data.frame(mean(species_sum))
      outside.mpa$sd.tot.biom = sd(species_sum)
      outside.mpa$mpa.coverage = i-1
      
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      colnames(outside.mpa)  = c("tot.biomass", "sd.tot.biomass", "mpa.coverage", "mpa", "x_scale")
   
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all = data.frame(rbind(Data.all, Data)) 
    }
    
    else{
      
      output.Biom = paste0(mpa.output.Dir, "/output_mpa_", i-1)
      Biom = tbl_fread(output.Biom, pattern = "med_biomass")
      
      Biom$simu = rep(seq(1,Nbofreps*2,1), each = NbofYears)
      Biom_mean = aggregate(Biom, by=list(Biom$simu), mean) # mean over 10 last years
      Biom_mean = Biom_mean[,-c(1,2,104)]
      
      Y <- as.data.frame(do.call(rbind, Biom_mean), row.names=FALSE)
      Y <- cbind(Y, species_sorted)
      
      species_fished = Y
  
      species_fished = species_fished[, c(1:60)]
      species_sum = colSums(species_fished, na.rm = TRUE) # sum biomass or yield of species fished
      species_sum[61:90] = species_sum[1:30] - species_sum[31:60] # calculate tot biomass outside MPAs
      
      inside.mpa = as.data.frame(mean(species_sum[31:60]))
      inside.mpa$sd.tot.biom = sd(species_sum[31:60])
      inside.mpa$mpa.coverage = i-1
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      colnames(inside.mpa)  = c("tot.biomass", "sd.tot.biomass", "mpa.coverage", "mpa", "x_scale")
      
      
      all.Med = as.data.frame(mean(species_sum[1:30]))
      all.Med$sd.tot.biom = sd(species_sum[1:30])
      all.Med$mpa.coverage = i-1
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 # quand pas régulier
      colnames(all.Med)  = c("tot.biomass", "sd.tot.biomass", "mpa.coverage", "mpa", "x_scale")
      
      outside.mpa = as.data.frame(mean(species_sum[61:90]))
      outside.mpa$sd.tot.biom = sd(species_sum[61:90])
      outside.mpa$mpa.coverage = i-1
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      colnames(outside.mpa)  = c("tot.biomass", "sd.tot.biomass", "mpa.coverage", "mpa", "x_scale")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all = data.frame(rbind(Data.all, Data))
    }
  }
  return(Data.all)
}

Total.biomass.mpa.ref.fn <- function(var, varid, output.Dir.ref.noCC, mpa.mask.dir, Step, pas){

  ncname     <- paste0("/", var, "_nompa_scenario")  
  ncfname    <- paste(output.Dir.ref.noCC, ncname, ".nc", sep="")
  nc <- nc_open(ncfname)
  ref_data <- ncvar_get(nc, varid = varid) 
  
  
  species_group = species_sorted 

  species_group_id = as.numeric(species_group$species_id)
  ref_data_species_fished = ref_data[1:190,1:83,(species_group_id + 1), 1:30] # select species where F>0
  
  ref_data_tot_species = apply(ref_data_species_fished, c(1, 2, 4), sum, na.rm = FALSE) # sum over species fished
  ref_data_tot_species_mean_rep = apply(ref_data_tot_species, c(1, 2), mean, na.rm = FALSE) # mean over 30 simulations
  ref_data_tot_species_sd_rep = apply(ref_data_tot_species, c(1, 2), sd, na.rm = FALSE) # sd between 30 simulations
  
  sum(ref_data_tot_species_mean_rep, na.rm = TRUE)
  
  Inside = c()
  Inside_sd = c()
  Outside = c()
  Outside_sd = c()
  All = c()
  All_sd = c()
  
  Med_sea = 6229 * 400
  
  for (i in 1:(Step + 1)){
    if (i == 1){
      ## No MPAs
      copy.ref_data_tot_species_mean_rep = ref_data_tot_species_mean_rep
      ref.data.no.mpa = sum(copy.ref_data_tot_species_mean_rep, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      Inside = c(Inside, ref.data.no.mpa) # 0 because no MPAs
      Outside = c(Outside, ref.data.no.mpa)
      All = c(All, ref.data.no.mpa)
      
      copy.ref_data_tot_species_sd_rep = ref_data_tot_species_sd_rep
      ref.data.no.mpa.sd = sum(copy.ref_data_tot_species_sd_rep, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      Inside_sd = c(Inside_sd, ref.data.no.mpa.sd)
      Outside_sd = c(Outside_sd, ref.data.no.mpa.sd)
      All_sd = c(All_sd, ref.data.no.mpa.sd)
    }
    
    else{
      # Extract according to MPA coverage %
      mpa = read.csv(paste0(mpa.mask.dir, "/", i-1, ".csv"), sep = ",", header=FALSE)
      mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
      mpa = t(as.matrix(mpa))
      
      # INSIDE
      ## Calculate mean biomass density of species fished (F>0) found in MPAs
      copy.ref_data_tot_species_mean_rep = ref_data_tot_species_mean_rep
      copy.ref_data_tot_species_mean_rep[mpa==0] = NA #set to NA zones which are not MPAs
      ref.data.inside.mpa = sum(copy.ref_data_tot_species_mean_rep, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      Inside = c(Inside, ref.data.inside.mpa)
      
      ## Calculate sd of biomass density of species fished (F>0) found in MPAs
      copy.ref_data_tot_species_sd_rep = ref_data_tot_species_sd_rep
      copy.ref_data_tot_species_sd_rep[mpa==0] = NA
      ref.data.inside.mpa.sd = sum(copy.ref_data_tot_species_sd_rep, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      Inside_sd = c(Inside_sd, ref.data.inside.mpa.sd)
      
      # OUTSIDE
      ## Calculate mean biomass density of species fished (F>0) found outside MPAs
      copy.ref_data_tot_species_mean_rep_out = ref_data_tot_species_mean_rep
      copy.ref_data_tot_species_mean_rep_out[mpa>0] = NA #set to NA zones which MPAs
      ref.data.outside.mpa = sum(copy.ref_data_tot_species_mean_rep_out, na.rm=TRUE) # sum averaged biomass outside MPA cells
      
      Outside = c(Outside, ref.data.outside.mpa)
      
      ## Calculate sd of biomass density of species fished (F>0) found in MPAs
      copy.ref_data_tot_species_sd_rep_out = ref_data_tot_species_sd_rep
      copy.ref_data_tot_species_sd_rep_out[mpa>0] = NA
      ref.data.outside.mpa.sd = sum(copy.ref_data_tot_species_sd_rep_out, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      Outside_sd = c(Outside_sd, ref.data.outside.mpa.sd)
      
      # TOTAL
      copy.ref_data_tot_species_mean_rep_tot = ref_data_tot_species_mean_rep
      ref.data.all = sum(copy.ref_data_tot_species_mean_rep_tot, na.rm=TRUE) # sum averaged biomass in entire domain
      
      All = c(All, ref.data.all)
      
      copy.ref_data_tot_species_sd_rep_out = ref_data_tot_species_sd_rep
      ref.data.all.sd = sum(copy.ref_data_tot_species_sd_rep_out, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      All_sd = c(All_sd, ref.data.all.sd)
    }
  }
  
  Biom = c(Inside, Outside, All)
  Biom_sd = c(Inside_sd, Outside_sd, All_sd)
  mpa_coverage = rep(seq(0, Step, 1), 3)
  x_scale = mpa_coverage * pas / 6229 * 100 # pas reguliers
  mpa = c(rep("Inside MPAs in reference scenario", Step + 1), rep("Outside MPAs in reference scenario", Step + 1), rep("Entire Mediterranean Sea in reference scenario", Step + 1))
  output = data.frame(Biom, Biom_sd, mpa_coverage, mpa, x_scale)
  
  colnames(output) = c("tot.biomass", "sd.tot.biomass", "mpa.coverage", "mpa", "x_scale")
  
  
  return(output)
}


Total.catches.inside.outside.MPAs.notake.fn <- function(var, mpa.output.Dir, nompa.output.Dir, Step, pas){ # var : Biomass or Yield
  
  Data.all = data.frame()
  Med_sea = 6229 * 400
  
  for (i in 1:(Step+1)){
    if (i == 1){
      # for 0% MPA
      Biom = tbl_fread(nompa.output.Dir, pattern = "med_yield_Simu")
      
      Biom$simu = rep(seq(1,Nbofreps,1), each = NbofYears)
      Biom_mean = aggregate(Biom, by=list(Biom$simu), mean) # mean over 60 last years
      Biom_mean = Biom_mean[,-c(1,2,104)]
      
      Y <- as.data.frame(do.call(rbind, Biom_mean), row.names=FALSE)
      Y <- cbind(Y, species_sorted)
      
      # if NoCC scenario :
      species_fished = Y
  
      species_fished = species_fished[, c(1:30)]
      species_sum = colSums(species_fished, na.rm = TRUE) # sum biomass or yield of species fished
      
      inside.mpa = as.data.frame(mean(species_sum))
      inside.mpa$sd.tot.biom = sd(species_sum)
      inside.mpa$mpa.coverage = i-1
      
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      colnames(inside.mpa)  = c("tot.catches", "sd.tot.catches", "mpa.coverage", "mpa", "x_scale")
      
      all.Med = as.data.frame(mean(species_sum))
      all.Med$sd.tot.biom = sd(species_sum)
      all.Med$mpa.coverage = i-1
      
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 
      colnames(all.Med)  = c("tot.catches", "sd.tot.catches", "mpa.coverage","mpa", "x_scale")
      
      outside.mpa = as.data.frame(mean(species_sum))
      outside.mpa$sd.tot.biom = sd(species_sum)
      outside.mpa$mpa.coverage = i-1
      
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      colnames(outside.mpa)  = c("tot.catches", "sd.tot.catches", "mpa.coverage", "mpa", "x_scale")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all = data.frame(rbind(Data.all, Data)) 
    }
    
    else{
      output.Biom = paste0(mpa.output.Dir, "/output_mpa_", i-1)
      Biom.mpa = tbl_fread(output.Biom, pattern = "med_yield-mpa")
      
      Biom.mpa$simu = rep(seq(1,Nbofreps*1,1), each = NbofYears)
      Biom_mean_mpa = aggregate(Biom.mpa, by=list(Biom.mpa$simu), mean) # mean over 10 last years
      Biom_mean_mpa = Biom_mean_mpa[,-c(1,2,104)]
      
      Biom.nompa = tbl_fread(output.Biom, pattern = "med_yield_Simu")
      Biom.nompa$simu = rep(seq(1,Nbofreps*1,1), each = NbofYears)
      Biom_mean = aggregate(Biom.nompa, by=list(Biom.nompa$simu), mean) # mean over 10 last years
      Biom_mean = Biom_mean[,-c(1,2,104)]
      
      Biom_mean_mpa_nompa = rbind(Biom_mean, Biom_mean_mpa)
      
      Y <- as.data.frame(do.call(rbind, Biom_mean_mpa_nompa), row.names=FALSE)
      Y <- cbind(Y, species_sorted)
      
      species_fished = Y
  
      species_fished = species_fished[, c(1:60)]
      species_sum = colSums(species_fished, na.rm = TRUE) # sum biomass or yield of species fished
      species_sum[61:90] = species_sum[1:30] - species_sum[31:60] # calculate tot biomass outside MPAs
      
      inside.mpa = as.data.frame(mean(species_sum[31:60]))
      inside.mpa$sd.tot.biom = sd(species_sum[31:60])
      inside.mpa$mpa.coverage = i-1
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      colnames(inside.mpa)  = c("tot.catches", "sd.tot.catches", "mpa.coverage", "mpa", "x_scale")
      
      
      all.Med = as.data.frame(mean(species_sum[1:30]))
      all.Med$sd.tot.biom = sd(species_sum[1:30])
      all.Med$mpa.coverage = i-1
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 # quand pas régulier
      colnames(all.Med)  = c("tot.catches", "sd.tot.catches", "mpa.coverage", "mpa", "x_scale")
      
      outside.mpa = as.data.frame(mean(species_sum[61:90]))
      outside.mpa$sd.tot.biom = sd(species_sum[61:90])
      outside.mpa$mpa.coverage = i-1
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      colnames(outside.mpa)  = c("tot.catches", "sd.tot.catches", "mpa.coverage", "mpa", "x_scale")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all = data.frame(rbind(Data.all, Data))
    }
  }
  return(Data.all)
}

Total.catches.mpa.ref.fn <- function(var, varid, output.Dir.ref.noCC, mpa.mask.dir, Step, pas){
  
  ncname     <- paste0("/", var, "_nompa_scenario")  
  ncfname    <- paste(output.Dir.ref.noCC, ncname, ".nc", sep="")
  nc <- nc_open(ncfname)
  ref_data <- ncvar_get(nc, varid = varid) 
  
  
  species_group = species_sorted 
pecies_group_id = as.numeric(species_group$species_id)
  ref_data_species_fished = ref_data[1:190,1:83,(species_group_id + 1), 1:30] # select species where F>0
  
  ref_data_tot_species = apply(ref_data_species_fished, c(1, 2, 4), sum, na.rm = FALSE) # sum over species fished
  ref_data_tot_species_mean_rep = apply(ref_data_tot_species, c(1, 2), mean, na.rm = FALSE) # mean over 30 simulations
  ref_data_tot_species_sd_rep = apply(ref_data_tot_species, c(1, 2), sd, na.rm = FALSE) # sd between 30 simulations
  
  sum(ref_data_tot_species_mean_rep, na.rm = TRUE)
  
  Inside = c()
  Inside_sd = c()
  Outside = c()
  Outside_sd = c()
  All = c()
  All_sd = c()
  
  Med_sea = 6229 * 400
  
  for (i in 1:(Step + 1)){
    if (i == 1){
      ## No MPAs
      copy.ref_data_tot_species_mean_rep = ref_data_tot_species_mean_rep
      ref.data.no.mpa = sum(copy.ref_data_tot_species_mean_rep, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      Inside = c(Inside, ref.data.no.mpa) # 0 because no MPAs
      Outside = c(Outside, ref.data.no.mpa)
      All = c(All, ref.data.no.mpa)
      
      copy.ref_data_tot_species_sd_rep = ref_data_tot_species_sd_rep
      ref.data.no.mpa.sd = sum(copy.ref_data_tot_species_sd_rep, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      Inside_sd = c(Inside_sd, ref.data.no.mpa.sd)
      Outside_sd = c(Outside_sd, ref.data.no.mpa.sd)
      All_sd = c(All_sd, ref.data.no.mpa.sd)
    }
    
    else{
      # Extract according to MPA coverage %
      mpa = read.csv(paste0(mpa.mask.dir, "/", i-1, ".csv"), sep = ",", header=FALSE)
      mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
      mpa = t(as.matrix(mpa))
      
      # INSIDE
      ## Calculate mean biomass density of species fished (F>0) found in MPAs
      copy.ref_data_tot_species_mean_rep = ref_data_tot_species_mean_rep
      copy.ref_data_tot_species_mean_rep[mpa==0] = NA #set to NA zones which are not MPAs
      ref.data.inside.mpa = sum(copy.ref_data_tot_species_mean_rep, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      Inside = c(Inside, ref.data.inside.mpa)
      
      ## Calculate sd of biomass density of species fished (F>0) found in MPAs
      copy.ref_data_tot_species_sd_rep = ref_data_tot_species_sd_rep
      copy.ref_data_tot_species_sd_rep[mpa==0] = NA
      ref.data.inside.mpa.sd = sum(copy.ref_data_tot_species_sd_rep, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      Inside_sd = c(Inside_sd, ref.data.inside.mpa.sd)
      
      # OUTSIDE
      ## Calculate mean biomass density of species fished (F>0) found outside MPAs
      copy.ref_data_tot_species_mean_rep_out = ref_data_tot_species_mean_rep
      copy.ref_data_tot_species_mean_rep_out[mpa>0] = NA #set to NA zones which MPAs
      ref.data.outside.mpa = sum(copy.ref_data_tot_species_mean_rep_out, na.rm=TRUE) # sum averaged biomass outside MPA cells
      
      Outside = c(Outside, ref.data.outside.mpa)
      
      ## Calculate sd of biomass density of species fished (F>0) found in MPAs
      copy.ref_data_tot_species_sd_rep_out = ref_data_tot_species_sd_rep
      copy.ref_data_tot_species_sd_rep_out[mpa>0] = NA
      ref.data.outside.mpa.sd = sum(copy.ref_data_tot_species_sd_rep_out, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      Outside_sd = c(Outside_sd, ref.data.outside.mpa.sd)
      
      # TOTAL
      copy.ref_data_tot_species_mean_rep_tot = ref_data_tot_species_mean_rep
      ref.data.all = sum(copy.ref_data_tot_species_mean_rep_tot, na.rm=TRUE) # sum averaged biomass in entire domain
      
      All = c(All, ref.data.all)
      
      copy.ref_data_tot_species_sd_rep_out = ref_data_tot_species_sd_rep
      ref.data.all.sd = sum(copy.ref_data_tot_species_sd_rep_out, na.rm=TRUE) # sum averaged biomass in all MPA cells
      
      All_sd = c(All_sd, ref.data.all.sd)
    }
  }
  
  Biom = c(Inside, Outside, All)
  Biom_sd = c(Inside_sd, Outside_sd, All_sd)
  mpa_coverage = rep(seq(0, Step, 1), 3)
  x_scale = mpa_coverage * pas / 6229 * 100 # pas reguliers
  mpa = c(rep("Inside MPAs in reference scenario", Step + 1), rep("Outside MPAs in reference scenario", Step + 1), rep("Entire Mediterranean Sea in reference scenario", Step + 1))
  output = data.frame(Biom, Biom_sd, mpa_coverage, mpa, x_scale)
  
  colnames(output) = c("tot.catches", "sd.tot.catches", "mpa.coverage", "mpa", "x_scale")
  
  
  return(output)
}


Biomass.by.groups.inside.outside.MPAs.notake.fn <- function(var, mpa.output.Dir, nompa.output.Dir, Step, pas, group){ # var : Biomass or Yield
  
  Data.all = data.frame()
  Med_sea = 6229 * 400
  
  for (i in 1:(Step+1)){
    if (i == 1){
      # for 0% MPA
      Biom = tbl_fread(nompa.output.Dir, pattern = "med_biomass_Simu")
      
      Biom$simu = rep(seq(1,Nbofreps,1), each = NbofYears)
      Biom_mean = aggregate(Biom, by=list(Biom$simu), mean) # mean over 60 last years
      Biom_mean = Biom_mean[,-c(1,2,104)]
      
      Y <- as.data.frame(do.call(rbind, Biom_mean), row.names=FALSE)
      Y <- cbind(Y, species_sorted)
      
      # if NoCC scenario :
      species_fished = Y

      species_fished = species_fished[species_fished$group == group,]
      
      species_fished = species_fished[, c(1:30)]
      species_sum = colSums(species_fished, na.rm = TRUE) # sum biomass or yield of species fished
      
      inside.mpa = as.data.frame(species_sum)
      inside.mpa$mpa.coverage = i-1
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      colnames(inside.mpa)  = c("tot.biomass", "mpa.coverage", "mpa", "x_scale")
      
      all.Med = as.data.frame(species_sum)
      all.Med$mpa.coverage = i-1
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 
      colnames(all.Med)  = c("tot.biomass", "mpa.coverage","mpa", "x_scale")
      
      outside.mpa = as.data.frame(species_sum)
      outside.mpa$mpa.coverage = i-1
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      colnames(outside.mpa)  = c("tot.biomass", "mpa.coverage", "mpa", "x_scale")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all = data.frame(rbind(Data.all, Data)) 
    }
    
    else{
      
      output.Biom = paste0(mpa.output.Dir, "/output_mpa_", i-1)
      Biom = tbl_fread(output.Biom, pattern = "med_biomass")
      
      Biom$simu = rep(seq(1,Nbofreps*2,1), each = NbofYears)
      Biom_mean = aggregate(Biom, by=list(Biom$simu), mean) # mean over 10 last years
      Biom_mean = Biom_mean[,-c(1,2,104)]
      
      Y <- as.data.frame(do.call(rbind, Biom_mean), row.names=FALSE)
      Y <- cbind(Y, species_sorted)
      
      species_fished = Y
   
      species_fished = species_fished[species_fished$group == group,]
      
      species_fished = species_fished[, c(1:60)]
      species_sum = colSums(species_fished, na.rm = TRUE) # sum biomass or yield of species fished
      species_sum[61:90] = species_sum[1:30] - species_sum[31:60] # calculate tot biomass outside MPAs
      
      inside.mpa = as.data.frame(species_sum[31:60])
      inside.mpa$mpa.coverage = i-1
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      colnames(inside.mpa)  = c("tot.biomass", "mpa.coverage", "mpa", "x_scale")
      
      
      all.Med = as.data.frame(species_sum[1:30])
      all.Med$mpa.coverage = i-1
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 # quand pas régulier
      colnames(all.Med)  = c("tot.biomass", "mpa.coverage", "mpa", "x_scale")
      
      outside.mpa = as.data.frame(species_sum[61:90])
      outside.mpa$mpa.coverage = i-1
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      colnames(outside.mpa)  = c("tot.biomass", "mpa.coverage", "mpa", "x_scale")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all = data.frame(rbind(Data.all, Data))
      
    }
    
  }
  Data.all$scenario_rep = rep(seq(1,30,1), 31*3)
  
  return(Data.all)
}

Biomass.by.groups.mpa.ref.fn <- function(var, varid, output.Dir.ref.noCC, mpa.mask.dir, Step, pas, group){
  
  ncname     <- paste0("/", var, "_nompa_scenario")  
  ncfname    <- paste(output.Dir.ref.noCC, ncname, ".nc", sep="")
  nc <- nc_open(ncfname)
  ref_data <- ncvar_get(nc, varid = varid) 
  
  
  species_group = species_sorted 

  species_group = species_sorted[species_sorted$group == group, ]
  
  species_group_id = as.numeric(species_group$species_id)
  ref_data_species_fished = ref_data[1:190,1:83,(species_group_id + 1), 1:30] # select species where F>0
  
  ref_data_tot_species = apply(ref_data_species_fished, c(1, 2, 4), sum, na.rm = FALSE) # sum over species fished
  
  Data.all.ref = data.frame()
  Med_sea = 6229 * 400
  
  for (i in 1:(Step + 1)){
    
    if (i == 1){
      ref.data.no.mpa.all.reps =c()
      for (r in 1:Nbofreps){
        ## No MPAs
        copy.ref_data_tot_species_rep_r = ref_data_tot_species[1:190,1:83,r]
        ref.data.no.mpa = sum(copy.ref_data_tot_species_rep_r, na.rm=TRUE) # sum averaged biomass in all MPA cells
        ref.data.no.mpa.all.reps = c(ref.data.no.mpa.all.reps, ref.data.no.mpa)
      }
      
      inside.mpa = as.data.frame(ref.data.no.mpa.all.reps)
      inside.mpa$mpa.coverage = i-1
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      inside.mpa$scenario_rep = seq(1,30,1)
      
      colnames(inside.mpa)  = c("tot.biomass.ref", "mpa.coverage", "mpa", "x_scale", "scenario_rep")
      
      all.Med = as.data.frame(ref.data.no.mpa.all.reps)
      all.Med$mpa.coverage = i-1
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 
      all.Med$scenario_rep = seq(1,30,1)
      
      colnames(all.Med)  = c("tot.biomass.ref", "mpa.coverage","mpa", "x_scale", "scenario_rep")
      
      outside.mpa = as.data.frame(ref.data.no.mpa.all.reps)
      outside.mpa$mpa.coverage = i-1 
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      outside.mpa$scenario_rep = seq(1,30,1)
      
      colnames(outside.mpa)  = c("tot.biomass.ref", "mpa.coverage", "mpa",  "x_scale", "scenario_rep")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all.ref = data.frame(rbind(Data.all.ref, Data))
    }
    
    else{
      # Extract according to MPA coverage %
      mpa = read.csv(paste0(mpa.mask.dir, "/", i-1, ".csv"), sep = ",", header=FALSE)
      mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
      mpa = t(as.matrix(mpa))
      
      ref.data.inside.mpa.all.reps = c()
      ref.data.outside.mpa.all.reps = c()
      ref.data.all.Med.all.reps = c()
      for(r in 1:Nbofreps){
        
        # INSIDE
        ## Calculate mean biomass density of species fished (F>0) found in MPAs
        copy.ref_data_tot_species_in_rep_r = ref_data_tot_species[1:190,1:83,r]
        copy.ref_data_tot_species_in_rep_r[mpa==0] = NA #set to NA zones which are not MPAs
        ref.data.inside.mpa = sum(copy.ref_data_tot_species_in_rep_r, na.rm=TRUE) # sum biomass in all MPA cells
        ref.data.inside.mpa.all.reps = c(ref.data.inside.mpa.all.reps, ref.data.inside.mpa)
        
        # OUTSIDE
        ## Calculate mean biomass density of species fished (F>0) found outside MPAs
        copy.ref_data_tot_species_out_rep_r = ref_data_tot_species[1:190,1:83,r]
        copy.ref_data_tot_species_out_rep_r[mpa>0] = NA #set to NA zones which MPAs
        ref.data.outside.mpa = sum(copy.ref_data_tot_species_out_rep_r, na.rm=TRUE) # sum averaged biomass outside MPA cells
        ref.data.outside.mpa.all.reps = c(ref.data.outside.mpa.all.reps, ref.data.outside.mpa)
        
        # TOTAL
        copy.ref_data_tot_species_tot_rep_r = ref_data_tot_species[1:190,1:83,r]
        ref.data.all = sum(copy.ref_data_tot_species_tot_rep_r, na.rm=TRUE) # sum averaged biomass in entire domain
        ref.data.all.Med.all.reps = c(ref.data.all.Med.all.reps, ref.data.all)
        
      }
      
      inside.mpa = as.data.frame(ref.data.inside.mpa.all.reps)
      inside.mpa$mpa.coverage = i-1
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      inside.mpa$scenario_rep = seq(1,30,1)
      
      colnames(inside.mpa)  = c("tot.biomass.ref", "mpa.coverage", "mpa", "x_scale", "scenario_rep")
      
      all.Med = as.data.frame(ref.data.all.Med.all.reps)
      all.Med$mpa.coverage = i-1
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 
      all.Med$scenario_rep = seq(1,30,1)
      
      colnames(all.Med)  = c("tot.biomass.ref", "mpa.coverage","mpa", "x_scale", "scenario_rep")
      
      outside.mpa = as.data.frame(ref.data.outside.mpa.all.reps)
      outside.mpa$mpa.coverage = i-1 
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      outside.mpa$scenario_rep = seq(1,30,1)
      
      colnames(outside.mpa)  = c("tot.biomass.ref", "mpa.coverage", "mpa",  "x_scale", "scenario_rep")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all.ref = data.frame(rbind(Data.all.ref, Data))
      
    }
  }
  
  
  return(Data.all.ref)
}


Catches.by.groups.inside.outside.MPAs.notake.fn <- function(var, mpa.output.Dir, nompa.output.Dir, Step, pas, group){ # var : Biomass or Yield
  
  Data.all = data.frame()
  Med_sea = 6229 * 400
  
  for (i in 1:(Step+1)){
    if (i == 1){
      # for 0% MPA
      Biom = tbl_fread(nompa.output.Dir, pattern = "med_yield_Simu")
      
      Biom$simu = rep(seq(1,Nbofreps,1), each = NbofYears)
      Biom_mean = aggregate(Biom, by=list(Biom$simu), mean) # mean over 60 last years
      Biom_mean = Biom_mean[,-c(1,2,104)]
      
      Y <- as.data.frame(do.call(rbind, Biom_mean), row.names=FALSE)
      Y <- cbind(Y, species_sorted)
      
      # if NoCC scenario :
      species_fished = Y

      species_fished = species_fished[species_fished$group == group,]
      
      species_fished = species_fished[, c(1:30)]
      species_sum = colSums(species_fished, na.rm = TRUE) # sum biomass or yield of species fished
      
      inside.mpa = as.data.frame(species_sum)
      inside.mpa$mpa.coverage = i-1
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      colnames(inside.mpa)  = c("tot.catches", "mpa.coverage", "mpa", "x_scale")
      
      all.Med = as.data.frame(species_sum)
      all.Med$mpa.coverage = i-1
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 
      colnames(all.Med)  = c("tot.catches", "mpa.coverage","mpa", "x_scale")
      
      outside.mpa = as.data.frame(species_sum)
      outside.mpa$mpa.coverage = i-1
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      colnames(outside.mpa)  = c("tot.catches", "mpa.coverage", "mpa", "x_scale")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all = data.frame(rbind(Data.all, Data)) 
    }
    
    else{
      
      output.Biom = paste0(mpa.output.Dir, "/output_mpa_", i-1)
      Biom.mpa = tbl_fread(output.Biom, pattern = "med_yield-mpa")
      
      Biom.mpa$simu = rep(seq(1,Nbofreps*1,1), each = NbofYears)
      Biom_mean_mpa = aggregate(Biom.mpa, by=list(Biom.mpa$simu), mean) # mean over 60 last years
      Biom_mean_mpa = Biom_mean_mpa[,-c(1,2,104)]
      
      Biom.nompa = tbl_fread(output.Biom, pattern = "med_yield_Simu")
      Biom.nompa$simu = rep(seq(1,Nbofreps*1,1), each = NbofYears)
      Biom_mean = aggregate(Biom.nompa, by=list(Biom.nompa$simu), mean) # mean over 60 last years
      Biom_mean = Biom_mean[,-c(1,2,104)]
      
      Biom_mean_mpa_nompa = rbind(Biom_mean, Biom_mean_mpa)
      
      Y <- as.data.frame(do.call(rbind, Biom_mean_mpa_nompa), row.names=FALSE)
      Y <- cbind(Y, species_sorted)
      
      species_fished = Y
      # species_fished = Y %>%
      #   filter(!species_name %in% c("Alosaalosa", "Alosafallax", "Anguillaanguilla")) 
      species_fished = species_fished[species_fished$group == group,]
      
      species_fished = species_fished[, c(1:60)]
      species_sum = colSums(species_fished, na.rm = TRUE) # sum biomass or yield of species fished
      species_sum[61:90] = species_sum[1:30] - species_sum[31:60] # calculate tot biomass outside MPAs
      
      inside.mpa = as.data.frame(species_sum[31:60])
      inside.mpa$mpa.coverage = i-1
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      colnames(inside.mpa)  = c("tot.catches", "mpa.coverage", "mpa", "x_scale")
      
      
      all.Med = as.data.frame(species_sum[1:30])
      all.Med$mpa.coverage = i-1
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 # quand pas régulier
      colnames(all.Med)  = c("tot.catches", "mpa.coverage", "mpa", "x_scale")
      
      outside.mpa = as.data.frame(species_sum[61:90])
      outside.mpa$mpa.coverage = i-1
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      colnames(outside.mpa)  = c("tot.catches", "mpa.coverage", "mpa", "x_scale")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all = data.frame(rbind(Data.all, Data))
      
    }
    
  }
  Data.all$scenario_rep = rep(seq(1,30,1), 31*3)
  
  return(Data.all)
}

Catches.by.groups.mpa.ref.fn <- function(var, varid, output.Dir.ref.noCC, mpa.mask.dir, Step, pas, group){
  
  ncname     <- paste0("/", var, "_nompa_scenario")  
  ncfname    <- paste(output.Dir.ref.noCC, ncname, ".nc", sep="")
  nc <- nc_open(ncfname)
  ref_data <- ncvar_get(nc, varid = varid) 
  
  
  species_group = species_sorted 
  #species_group = species_sorted %>%
  #      filter(!species_name %in% c("Alosaalosa", "Alosafallax", "Anguillaanguilla")) 
  species_group = species_sorted[species_sorted$group == group, ]
  
  species_group_id = as.numeric(species_group$species_id)
  ref_data_species_fished = ref_data[1:190,1:83,(species_group_id + 1), 1:30] # select species where F>0
  
  ref_data_tot_species = apply(ref_data_species_fished, c(1, 2, 4), sum, na.rm = FALSE) # sum over species fished
  
  Data.all.ref = data.frame()
  Med_sea = 6229 * 400
  
  for (i in 1:(Step + 1)){
    
    if (i == 1){
      ref.data.no.mpa.all.reps =c()
      for (r in 1:Nbofreps){
        ## No MPAs
        copy.ref_data_tot_species_rep_r = ref_data_tot_species[1:190,1:83,r]
        ref.data.no.mpa = sum(copy.ref_data_tot_species_rep_r, na.rm=TRUE) # sum averaged biomass in all MPA cells
        ref.data.no.mpa.all.reps = c(ref.data.no.mpa.all.reps, ref.data.no.mpa)
      }
      
      inside.mpa = as.data.frame(ref.data.no.mpa.all.reps)
      inside.mpa$mpa.coverage = i-1
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      inside.mpa$scenario_rep = seq(1,30,1)
      
      colnames(inside.mpa)  = c("tot.catches.ref", "mpa.coverage", "mpa", "x_scale", "scenario_rep")
      
      all.Med = as.data.frame(ref.data.no.mpa.all.reps)
      all.Med$mpa.coverage = i-1
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 
      all.Med$scenario_rep = seq(1,30,1)
      
      colnames(all.Med)  = c("tot.catches.ref", "mpa.coverage","mpa", "x_scale", "scenario_rep")
      
      outside.mpa = as.data.frame(ref.data.no.mpa.all.reps)
      outside.mpa$mpa.coverage = i-1 
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      outside.mpa$scenario_rep = seq(1,30,1)
      
      colnames(outside.mpa)  = c("tot.catches.ref", "mpa.coverage", "mpa",  "x_scale", "scenario_rep")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all.ref = data.frame(rbind(Data.all.ref, Data))
    }
    
    else{
      # Extract according to MPA coverage %
      mpa = read.csv(paste0(mpa.mask.dir, "/", i-1, ".csv"), sep = ",", header=FALSE)
      mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
      mpa = t(as.matrix(mpa))
      
      ref.data.inside.mpa.all.reps = c()
      ref.data.outside.mpa.all.reps = c()
      ref.data.all.Med.all.reps = c()
      for(r in 1:Nbofreps){
        
        # INSIDE
        ## Calculate mean biomass density of species fished (F>0) found in MPAs
        copy.ref_data_tot_species_in_rep_r = ref_data_tot_species[1:190,1:83,r]
        copy.ref_data_tot_species_in_rep_r[mpa==0] = NA #set to NA zones which are not MPAs
        ref.data.inside.mpa = sum(copy.ref_data_tot_species_in_rep_r, na.rm=TRUE) # sum biomass in all MPA cells
        ref.data.inside.mpa.all.reps = c(ref.data.inside.mpa.all.reps, ref.data.inside.mpa)
        
        # OUTSIDE
        ## Calculate mean biomass density of species fished (F>0) found outside MPAs
        copy.ref_data_tot_species_out_rep_r = ref_data_tot_species[1:190,1:83,r]
        copy.ref_data_tot_species_out_rep_r[mpa>0] = NA #set to NA zones which MPAs
        ref.data.outside.mpa = sum(copy.ref_data_tot_species_out_rep_r, na.rm=TRUE) # sum averaged biomass outside MPA cells
        ref.data.outside.mpa.all.reps = c(ref.data.outside.mpa.all.reps, ref.data.outside.mpa)
        
        # TOTAL
        copy.ref_data_tot_species_tot_rep_r = ref_data_tot_species[1:190,1:83,r]
        ref.data.all = sum(copy.ref_data_tot_species_tot_rep_r, na.rm=TRUE) # sum averaged biomass in entire domain
        ref.data.all.Med.all.reps = c(ref.data.all.Med.all.reps, ref.data.all)
        
      }
      
      inside.mpa = as.data.frame(ref.data.inside.mpa.all.reps)
      inside.mpa$mpa.coverage = i-1
      inside.mpa$mpa = "Inside MPAs"
      inside.mpa$x_scale = inside.mpa$mpa.coverage *pas/6229*100
      inside.mpa$scenario_rep = seq(1,30,1)
      
      colnames(inside.mpa)  = c("tot.catches.ref", "mpa.coverage", "mpa", "x_scale", "scenario_rep")
      
      all.Med = as.data.frame(ref.data.all.Med.all.reps)
      all.Med$mpa.coverage = i-1
      all.Med$mpa = "Entire Mediterranean Sea"
      all.Med$x_scale = all.Med$mpa.coverage *pas/6229*100 
      all.Med$scenario_rep = seq(1,30,1)
      
      colnames(all.Med)  = c("tot.catches.ref", "mpa.coverage","mpa", "x_scale", "scenario_rep")
      
      outside.mpa = as.data.frame(ref.data.outside.mpa.all.reps)
      outside.mpa$mpa.coverage = i-1 
      outside.mpa$mpa = "Outside MPAs"
      outside.mpa$x_scale = outside.mpa$mpa.coverage *pas/6229*100
      outside.mpa$scenario_rep = seq(1,30,1)
      
      colnames(outside.mpa)  = c("tot.catches.ref", "mpa.coverage", "mpa",  "x_scale", "scenario_rep")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all.ref = data.frame(rbind(Data.all.ref, Data))
      
    }
  }
  
  
  return(Data.all.ref)
}


LFI.with.sd.fn <- function(threshold, nompa.output.Dir, mpa.output.Dir){
  
  Data.all = data.frame()
  Med_sea = 6229 * 400
  
  for (i in 1:(Step+1)){
    if (i == 1){
      i = 1
      # for 0% MPA
      path.dir = paste0(nompa.output.Dir, "/Indicators")
      Biom = tbl_fread(path.dir, pattern = "med_biomassDistribBySize_Simu")
      
      Biom_reps = Biom %>%
        mutate(rep = rep(seq(1, Nbofreps, 1), each = NbofYears*83)) 
      
      for (r in 1:30){
        Biom_sum = Biom_reps %>%
          filter(rep == r) %>%
          mutate(Biom_all_species = rowSums(select(.,3:103))) %>%
          dplyr::select(Time, Size, Biom_all_species) %>%
          group_by(Time) %>%
          mutate(Total_biomass_by_year = sum(Biom_all_species)) %>%
          filter(Size >= threshold) %>%
          mutate(Total_biomass_by_year_over_threshold = sum(Biom_all_species)) %>%
          distinct(Total_biomass_by_year_over_threshold, .keep_all = T) %>%
          mutate(LFI = Total_biomass_by_year_over_threshold/Total_biomass_by_year) %>%
          dplyr::select(-Size) %>%
          ungroup() %>%
          summarise(mean_LFI_60_years = mean(LFI)) %>%
          mutate(rep = r)
        
        if (r == 1){
          Data = Biom_sum
        } else{
          Data = rbind(Data, Biom_sum)
        }
      }
      
      LFI_mean = Data %>%
        #summarise(mean_LFI_by_years_reps = mean(LFI), sd_LFI_by_years_reps = sd(LFI)) %>%
        mutate(mpa_coverage = i-1) %>%
        mutate(x_scale = mpa_coverage * pas/6229*100)
      
      inside.mpa = LFI_mean %>%
        mutate(mpa = "Inside MPAs")
      
      all.Med = LFI_mean %>%
        mutate(mpa = "Entire Mediterranean Sea")
      
      outside.mpa = LFI_mean %>%
        mutate(mpa = "Outside MPAs")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all = data.frame(rbind(Data.all, Data)) 
    }
    
    else{
      
      output.Biom = paste0(mpa.output.Dir, "/output_mpa_", i-1, "/Indicators")
      Biom_tot = tbl_fread(output.Biom, pattern = "med_biomassDistribBySize_Simu")
      Biom_mpa = tbl_fread(output.Biom, pattern = "med_biomassDistribBySize-mpa")
      
      Biom_tot_reps = Biom_tot %>%
        mutate(rep = rep(seq(1, Nbofreps, 1), each = NbofYears*83))
      
      Biom_mpa_reps = Biom_mpa %>%
        mutate(rep = rep(seq(1, Nbofreps, 1), each = NbofYears*83)) 
      
      Biom_outside_reps = Biom_tot_reps - Biom_mpa_reps
      Biom_outside_reps$Time = Biom_tot_reps$Time
      Biom_outside_reps$Size = Biom_tot_reps$Size
      Biom_outside_reps$rep = rep(seq(1, Nbofreps, 1), each = NbofYears*83)
      
      for (r in 1:30){
        
        Biom_sum_Med = Biom_tot_reps %>%
          filter(rep == r) %>%
          mutate(Biom_all_species = rowSums(select(.,3:103))) %>%
          dplyr::select(Time, Size, Biom_all_species) %>%
          group_by(Time) %>%
          mutate(Total_biomass_by_year = sum(Biom_all_species)) %>%
          filter(Size >= threshold) %>%
          mutate(Total_biomass_by_year_over_threshold = sum(Biom_all_species)) %>%
          distinct(Total_biomass_by_year_over_threshold, .keep_all = T) %>%
          mutate(LFI = Total_biomass_by_year_over_threshold/Total_biomass_by_year) %>%
          dplyr::select(-Size) %>%
          ungroup() %>%
          summarise(mean_LFI_60_years = mean(LFI)) %>%
          mutate(rep = r)
        
        Biom_sum_mpa = Biom_mpa_reps %>%
          filter(rep == r) %>%
          mutate(Biom_all_species = rowSums(select(.,3:103))) %>%
          dplyr::select(Time, Size, Biom_all_species) %>%
          group_by(Time) %>%
          mutate(Total_biomass_by_year = sum(Biom_all_species)) %>%
          filter(Size >= threshold) %>%
          mutate(Total_biomass_by_year_over_threshold = sum(Biom_all_species)) %>%
          distinct(Total_biomass_by_year_over_threshold, .keep_all = T) %>%
          mutate(LFI = Total_biomass_by_year_over_threshold/Total_biomass_by_year) %>%
          dplyr::select(-Size) %>%
          ungroup() %>%
          summarise(mean_LFI_60_years = mean(LFI)) %>%
          mutate(rep = r)
        
        Biom_sum_outside = Biom_outside_reps %>%
          filter(rep == r) %>%
          mutate(Biom_all_species = rowSums(select(.,3:103))) %>%
          dplyr::select(Time, Size, Biom_all_species) %>%
          group_by(Time) %>%
          mutate(Total_biomass_by_year = sum(Biom_all_species)) %>%
          filter(Size >= threshold) %>%
          mutate(Total_biomass_by_year_over_threshold = sum(Biom_all_species)) %>%
          distinct(Total_biomass_by_year_over_threshold, .keep_all = T) %>%
          mutate(LFI = Total_biomass_by_year_over_threshold/Total_biomass_by_year) %>%
          dplyr::select(-Size) %>%
          ungroup() %>%
          summarise(mean_LFI_60_years = mean(LFI)) %>%
          mutate(rep = r)
        
        if (r == 1){
          Data_Med = Biom_sum_Med
          Data_mpa = Biom_sum_mpa
          Data_outside = Biom_sum_outside
          
        } else{
          Data_Med = rbind(Data_Med, Biom_sum_Med)
          Data_mpa = rbind(Data_mpa, Biom_sum_mpa)
          Data_outside= rbind(Data_outside, Biom_sum_outside)
          
        }
      }
      
      inside.mpa = Data_mpa %>%
        #summarise(mean_LFI_by_years_reps = mean(LFI), sd_LFI_by_years_reps = sd(LFI)) %>%
        mutate(mpa_coverage = i-1) %>%
        mutate(x_scale = mpa_coverage * pas/6229*100) %>%
        mutate(mpa = "Inside MPAs")
      
      all.Med = Data_Med %>%
        #summarise(mean_LFI_by_years_reps = mean(LFI), sd_LFI_by_years_reps = sd(LFI)) %>%
        mutate(mpa_coverage = i-1) %>%
        mutate(x_scale = mpa_coverage * pas/6229*100) %>%
        mutate(mpa = "Entire Mediterranean Sea")
      
      outside.mpa = Data_outside %>%
        #summarise(mean_LFI_by_years_reps = mean(LFI), sd_LFI_by_years_reps = sd(LFI)) %>%
        mutate(mpa_coverage = i-1) %>%
        mutate(x_scale = mpa_coverage * pas/6229*100) %>%
        mutate(mpa = "Outside MPAs")
      
      Data = rbind(inside.mpa, outside.mpa, all.Med)
      Data.all = data.frame(rbind(Data.all, Data))
      
    }
  }
  return(Data.all)
  
}


# BIOMASS ----------------------------------------------------------------------

var = "Biomass"
varid = "Biomass"
Nscenarios = length(lit.scenarios.name) + length(random.scenarios.name)

for (i in 1:Nscenarios){
  if (i<5){
    mpa_scenario = Total.biomass.inside.outside.MPAs.notake.fn(var, lit.scenarios.dir[i], output.Dir.nompa, Step, pas)
    ref_scenario = Total.biomass.mpa.ref.fn(var, varid, output.Dir.ref.noCC, lit.mpa.mask.dir[i], Step, pas)

  }
  else{
    mpa_scenario = Total.biomass.inside.outside.MPAs.notake.fn(var, random.scenarios.dir[i-4], output.Dir.nompa, Step, pas)
    ref_scenario = Total.biomass.mpa.ref.fn(var, varid, output.Dir.ref.noCC, random.mpa.mask.dir[i-4], Step, pas)
  }

  Inside = mpa_scenario[mpa_scenario$mpa == "Inside MPAs",]
  Outside = mpa_scenario[mpa_scenario$mpa == "Outside MPAs",]
  All = mpa_scenario[mpa_scenario$mpa == "Entire Mediterranean Sea",]

  Inside_ref = ref_scenario[ref_scenario$mpa == "Inside MPAs in reference scenario",]
  Outside_ref = ref_scenario[ref_scenario$mpa == "Outside MPAs in reference scenario",]
  All_ref = ref_scenario[ref_scenario$mpa == "Entire Mediterranean Sea in reference scenario",]

  Inside$relative_biom = (Inside$tot.biomass - Inside_ref$tot.biomass)/Inside_ref$tot.biomass * 100
  Outside$relative_biom = (Outside$tot.biomass - Outside_ref$tot.biomass)/Outside_ref$tot.biomass * 100
  All$relative_biom = (All$tot.biomass - All_ref$tot.biomass)/All_ref$tot.biomass * 100

  Inside$relative_biom_sd = (Inside$sd.tot.biomass - Inside_ref$sd.tot.biomass)/Inside_ref$sd.tot.biomass * 100
  Outside$relative_biom_sd = (Outside$sd.tot.biomass - Outside_ref$sd.tot.biomass)/Outside_ref$sd.tot.biomass * 100
  All$relative_biom_sd = (All$sd.tot.biomass - All_ref$sd.tot.biomass)/All_ref$sd.tot.biomass * 100

  Data = rbind(Inside, Outside, All)

  if (i<5){
    Data$scenario = lit.scenarios.name[i]
  } else{
    Data$scenario = random.scenarios.name[i-4]
  }

  Data$min = NA
  Data$max = NA

  if (i == 1){
    Total_biom_all_scenarios = Data
  }
  else{
    Total_biom_all_scenarios = rbind(Total_biom_all_scenarios, Data)
  }
}

write.csv(Total_biom_all_scenarios, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/Total_biomass_", opt$CC_scenario, "_", opt$fishing_strategy, ".csv"), row.names = FALSE)

# CATCHES ----------------------------------------------------------------------
var = "Yield"
varid = "Catches"
Nscenarios = length(lit.scenarios.name) + length(random.scenarios.name)

for (i in 1:Nscenarios){
  if (i<5){
    mpa_scenario = Total.catches.inside.outside.MPAs.notake.fn(var, lit.scenarios.dir[i], output.Dir.nompa, Step, pas)
    ref_scenario = Total.catches.mpa.ref.fn(var, varid, output.Dir.ref.noCC, lit.mpa.mask.dir[i], Step, pas)

  }
  else{
    mpa_scenario = Total.catches.inside.outside.MPAs.notake.fn(var, random.scenarios.dir[i-4], output.Dir.nompa, Step, pas)
    ref_scenario = Total.catches.mpa.ref.fn(var, varid, output.Dir.ref.noCC, random.mpa.mask.dir[i-4], Step, pas)
  }

  Inside = mpa_scenario[mpa_scenario$mpa == "Inside MPAs",]
  Outside = mpa_scenario[mpa_scenario$mpa == "Outside MPAs",]
  All = mpa_scenario[mpa_scenario$mpa == "Entire Mediterranean Sea",]

  Inside_ref = ref_scenario[ref_scenario$mpa == "Inside MPAs in reference scenario",]
  Outside_ref = ref_scenario[ref_scenario$mpa == "Outside MPAs in reference scenario",]
  All_ref = ref_scenario[ref_scenario$mpa == "Entire Mediterranean Sea in reference scenario",]

  Inside$relative_catches = (Inside$tot.catches - Inside_ref$tot.catches)/Inside_ref$tot.catches * 100
  Outside$relative_catches = (Outside$tot.catches - Outside_ref$tot.catches)/Outside_ref$tot.catches * 100
  All$relative_catches = (All$tot.catches - All_ref$tot.catches)/All_ref$tot.catches * 100

  Inside$relative_catches_sd = (Inside$sd.tot.catches - Inside_ref$sd.tot.catches)/Inside_ref$sd.tot.catches * 100
  Outside$relative_catches_sd = (Outside$sd.tot.catches - Outside_ref$sd.tot.catches)/Outside_ref$sd.tot.catches * 100
  All$relative_catches_sd = (All$sd.tot.catches - All_ref$sd.tot.catches)/All_ref$sd.tot.catches * 100

  Data = rbind(Inside, Outside, All)

  if (i<5){
    Data$scenario = lit.scenarios.name[i]
  } else{
    Data$scenario = random.scenarios.name[i-4]
  }

  Data$min = NA
  Data$max = NA

  if (i == 1){
    Total_catches_all_scenarios = Data
  }
  else{
    Total_catches_all_scenarios = rbind(Total_catches_all_scenarios, Data)
  }
}

write.csv(Total_catches_all_scenarios, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/Total_catches_", opt$CC_scenario, "_", opt$fishing_strategy, ".csv"), row.names = FALSE)

# LFI ------------------------------------------------------------------------

# LFI with sd among reps

Nscenarios = length(lit.scenarios.name) + length(random.scenarios.name)

for (i in 1:Nscenarios){
  if (i<5){
    LFI = LFI.with.sd.fn(threshold = 20, nompa.output.Dir = output.Dir.nompa, mpa.output.Dir = lit.scenarios.dir[i])
    LFI$scenario = lit.scenarios.name[i]
  }
  else{
    LFI = LFI.with.sd.fn(threshold = 20, nompa.output.Dir = output.Dir.nompa, mpa.output.Dir = random.scenarios.dir[i-4])
    LFI$scenario = random.scenarios.name[i-4]
  }
  
  if (i == 1){
    LFI_all_scenarios = LFI
  }
  else{
    LFI_all_scenarios = rbind(LFI_all_scenarios, LFI)
  }
}

write.csv(LFI_all_scenarios, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/LFI_with_sd_among_reps", opt$CC_scenario, "_", opt$fishing_strategy, ".csv"), row.names = FALSE)

# Biomass by groups -----------------------------------------------------------

var = "Biomass"
varid = "Biomass"
Nscenarios = length(lit.scenarios.name) + length(random.scenarios.name)

for (g in 1:length(group)){

 for (i in 1:Nscenarios){
   if (i<5){
     mpa_scenario = Biomass.by.groups.inside.outside.MPAs.notake.fn(var, lit.scenarios.dir[i], output.Dir.nompa, Step, pas, group[g])
     ref_scenario = Biomass.by.groups.mpa.ref.fn(var, varid, output.Dir.ref.noCC, lit.mpa.mask.dir[i], Step, pas, group[g])
     data_scenario = mpa_scenario %>%
       left_join(ref_scenario, by = c("mpa.coverage", "mpa", "scenario_rep", "x_scale"))
     data_scenario$scenario = lit.scenarios.name[i]
     data_scenario$group = group[g]
   }
   else{
     mpa_scenario = Biomass.by.groups.inside.outside.MPAs.notake.fn(var, random.scenarios.dir[i-4], output.Dir.nompa, Step, pas, group[g])
     ref_scenario = Biomass.by.groups.mpa.ref.fn(var, varid, output.Dir.ref.noCC, random.mpa.mask.dir[i-4], Step, pas, group[g])
     data_scenario = mpa_scenario %>%
       left_join(ref_scenario, by = c("mpa.coverage", "mpa", "scenario_rep", "x_scale"))
     data_scenario$scenario = random.scenarios.name[i-4]
     data_scenario$group = group[g]
   }

   if (i == 1){
     Total_biom_all_scenarios_bygroups = data_scenario
   }
   else{
     Total_biom_all_scenarios_bygroups = rbind(Total_biom_all_scenarios_bygroups, data_scenario)
   }
 }

 if (g == 1){
   Biom_all_scenarios_all_groups = Total_biom_all_scenarios_bygroups

 } else{
   Biom_all_scenarios_all_groups = rbind(Biom_all_scenarios_all_groups, Total_biom_all_scenarios_bygroups)
 }


}

write.csv(Biom_all_scenarios_all_groups, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/Biomass_bygroups_", opt$CC_scenario, "_", opt$fishing_strategy, ".csv"), row.names = FALSE)

# Catches by groups ----

var = "Yield"
varid = "Catches"
Nscenarios = length(lit.scenarios.name) + length(random.scenarios.name)

for (g in 1:length(group)){
  
  for (i in 1:Nscenarios){
    if (i<5){
      mpa_scenario = Catches.by.groups.inside.outside.MPAs.notake.fn(var, lit.scenarios.dir[i], output.Dir.nompa, Step, pas, group[g])
      ref_scenario = Catches.by.groups.mpa.ref.fn(var, varid, output.Dir.ref.noCC, lit.mpa.mask.dir[i], Step, pas, group[g])
      data_scenario = mpa_scenario %>%
        left_join(ref_scenario, by = c("mpa.coverage", "mpa", "scenario_rep", "x_scale"))
      data_scenario$scenario = lit.scenarios.name[i]
      data_scenario$group = group[g]
      
    }
    else{
      mpa_scenario = Catches.by.groups.inside.outside.MPAs.notake.fn(var, random.scenarios.dir[i-4], output.Dir.nompa, Step, pas, group[g])
      ref_scenario = Catches.by.groups.mpa.ref.fn(var, varid, output.Dir.ref.noCC, random.mpa.mask.dir[i-4], Step, pas, group[g])
      data_scenario = mpa_scenario %>%
        left_join(ref_scenario, by = c("mpa.coverage", "mpa", "scenario_rep", "x_scale"))
      data_scenario$scenario = random.scenarios.name[i-4]
      data_scenario$group = group[g]
      
    }
    
    
    if (i == 1){
      Total_catches_all_scenarios_bygroups = data_scenario
    }
    else{
      Total_catches_all_scenarios_bygroups = rbind(Total_catches_all_scenarios_bygroups, data_scenario)
    }
  }
  
  if (g == 1){
    Catches_all_scenarios_all_groups = Total_catches_all_scenarios_bygroups
    
  } else{
    Catches_all_scenarios_all_groups = rbind(Catches_all_scenarios_all_groups, Total_catches_all_scenarios_bygroups)
  }
  
  
}

write.csv(Catches_all_scenarios_all_groups, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/Catches_bygroups_", opt$CC_scenario, "_", opt$fishing_strategy, ".csv"), row.names = FALSE)

# Taxonomic alpha-diversity ----------------------------------------------------

# Literature scenarios
# Literature scenarios - inside MPAs
Nscenarios = length(lit.scenarios.name) + length(random.scenarios.name)

for (scenario in 1:length(lit.scenarios.name)){
  for (mpa_coverage in 1:30){
    
    # open abundance
    nc_abundance_spatial  = nc_open(file.path(paste0(lit.scenarios.dir[scenario], "/Spatial_mean/output_mpa_", mpa_coverage, "/Abundance_mpa_scenario.nc")))
    
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
    
    # MPA scenario and coverage %
    mpa = read.csv(paste0(lit.mpa.mask.dir[i], "/", mpa_coverage, ".csv"), sep = ",", header=FALSE)
    mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
    mpa = t(as.matrix(mpa))
    
    exp_H_spatial_copy = exp_H_spatial
    exp_H_spatial_copy[mpa == 0] = NA # keep only inside MPAs
    
    # average exp_H between all cells in areas that will become MPAs (map before MPAs)
    mean_exp_H_inside = mean(exp_H_spatial_copy, na.rm = T)
    
    if (mpa_coverage == 1){
      mean_exp_H_inside_all_coverages = mean_exp_H_inside
    } else{
      mean_exp_H_inside_all_coverages = c(mean_exp_H_inside_all_coverages, mean_exp_H_inside)
    }
    
  }  
  Mean_diversity_inside = data.frame(mean_exp_H_inside_all_coverages) %>%
    rename(mean_exp_H_inside = mean_exp_H_inside_all_coverages) %>%
    mutate(mpa_coverage = seq(1,30,1)) %>%
    mutate(mpa_scenario = lit.scenarios.name[scenario])
  
  if (scenario == 1){
    Mean_diversity_inside_lit_scenarios = Mean_diversity_inside
  } else{
    Mean_diversity_inside_lit_scenarios = rbind(Mean_diversity_inside_lit_scenarios, Mean_diversity_inside)
  }
}

write.csv(Mean_diversity_inside_lit_scenarios, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/Mean_diversity_inside_scenarios.csv"), row.names = FALSE)

# Literature scenarios - outside MPAs
for (scenario in 1:length(lit.scenarios.name)){
  for (mpa_coverage in 1:Nbofreps){
    
    # open abundance
    nc_abundance_spatial  = nc_open(file.path(paste0(lit.scenarios.dir[scenario], "/Spatial_mean/output_mpa_", mpa_coverage, "/Abundance_mpa_scenario.nc")))
    
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
    
    # MPA scenario and coverage %
    mpa = read.csv(paste0(lit.mpa.mask.dir[i], "/", mpa_coverage, ".csv"), sep = ",", header=FALSE)
    mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
    mpa = t(as.matrix(mpa))
    
    exp_H_spatial_copy = exp_H_spatial
    exp_H_spatial_copy[mpa > 0] = NA # keep only outside MPAs
    # average exp_H between all cells in areas that will become MPAs (map before MPAs)
    mean_exp_H_inside = mean(exp_H_spatial_copy, na.rm = T)
    
    if (mpa_coverage == 1){
      mean_exp_H_inside_all_coverages = mean_exp_H_inside
    } else{
      mean_exp_H_inside_all_coverages = c(mean_exp_H_inside_all_coverages, mean_exp_H_inside)
    }
    
  }  
  Mean_diversity_inside = data.frame(mean_exp_H_inside_all_coverages) %>%
    rename(mean_exp_H_inside = mean_exp_H_inside_all_coverages) %>%
    mutate(mpa_coverage = seq(1,30,1)) %>%
    mutate(mpa_scenario = lit.scenarios.name[scenario])
  
  if (scenario == 1){
    Mean_diversity_outside_lit_scenarios = Mean_diversity_inside
  } else{
    Mean_diversity_outside_lit_scenarios = rbind(Mean_diversity_outside_lit_scenarios, Mean_diversity_inside)
  }
} 

write.csv(Mean_diversity_outside_lit_scenarios, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/Mean_diversity_outside_lit_scenarios.csv"), row.names = FALSE)

# Random scenarios
random.scenarios.name = c("Random-rep11", "Random-rep12", "Random-rep13", "Random-rep14", "Random-rep15", "Random-rep16", "Random-rep17", "Random-rep18", "Random-rep19", "Random-rep20")
number_random_scenarios = length(random.scenarios.name)

# Random scenarios inside MPAs
for (scenario in 1:number_random_scenarios){
  for (mpa_coverage in 1:Nbofreps){
    
    # open abundance
    nc_abundance_spatial  = nc_open(file.path(paste0(random.scenarios.dir[scenario], "/Spatial_mean/output_mpa_", mpa_coverage, "/Abundance_mpa_scenario.nc")))
    
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
    
    # MPA scenario and coverage %
    mpa = read.csv(paste0(random.mpa.mask.dir[scenario], "/", mpa_coverage, ".csv"), sep = ",", header=FALSE)
    mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
    mpa = t(as.matrix(mpa))
    
    exp_H_spatial_copy = exp_H_spatial
    exp_H_spatial_copy[mpa == 0] = NA # keep only inside MPAs
    
    # average exp_H between all cells in areas that will become MPAs (map before MPAs)
    mean_exp_H_inside = mean(exp_H_spatial_copy, na.rm = T)
    
    if (mpa_coverage == 1){
      mean_exp_H_inside_all_coverages = mean_exp_H_inside
    } else{
      mean_exp_H_inside_all_coverages = c(mean_exp_H_inside_all_coverages, mean_exp_H_inside)
    }
    
  }  
  Mean_diversity_inside = data.frame(mean_exp_H_inside_all_coverages) %>%
    rename(mean_exp_H_inside = mean_exp_H_inside_all_coverages) %>%
    mutate(mpa_coverage = seq(1,30,1)) %>%
    mutate(mpa_scenario = random.scenarios.name[scenario])
  
  if (scenario == 1){
    Mean_diversity_inside_random_scenarios = Mean_diversity_inside
  } else{
    Mean_diversity_inside_random_scenarios = rbind(Mean_diversity_inside_random_scenarios, Mean_diversity_inside)
  }
} 

write.csv(Mean_diversity_inside_random_scenarios, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/Mean_diversity_inside_random_basin_scenarios.csv"), row.names = FALSE)

# Random scenarios outside MPAs
for (scenario in 1:number_random_scenarios){
  for (mpa_coverage in 1:Nbofreps){
    
    # open abundance
    nc_abundance_spatial  = nc_open(file.path(paste0(random.scenarios.dir[scenario], "/Spatial_mean/output_mpa_", mpa_coverage, "/Abundance_mpa_scenario.nc")))
    
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
    
    # MPA scenario and coverage %
    mpa = read.csv(paste0(random.mpa.mask.dir[scenario], "/", mpa_coverage, ".csv"), sep = ",", header=FALSE)
    mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
    mpa = t(as.matrix(mpa))
    
    exp_H_spatial_copy = exp_H_spatial
    exp_H_spatial_copy[mpa > 0] = NA # keep only outside MPAs
    
    # average exp_H between all cells in areas that will become MPAs (map before MPAs)
    mean_exp_H_inside = mean(exp_H_spatial_copy, na.rm = T)
    
    if (mpa_coverage == 1){
      mean_exp_H_inside_all_coverages = mean_exp_H_inside
    } else{
      mean_exp_H_inside_all_coverages = c(mean_exp_H_inside_all_coverages, mean_exp_H_inside)
    }
    
  }  
  Mean_diversity_inside = data.frame(mean_exp_H_inside_all_coverages) %>%
    rename(mean_exp_H_inside = mean_exp_H_inside_all_coverages) %>%
    mutate(mpa_coverage = seq(1,30,1)) %>%
    mutate(mpa_scenario = random.scenarios.name[scenario])
  
  if (scenario == 1){
    Mean_diversity_outside_random_scenarios = Mean_diversity_inside
  } else{
    Mean_diversity_outside_random_scenarios = rbind(Mean_diversity_outside_random_scenarios, Mean_diversity_inside)
  }
} 

write.csv(Mean_diversity_outside_random_scenarios, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/Mean_diversity_outside_random_basin_scenarios.csv"), row.names = FALSE)

# EEZ random scenarios
random.scenarios.name = c("EEZ-conservation-rep1", "EEZ-conservation-rep2", "EEZ-conservation-rep3", "EEZ-conservation-rep4", "EEZ-conservation-rep5", "EEZ-conservation-rep6", "EEZ-conservation-rep7", "EEZ-conservation-rep8", "EEZ-conservation-rep9", "EEZ-conservation-rep10")
number_random_scenarios = length(random.scenarios.name)

# EEZ random scenarios inside MPAs
for (scenario in 1:number_random_scenarios){
  for (mpa_coverage in 1:Nbofreps){
    
    # open abundance
    nc_abundance_spatial  = nc_open(file.path(paste0(random.scenarios.dir[scenario+10], "/Spatial_mean/output_mpa_", mpa_coverage, "/Abundance_mpa_scenario.nc")))
    
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
    
    # MPA scenario and coverage %
    mpa = read.csv(paste0(random.mpa.mask.dir[scenario+10], "/", mpa_coverage, ".csv"), sep = ",", header=FALSE)
    mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
    mpa = t(as.matrix(mpa))
    
    exp_H_spatial_copy = exp_H_spatial
    exp_H_spatial_copy[mpa == 0] = NA # keep only inside MPAs
    
    # average exp_H between all cells in areas that will become MPAs (map before MPAs)
    mean_exp_H_inside = mean(exp_H_spatial_copy, na.rm = T)
    
    if (mpa_coverage == 1){
      mean_exp_H_inside_all_coverages = mean_exp_H_inside
    } else{
      mean_exp_H_inside_all_coverages = c(mean_exp_H_inside_all_coverages, mean_exp_H_inside)
    }
    
  }  
  Mean_diversity_inside = data.frame(mean_exp_H_inside_all_coverages) %>%
    rename(mean_exp_H_inside = mean_exp_H_inside_all_coverages) %>%
    mutate(mpa_coverage = seq(1,30,1)) %>%
    mutate(mpa_scenario = random.scenarios.name[scenario])
  
  if (scenario == 1){
    Mean_diversity_inside_random_scenarios = Mean_diversity_inside
  } else{
    Mean_diversity_inside_random_scenarios = rbind(Mean_diversity_inside_random_scenarios, Mean_diversity_inside)
  }
} 

write.csv(Mean_diversity_inside_random_scenarios, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/Mean_diversity_inside_random_EEZ_scenarios.csv"), row.names = FALSE)

# EEZ random scenarios outside MPAs
for (scenario in 1:number_random_scenarios){
  for (mpa_coverage in 1:Nbofreps){
    
    # open abundance
    nc_abundance_spatial  = nc_open(file.path(paste0(random.scenarios.dir[scenario+10], "/Spatial_mean/output_mpa_", mpa_coverage, "/Abundance_mpa_scenario.nc")))
    
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
    
    # MPA scenario and coverage %
    mpa = read.csv(paste0(random.mpa.mask.dir[scenario+10], "/", mpa_coverage, ".csv"), sep = ",", header=FALSE)
    mpa = mpa[order(nrow(mpa):1),] # reverse row order because 0 is bottom left in osmose
    mpa = t(as.matrix(mpa))
    
    exp_H_spatial_copy = exp_H_spatial
    exp_H_spatial_copy[mpa > 0] = NA # keep only outside MPAs
    
    # average exp_H between all cells in areas that will become MPAs (map before MPAs)
    mean_exp_H_inside = mean(exp_H_spatial_copy, na.rm = T)
    if (mpa_coverage == 1){
      mean_exp_H_inside_all_coverages = mean_exp_H_inside
    } else{
      mean_exp_H_inside_all_coverages = c(mean_exp_H_inside_all_coverages, mean_exp_H_inside)
    }
    
  }  
  Mean_diversity_inside = data.frame(mean_exp_H_inside_all_coverages) %>%
    rename(mean_exp_H_inside = mean_exp_H_inside_all_coverages) %>%
    mutate(mpa_coverage = seq(1,30,1)) %>%
    mutate(mpa_scenario = random.scenarios.name[scenario])
  
  if (scenario == 1){
    Mean_diversity_outside_random_scenarios = Mean_diversity_inside
  } else{
    Mean_diversity_outside_random_scenarios = rbind(Mean_diversity_outside_random_scenarios, Mean_diversity_inside)
  }
} 

write.csv(Mean_diversity_outside_random_scenarios, paste0(save_indicators_dir, "/", opt$CC_scenario, "/", opt$fishing_strategy, "/Mean_diversity_outside_random_EEZ_scenarios.csv"), row.names = FALSE)
