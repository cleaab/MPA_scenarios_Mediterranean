# 1. % change of taxonomic diversity inside reserves

# reference  
# Reference --------
# without mpa 
ncname     <- "/Abundance_nompa_scenario"  
ncfname    <- paste(here("Data/Simus_article1_GSA-prop_11-07-2023/Nompa"), ncname, ".nc", sep="") 
nc <- nc_open(ncfname)
abundance_ref <- ncvar_get(nc, varid = "Abundance") 

# extract inside MPAs
for (scen in 1:length(lit.scenarios.name)){
  for (i in 1:31){
    if (i == 1){
      abundance_inside_ref = abundance_ref
      N = apply(abundance_inside_ref, c(4), sum, na.rm = T)
      S = apply(abundance_inside_ref, c(3,4), sum, na.rm = T)
      
      abundance_inside_nompa = data.frame(N) %>%
        mutate(S = colSums(S!=0), rep = seq(1,30,1))
      
      abundance_inside_nompa = cbind(abundance_inside_nompa, t(S))
      colnames(abundance_inside_nompa) = c("N", "S", "rep", species_sorted$species_name)
      
      abundance_inside_nompa_df = abundance_inside_nompa %>%
        pivot_longer(cols = 4:104, names_to = "species", values_to = "abundance_by_species") %>%
        mutate(p = abundance_by_species/N) %>%
        filter(p > 0) %>%
        group_by(rep) %>%
        mutate(H = -sum(p*log(p))) %>%
        mutate(exp_H = exp(H)) %>%
        distinct(N, S, H, exp_H) %>%
        mutate(mpa = "Inside MPAs", mpa_coverage = i-1)
      
    } else{
      mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/", i-1, ".csv"), sep = ",", header=FALSE)
      mpa = mpa.file[order(nrow(mpa.file):1),] # reverse row order because 0 is bottom left in osmose
      mpa = t(as.matrix(mpa))
      
      abundance_inside_ref = abundance_ref
      abundance_inside_ref[mpa!=1] = NA
      N = apply(abundance_inside_ref, c(4), sum, na.rm = T)
      S = apply(abundance_inside_ref, c(3,4), sum, na.rm = T)
      
      abundance_inside_nompa = data.frame(N) %>%
        mutate(S = colSums(S!=0), rep = seq(1,30,1))
      
      abundance_inside_nompa = cbind(abundance_inside_nompa, t(S))
      colnames(abundance_inside_nompa) = c("N", "S", "rep", species_sorted$species_name)
      
      abundance_inside_nompa_df = abundance_inside_nompa %>%
        pivot_longer(cols = 4:104, names_to = "species", values_to = "abundance_by_species") %>%
        mutate(p = abundance_by_species/N) %>%
        filter(p > 0) %>%
        group_by(rep) %>%
        mutate(H = -sum(p*log(p))) %>%
        mutate(exp_H = exp(H)) %>%
        distinct(N, S, H, exp_H) %>%
        mutate(mpa = "Inside MPAs", mpa_coverage = i-1)
    }
    
    if (i == 1){
      Diversity_ref_lit = abundance_inside_nompa_df
    } else{
      Diversity_ref_lit = rbind(Diversity_ref_lit, abundance_inside_nompa_df)
    }
    
  }
  Diversity_ref_lit$scenario = lit.scenarios.name[scen]
  
  if (scen == 1){
    Diversity_ref_lit_all_inside = Diversity_ref_lit
  } else{
    Diversity_ref_lit_all_inside = rbind(Diversity_ref_lit_all_inside, Diversity_ref_lit)
    
  }
}

write.csv(Diversity_ref_lit_all_inside, here("Data/Simus_article1_GSA-prop_11-07-2023/Nompa/Diversity_ref_lit_all_inside.csv"), row.names = FALSE)


for (scen in 1:length(random.scenarios.name)){
  for (i in 1:31){
    
    if (i == 1){
      abundance_inside_ref = abundance_ref
      N = apply(abundance_inside_ref, c(4), sum, na.rm = T)
      S = apply(abundance_inside_ref, c(3,4), sum, na.rm = T)
      
      abundance_inside_nompa = data.frame(N) %>%
        mutate(S = colSums(S!=0), rep = seq(1,30,1))
      
      abundance_inside_nompa = cbind(abundance_inside_nompa, t(S))
      colnames(abundance_inside_nompa) = c("N", "S", "rep", species_sorted$species_name)
      
      abundance_inside_nompa_df = abundance_inside_nompa %>%
        pivot_longer(cols = 4:104, names_to = "species", values_to = "abundance_by_species") %>%
        mutate(p = abundance_by_species/N) %>%
        filter(p > 0) %>%
        group_by(rep) %>%
        mutate(H = -sum(p*log(p))) %>%
        mutate(exp_H = exp(H)) %>%
        distinct(N, S, H, exp_H) %>%
        mutate(mpa = "Inside MPAs", mpa_coverage = i-1)
    } else{
      mpa.file = read.csv(paste0(random.mpa.mask.dir[scen], "/", i-1, ".csv"), sep = ",", header=FALSE)
      mpa = mpa.file[order(nrow(mpa.file):1),] # reverse row order because 0 is bottom left in osmose
      mpa = t(as.matrix(mpa))
      
      abundance_inside_ref = abundance_ref
      abundance_inside_ref[mpa!=1] = NA
      N = apply(abundance_inside_ref, c(4), sum, na.rm = T)
      S = apply(abundance_inside_ref, c(3,4), sum, na.rm = T)
      
      abundance_inside_nompa = data.frame(N) %>%
        mutate(S = colSums(S!=0), rep = seq(1,30,1))
      
      abundance_inside_nompa = cbind(abundance_inside_nompa, t(S))
      colnames(abundance_inside_nompa) = c("N", "S", "rep", species_sorted$species_name)
      
      abundance_inside_nompa_df = abundance_inside_nompa %>%
        pivot_longer(cols = 4:104, names_to = "species", values_to = "abundance_by_species") %>%
        mutate(p = abundance_by_species/N) %>%
        filter(p > 0) %>%
        group_by(rep) %>%
        mutate(H = -sum(p*log(p))) %>%
        mutate(exp_H = exp(H)) %>%
        distinct(N, S, H, exp_H) %>%
        mutate(mpa = "Inside MPAs", mpa_coverage = i-1)
    }
    
    if (i == 1){
      Diversity_ref_random_EEZ = abundance_inside_nompa_df
    } else{
      Diversity_ref_random_EEZ = rbind(Diversity_ref_random_EEZ, abundance_inside_nompa_df)
    }
    
  }
  Diversity_ref_random_EEZ$scenario = random.scenarios.name[scen]
  
  if (scen == 1){
    Diversity_ref_random_EEZ_all_inside = Diversity_ref_random_EEZ
  } else{
    Diversity_ref_random_EEZ_all_inside = rbind(Diversity_ref_random_EEZ_all_inside, Diversity_ref_random_EEZ)
    
  }
}

write.csv(Diversity_ref_random_EEZ_all_inside, here("Data/Simus_article1_GSA-prop_11-07-2023/Nompa/Diversity_ref_random_EEZ_all_inside.csv"), row.names = FALSE)

# Outside ----------------------------------------------------------------------


for (scen in 1:length(lit.scenarios.name)){
  for (i in 1:30){
    
    mpa.file = read.csv(paste0(lit.mpa.mask.dir[scen], "/", i, ".csv"), sep = ",", header=FALSE)
    mpa = mpa.file[order(nrow(mpa.file):1),] # reverse row order because 0 is bottom left in osmose
    mpa = t(as.matrix(mpa))
    
    abundance_inside_ref = abundance_ref
    abundance_inside_ref[mpa==1] = NA
    N = apply(abundance_inside_ref, c(4), sum, na.rm = T)
    S = apply(abundance_inside_ref, c(3,4), sum, na.rm = T)
    
    abundance_inside_nompa = data.frame(N) %>%
      mutate(S = colSums(S!=0), rep = seq(1,30,1))
    
    abundance_inside_nompa = cbind(abundance_inside_nompa, t(S))
    colnames(abundance_inside_nompa) = c("N", "S", "rep", species_sorted$species_name)
    
    abundance_inside_nompa_df = abundance_inside_nompa %>%
      pivot_longer(cols = 4:104, names_to = "species", values_to = "abundance_by_species") %>%
      mutate(p = abundance_by_species/N) %>%
      filter(p > 0) %>%
      group_by(rep) %>%
      mutate(H = -sum(p*log(p))) %>%
      mutate(exp_H = exp(H)) %>%
      distinct(N, S, H, exp_H) %>%
      mutate(mpa = "Outside MPAs", mpa_coverage = i)
    
    if (i == 1){
      Diversity_ref_lit = abundance_inside_nompa_df
    } else{
      Diversity_ref_lit = rbind(Diversity_ref_lit, abundance_inside_nompa_df)
    }
    
  }
  Diversity_ref_lit$scenario = lit.scenarios.name[scen]
  
  if (scen == 1){
    Diversity_ref_lit_all_outside = Diversity_ref_lit
  } else{
    Diversity_ref_lit_all_outside = rbind(Diversity_ref_lit_all_outside, Diversity_ref_lit)
    
  }
}

write.csv(Diversity_ref_lit_all_outside, here("Data/Simus_article1_GSA-prop_11-07-2023/Nompa/Diversity_ref_lit_all_outside.csv"), row.names = FALSE)

for (scen in 1:length(random.scenarios.name)){
  for (i in 1:30){
    
    mpa.file = read.csv(paste0(random.mpa.mask.dir[scen], "/", i, ".csv"), sep = ",", header=FALSE)
    mpa = mpa.file[order(nrow(mpa.file):1),] # reverse row order because 0 is bottom left in osmose
    mpa = t(as.matrix(mpa))
    
    abundance_inside_ref = abundance_ref
    abundance_inside_ref[mpa==1] = NA
    N = apply(abundance_inside_ref, c(4), sum, na.rm = T)
    S = apply(abundance_inside_ref, c(3,4), sum, na.rm = T)
    
    abundance_inside_nompa = data.frame(N) %>%
      mutate(S = colSums(S!=0), rep = seq(1,30,1))
    
    abundance_inside_nompa = cbind(abundance_inside_nompa, t(S))
    colnames(abundance_inside_nompa) = c("N", "S", "rep", species_sorted$species_name)
    
    abundance_inside_nompa_df = abundance_inside_nompa %>%
      pivot_longer(cols = 4:104, names_to = "species", values_to = "abundance_by_species") %>%
      mutate(p = abundance_by_species/N) %>%
      filter(p > 0) %>%
      group_by(rep) %>%
      mutate(H = -sum(p*log(p))) %>%
      mutate(exp_H = exp(H)) %>%
      distinct(N, S, H, exp_H) %>%
      mutate(mpa = "Outside MPAs", mpa_coverage = i)
    
    if (i == 1){
      Diversity_ref_random_EEZ = abundance_inside_nompa_df
    } else{
      Diversity_ref_random_EEZ = rbind(Diversity_ref_random_EEZ, abundance_inside_nompa_df)
    }
    
  }
  Diversity_ref_random_EEZ$scenario = random.scenarios.name[scen]
  
  if (scen == 1){
    Diversity_ref_random_EEZ_all_outside = Diversity_ref_random_EEZ
  } else{
    Diversity_ref_random_EEZ_all_outside = rbind(Diversity_ref_random_EEZ_all_outside, Diversity_ref_random_EEZ)
    
  }
}

write.csv(Diversity_ref_random_EEZ_all_outside, here("Data/Simus_article1_GSA-prop_11-07-2023/Nompa/Diversity_ref_random_EEZ_all_outside.csv"), row.names = FALSE)
