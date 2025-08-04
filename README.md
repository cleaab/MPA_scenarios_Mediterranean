# MPA_scenarios_Mediterranean
This repository contains data and code used to obtain results of the paper Abello et al.  (202?) - Science-informed marine protected area networks outperform random and opportunistic designs submitted in NPJ Ocean Sustainability.

Model code and configuration to run the MPA scenarios are accessible on zenodo: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14039492.svg)](https://doi.org/10.5281/zenodo.14039492)

Raw model outputs used for the analyis exceeded 30 Terabytes and could not be uploaded on Zenodo, but are available upon request. Aggregated outputs used for the analysis and the scripts to generate them are available in the data folder on Zenodo: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14039492.svg)](https://doi.org/10.5281/zenodo.14039492)

To run all the analysis:
1. Clone the git directory
2. Open the .Rproj file in RStudioRun and run the main file Main.Rmd.
3. If you are not able to download data directory automatically from Main.Rmd, download it manually from the zenodo repository [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14039492.svg)](https://doi.org/10.5281/zenodo.14039492) and place it at the root of the repository.


The code directory regroups functions used for:
- creating the MPA scenarios (mpa_scenarios)
- redistributing fishing effort according to the 3 redistribution strategies (fishing_redistribution_strategies)
- scripts to spatialize fishing effort using Synthetic Aperture Radar imagery (fishing_effort)
- scripts to calculate spatial metrics used for the Redundancy Analysis
- scripts to calculate indicators from raw outputs (calculate_indicators)
- scripts to create figures (create_figures)

Scripts in this repository are also accessible on Zenodo: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16734084.svg)](https://doi.org/10.5281/zenodo.16734084)

## Data sources
* Synthetic Aperture Radar images from Sentinel-1 were downloaded from [Alaska Satellite Facility](https://search.asf.alaska.edu/#/)
* Location of offshore platforms used to spatialize fishing effort were downloaded from [EMODnet](https://www.emodnet-humanactivities.eu) ((European Marine Observation and Data network)
* Shapefiles of Mediterranean Geographical Sub-Areas (GSAs) were downloaded from the [GFCM-FAO](https://www.fao.org/gfcm/data/maps/gsas/es/)
* Wordlbound shapefiles and EEZ boundaries were retrieved from [Marineregions](https://www.marineregions.org/downloads.php)
* Shapefiles of Marine Protected Areas in the Mediterranean Sea were downloaded from [MAPAMED](https://www.mapamed.org/) (MAPAMED, the database of MArine Protected Areas in the MEDiterranean. 2019 edition, version 2.)
  
Main contact: Clea Abello [clea.abello@gmail.com](mailto:clea.abello@gmail.com)

```r
utils::sessionInfo()

R version 4.4.0 (2024-04-24 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
[4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

time zone: Europe/Paris
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] zenodor_0.1            data.table_1.15.4      landscapemetrics_2.1.4 ggcorrplot_0.1.4.1     scales_1.3.0           heatmaply_1.5.0       
 [7] viridis_0.6.5          plotly_4.10.4          vegan_2.6-8            lattice_0.22-6         permute_0.9-7          fields_16.2           
[13] viridisLite_0.4.2      spam_2.10-0            ncdf4_1.22             osmose_4.4.1.9035      terra_1.7-78           raster_3.6-26         
[19] sp_2.1-4               ggpubr_0.6.0           RColorBrewer_1.1-3     sf_1.0-16              cowplot_1.1.3          here_1.0.1            
[25] lubridate_1.9.3        forcats_1.0.0          stringr_1.5.1          dplyr_1.1.4            purrr_1.0.2            readr_2.1.5           
[31] tidyr_1.3.1            tibble_3.2.1           ggplot2_3.5.1          tidyverse_2.0.0       

loaded via a namespace (and not attached):
 [1] rstudioapi_0.16.0   jsonlite_1.8.8      magrittr_2.0.3      nloptr_2.0.3        rmarkdown_2.27      vctrs_0.6.5         minqa_1.2.7        
 [8] rstatix_0.7.2       webshot_0.5.5       htmltools_0.5.8.1   curl_5.2.1          broom_1.0.6         pracma_2.4.4        KernSmooth_2.23-22 
[15] htmlwidgets_1.6.4   desc_1.4.3          lifecycle_1.0.4     iterators_1.0.14    pkgconfig_2.0.3     optimx_2023-10.21   Matrix_1.7-0       
[22] R6_2.5.1            fastmap_1.2.0       digest_0.6.35       numDeriv_2016.8-1.1 colorspace_2.1-0    ps_1.7.6            rprojroot_2.0.4    
[29] pkgload_1.3.4       seriation_1.5.6     fansi_1.0.6         timechange_0.3.0    httr_1.4.7          abind_1.4-5         mgcv_1.9-1         
[36] compiler_4.4.0      proxy_0.4-27        intervals_0.15.5    remotes_2.5.0       withr_3.0.0         backports_1.5.0     calibrar_0.9.0.9008
[43] carData_3.0-5       DBI_1.2.3           dendextend_1.17.1   pkgbuild_1.4.4      R.utils_2.12.3      maps_3.4.2          ggsignif_0.6.4     
[50] MASS_7.3-60.2       classInt_0.4-10     tools_4.4.0         units_0.8-5         R.oo_1.26.0         glue_1.7.0          callr_3.7.6        
[57] nlme_3.1-164        grid_4.4.0          cluster_2.1.6       generics_0.1.3      gtable_0.3.5        tzdb_0.4.0          R.methodsS3_1.8.2  
[64] class_7.3-22        ca_0.71.1           hms_1.1.3           car_3.1-2           utf8_1.2.4          foreach_1.5.2       pillar_1.9.0       
[71] splines_4.4.0       tidyselect_1.2.1    registry_0.5-1      knitr_1.47          gridExtra_2.3       xfun_0.44           timeDate_4032.109  
[78] stringi_1.8.4       lazyeval_0.2.2      yaml_2.3.8          evaluate_0.24.0     codetools_0.2-20    cli_3.6.2           munsell_0.5.1      
[85] processx_3.8.4      Rcpp_1.0.12         parallel_4.4.0      assertthat_0.2.1    dotCall64_1.1-1     rlist_0.4.6.2       e1071_1.7-14       
[92] rlang_1.1.4         TSP_1.2-4 
```

