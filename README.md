# MPA_scenarios_Mediterranean
This repository contains data and code used to obtain results of the paper Abello et al.  (202?) - Science-informed marine protected area networks outperform random and opportunistic designs submitted in NPJ Ocean Sustainability.

Model code and configuration to run the MPA scenarios are accessible on zenodo: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14039492.svg)](https://doi.org/10.5281/zenodo.14039492)

Raw model outputs used for the analyis exceeded 30 Terabytes and could not be uploaded on Zenodo, but are available upon request. Aggregated outputs used for the analysis and the scripts to generate them are available in the data folder on Zenodo: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14039492.svg)](https://doi.org/10.5281/zenodo.14039492)

To run all the analysis:
1. Clone the git directory
1. Download zenodo repository and get data folder. Place the data folder in the root directory of the repository.
3. Open the .Rproj file in RStudioRun and run the main file Main.Rmd

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
