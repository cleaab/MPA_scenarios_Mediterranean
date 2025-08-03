# MPA_scenarios_Mediterranean
Data and scripts used for the analysis in Abello et al. (2024)
This repository contains data and code used to obtain results of the paper Abello et al.  (202?) - Design matters: Science-informed marine protected area networks outperform random and opportunistic designs submitted in NPJ Ocean Sustainability.

Model code and configuration to run the MPA scenarios are accessible on zenodo: 10.5281/zenodo.14039492

Raw model outputs used for the analyis exceeded 30 Terabytes and could not be uploaded on Zenodo, but aggregated outputs used for the analysis and the scripts to generate them are available in the data folder on Zenodo: 10.5281/zenodo.14039492

Aggregated outputs and other data used for the analysis is accessible on zenodo: 10.5281/zenodo.14039492

To run all the analysis:
1. Download zenodo repository and get data folder
2. Clone the git directory
3. Open the .Rproj file in RStudioRun and run the main file Main.Rmd

The code directory regroups functions used for:
- creating the MPA scenarios (mpa_scenarios)
- redistributing fishing effort according to the 3 redistribution strategies (fishing_redistribution_strategies)
- scripts to spatialize fishing effort using Synthetic Aperture Radar imagery (fishing_effort)
- scripts to calculate spatial metrics used for the Redundancy Analysis
- scripts to create figures



