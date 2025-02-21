#' Figure Appendix A4.2
#'
#' 
#' @param MAPAMED database and OSMOSE-MED grid
#' 
#' @return Current MPA network map used as basline for S1 scenario
#'
#' @export
#' 


# Expansion of current MPA network ---------------------------------------------

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DATA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Med Sea ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Import Med grid (each cell = 1 polygon)
Med_grid = st_read(here("data/MPA_scenarios/QGIS/Med_polygon.gpkg"))
Med_grid_sea = subset(Med_grid, Med_grid$VALUE == 0)
Med_grid_area = set_units(sum(st_area(Med_grid_sea$geom)), m^2)
Med_grid_union = st_union(Med_grid_sea$geom)

## ~~~~~~~~~~~~~~~~~~~~~~ MPA data - MAPAMED ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# Import database
MAPAMED <- st_read(here("data/MPA_scenarios/QGIS/MAPAMED_2019_v2_spatial_data_epsg3035.gpkg"))

# Remove terrestrial parts of partially MPAs
MAPAMED_marine = st_intersection(MAPAMED,Med_grid_union)

FRA_deepsea = MAPAMED_marine %>%
  filter(DESIG_CAT_ENG == "Fisheries Restricted Area (deep sea)")
FRA_deepsea$Coverage_pct = st_area(FRA_deepsea$geom)/Med_grid_area * 100

# Selection of categories to keep
MPA = MAPAMED_marine[,1:33] # 8 is erastothenes seamount

# keep only areas - purpose towards fishing
MPA = MAPAMED_marine %>%
  dplyr::select(1:33) %>%
  filter(DESIG_CAT_ENG %in% c("MPA with a national statute", "Marine Natura 2000 site", "Fisheries Restricted Area (VME)", "Fisheries Restricted Area (EFH)")) %>%
  filter(!NAME == "Corredor de Migracion de Cetaceos del Mediterraneo")

MPA_union = st_union(MPA$geom) #remove overlaps
plot(MPA_union)


#st_write(MPA_union, "Conservation_plans_masks_qgislayers/Existing_network/Baseline_MPA_network_union.shp") # used in QGIS for overlap
#st_write(MPA, "Conservation_plans_masks_qgislayers/Existing_network/Baseline_MPA_network.shp")

# Plot
fig.A4.2.current.network <- ggplot() +
  geom_sf(data = Med_grid_union, fill = "lightblue", colour = "white", alpha = 0.5) +
  geom_sf(data = MPA$geom, aes(fill = MPA$DESIG_CAT_ENG)) +
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "#000000", fill=NA, size=1),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(family = "Helvetica", size = (12)))  

ggsave(here("figures/appendix/figA4.2_current_MPA_network.png"), dpi = 300)


# Calculate median and mean sizes of MPAs
MPA_nogeom = MPA %>%
  st_drop_geometry()

mean(MPA_nogeom$REP_M_AREA, na.rm = T)
median(MPA_nogeom$REP_M_AREA, na.rm = T)
quantile(MPA_nogeom$REP_M_AREA, na.rm = T)

# Calculate coverage of baseline network
MPA_area = set_units(st_area(MPA_union), m^2)
MPA_coverage = set_units(MPA_area/Med_grid_area * 100, NULL)
print(MPA_coverage)
