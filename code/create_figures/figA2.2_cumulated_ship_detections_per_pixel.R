#' Figure appendix A2.2
#'
#' 
#' @param SAR_data_noRFI output from filter_RFI function
#' 
#' @return In the figures/RFI folder, plot showing how much observations we lose according to ships thresholds, and also map iwth/without shipping routes
#'
#' @export
#' 


shipping_plot = function(shipping_routes, sar_all_cleaned, save = TRUE){

  
  #Plot to show the threshold needed for ships
  raster_sf = raster::extract(shipping_routes, sar_all_cleaned, df = T, weights = F)
  
  output = data.frame()
  
  for(i in 1:50){
    
    temp_forloop = cbind(sar_all_cleaned, raster_sf) %>% filter(shipping < i)
    
    num_ships= data.frame(num_ships = i, obs = nrow(temp_forloop))
    
    output = rbind(num_ships,output )
    
  }
  
  ggplot(output, aes(num_ships,obs)) +
    geom_point()+
    labs(x = "Density of detections per pixel",
         y = "Number of detections after filtering out shipping routes")+
    geom_vline(xintercept = 20) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14))
  
  if (save == TRUE){
    ggsave(here("figures/appendix/figA2.2_data_lost_ships.png"), width = 7, height = 6, dpi = 300)
  }
}

  
