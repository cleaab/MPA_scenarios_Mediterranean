#' Plot every image we have to check for RFI
#'
#' 
#' @param data  Output from the 4_filter_RFI
#' 
#' @return Dataframe with SAR observations but without shipping routes
#'
#' @export
#' 

library(raster)


filter_shipping = function(data_clean, shipping_routes){
  
  st_crs(data_clean)==st_crs(shipping_routes)
  raster_sf = raster::extract(shipping_routes, data_clean, df = T, weights = F)
  data_filter_shipping = cbind(data_clean, raster_sf) %>% filter(shipping < 20)
  

   
  return(data_filter_shipping)
  
}

