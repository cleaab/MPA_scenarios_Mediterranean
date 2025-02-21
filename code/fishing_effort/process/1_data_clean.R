#' Cleans up data of SAR observations, removing outliers false positives etc
#'
#' 
#' @param data  
#' 
#' @return Dataframe with cleaned data
#'
#' @export
#' 



data_clean = function(SAR_data){
  
  #Remove outliers by deleting all days where number of observations is greater than 95% of observed values
  data_subset = SAR_data %>% 
    #Number of observations per image
    group_by(detecttime) %>%
    mutate(ObsImg  = n()) %>%
    ungroup()
  
  SAR_dataa_nooutliers =  filter(data_subset, ObsImg < quantile(data_subset$ObsImg,0.95))
  

  #Convert observations to sf object with same CRS as Med data
  SAR_data_sf = st_as_sf(SAR_dataa_nooutliers, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", coords=c("lon","lat"), remove = FALSE)
  
  
  #General informations on data
  data_clean = SAR_data_sf %>%
    #Delete false positives
    filter(reliability != 3) %>%
    #General temporal info on data
    mutate(Year = as.factor(ifelse(str_detect(detecttime,"2019"),"2019",ifelse(str_detect(detecttime,"2018"),"2018","2017"))),
           Month = as.factor(ifelse(str_detect(detecttime, "-01-"), "January", 
                                    ifelse(str_detect(detecttime, "-02-"), "February", 
                                           ifelse(str_detect(detecttime,"-03-"),"March",
                                                  ifelse(str_detect(detecttime,"-04-"),"April",
                                                         ifelse(str_detect(detecttime,"-05-"),"May",
                                                                ifelse(str_detect(detecttime,"-06-"),"June",
                                                                       ifelse(str_detect(detecttime,"-07-"),"July",
                                                                              ifelse(str_detect(detecttime,"-08-"),"August",
                                                                                     ifelse(str_detect(detecttime,"-09-"),"September",
                                                                                            ifelse(str_detect(detecttime,"-10-"),"October",
                                                                                                   ifelse(str_detect(detecttime,"-11-"),"November",
                                                                                                          "December")))))))))))),
           MonthNum = as.factor(ifelse(str_detect(detecttime, "-01-"), "01",
                                       ifelse(str_detect(detecttime, "-02-"), "02",
                                              ifelse(str_detect(detecttime,"-03-"),"03",
                                                     ifelse(str_detect(detecttime,"-04-"),"04",
                                                            ifelse(str_detect(detecttime,"-05-"),"05",
                                                                     ifelse(str_detect(detecttime,"-06-"),"06",
                                                                            ifelse(str_detect(detecttime,"-07-"),"07",
                                                                                   ifelse(str_detect(detecttime,"-08-"),"08",
                                                                                          ifelse(str_detect(detecttime,"-09-"),"09",
                                                                                                 ifelse(str_detect(detecttime,"-10-"),"10",
                                                                                                        ifelse(str_detect(detecttime,"-11-"),"11","12")))))))))))),
           Day = as.factor(gsub( " .*$", "", detecttime))) %>%
    
    #Number of shots taken for each day
    group_by(Day) %>%
    mutate(Img = n_distinct(detecttime)) %>%
    ungroup() %>%
    #Number of shots taken each Month
    group_by(Month) %>%
    mutate(ImgMonth = n_distinct(detecttime)) %>% 
    ungroup()  %>%
    #Number of shots taken each year
    group_by(Year) %>%
    mutate(ImgYear = n_distinct(detecttime)) %>%
    ungroup() %>%
    #And number of observations per day 
    group_by(Day) %>%
    mutate(ObsDay = n()) %>%
    ungroup()
  
    data_clean$Time = format(as.POSIXct(data_clean$detecttime), format = "%H:%M:%S")
  
  return(data_clean)
  
  
}



