##################Functions to calculate global emissis for different process over different time slices#######################################
##################Houghton book keepig model###########################
global_deforestation_timeSlice_HN <- function(data, startYear, endYear) {
  data %>%
    filter(.data$Year >= startYear &  .data$Year <= endYear & (.data$Process == "FP" | .data$Process == "FC" |
                                                                 .data$Process == "FCO")) %>%
    summarise(CO2tot=(sum(.data$CO2, na.rm=TRUE))/((endYear-startYear)+1))
}
global_oLandUse_timeSlice_HN <- function(data, startYear, endYear) {
  data %>%
    filter(.data$Year >= startYear &  .data$Year <= endYear &  
             (.data$Process == "CP" | .data$Process == "OC" | .data$Process == "OP" | .data$Process == "FIRE" |
                .data$Process == "PLANT" )) %>%
    summarise(CO2tot=(sum(.data$CO2, na.rm=TRUE))/((endYear-startYear)+1))
}
global_forestry_timeSlice_HN <- function(data, startYear, endYear) {
  data %>%
    filter(.data$Year >= startYear &  .data$Year <= endYear &  
             (.data$Process == "FUEL" | .data$Process == "IND")) %>%
    summarise(CO2tot=(sum(.data$CO2, na.rm=TRUE))/((endYear-startYear)+1))
}

#################################FAOSTAT######################
global_deforestation_timeSlice_FAO <- function(data, startYear, endYear) {
  data %>%
    ungroup() %>%
    filter(.data$Year >= startYear &  .data$Year <= endYear & .data$Process == "Net Forest conversion") %>%
    summarise(CO2tot=(sum(.data$CO2, na.rm=TRUE))/((endYear-startYear)+1))
}
global_forest_timeSlice_FAO <- function(data, startYear, endYear) {
  data %>%
    ungroup() %>%
    filter(.data$Year >= startYear &  .data$Year <= endYear & .data$Process == "Forest land") %>%
    summarise(CO2tot=(sum(.data$CO2, na.rm=TRUE))/((endYear-startYear)+1))
}
global_oLandUse_timeSlice_FAO <- function(data, startYear, endYear) {
  data %>%
    filter(.data$Year >= startYear &  .data$Year <= endYear &  
             (.data$Process == "Burning Biomass" | .data$Process == "Cropland" |
                .data$Process == "Grassland")) %>%
    summarise(CO2tot=(sum(.data$CO2, na.rm=TRUE))/((endYear-startYear)+1))
}


