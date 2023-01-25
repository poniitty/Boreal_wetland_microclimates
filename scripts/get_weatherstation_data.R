library(tidyverse)
library(lubridate)
# remotes::install_github("rOpenGov/fmi2", lib = "/projappl/project_2003061/Rpackages")
library(fmi2, lib.loc = "/projappl/project_2003061/Rpackages")
library(sf)


mindate <- min(read_csv("data/tomst_data/tomst_data_daily.csv", show_col_types = FALSE)$date) - days(14)
maxdate <- max(read_csv("data/tomst_data/tomst_data_daily.csv", show_col_types = FALSE)$date)

station_data <- read_delim("data/fmi/fmi_stations.csv", delim = ";", locale = locale(encoding = "UTF-8"))
station_xy <- st_as_sf(station_data, coords = c("Lon","Lat"), crs = 4326) %>% 
  st_transform(crs = 32635)

#############################################################################

p <- st_read("data/geo_data/points_all.gpkg") %>% 
  st_transform(crs = 32635)

# Check which stations are within 50 km of our study sites
station_xy$within25km <- unlist(lapply(st_is_within_distance(station_xy, p, 25000), function(x) !is_empty(x)))

station_xy %>% filter(within25km) %>% 
  filter(grepl("weather", Groups, ignore.case = T)) -> selected_stations

plot(st_geometry(selected_stations))

####################################################################################
# DOWNLOAD DATA

# Daily

# Empty dataframe or previous data
if(file.exists("data/fmi/fmi_daily.csv")){
  df <- as_tibble(read.csv("data/fmi/fmi_daily.csv")) %>% 
    mutate(time = as_date(time))
} else {
  df <- data.frame()
}

if(file.exists("data/fmi/fmi_daily.csv")){
  startdate <- max(df$time)
} else {
  startdate <- mindate
}
enddate <- maxdate

days <- seq(as_date(mindate), as_date(enddate), by  = "month")

for(i in 1:NROW(selected_stations)){
  # i <- 2
  print(selected_stations$Name[i])
  
  for(ii in days){
    # ii <- "2016-11-01"
    print(as_date(ii))
    
    tryCatch({
      d <- fmi2::obs_weather_daily(starttime = as_date(ii),
                                   endtime = (as_date(ii) + months(1)) - 1,
                                   fmisid = selected_stations$FMISID[i])
      d %>% st_drop_geometry() %>% 
        filter(complete.cases(.)) %>% 
        pivot_wider(names_from = "variable", values_from = "value") %>% 
        mutate(FMISID = selected_stations$FMISID[i]) -> d
      
      df <- bind_rows(df, d)
    }, error = function(e) { print("NO DATA FOR THESE DATES") })
    #Sys.sleep(1)
  }
}

df %>% filter(!duplicated(df %>% select(time, FMISID), fromLast = T)) -> df

# Station summary

df %>% group_by(FMISID) %>% 
  summarise(first_date = min(time),
            last_date = max(time),
            tdays = sum(!is.na(tday)),
            rrdays = sum(!is.na(rrday))) -> aggr

selected_stations <- full_join(selected_stations, aggr)

bind_cols(p, p %>% st_coordinates() %>% as.data.frame()) %>% 
  st_drop_geometry() %>% 
  group_by(area) %>% 
  summarise(X = mean(X),
            Y = mean(Y)) %>% 
  st_as_sf(., coords = c("X","Y"), crs = st_crs(p)) -> centroids


selected_stations %>% 
  mutate(HYY = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "HYY"))/1000),1),
         TII = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "TII"))/1000),1),
         VAR = round(as.numeric(st_distance(selected_stations, centroids %>% filter(area == "VAR"))/1000),1)) -> selected_stations


selected_stations$closest_area <- names(selected_stations %>% select(HYY:VAR) %>% st_drop_geometry())[apply(selected_stations %>% select(HYY:VAR) %>% st_drop_geometry(), 1, function(x) which.min(x))]

bind_cols(selected_stations, selected_stations %>% st_coordinates() %>% as.data.frame()) %>% 
  st_drop_geometry() %>% 
  select(-Groups,-within25km) %>% 
  relocate(X, Y, .after = Elevation) %>% 
  relocate(closest_area, .after = rrdays) %>% 
  rename(x_utm35 = X,
         y_utm35 = Y) -> selected_stations_df

df <- left_join(df, selected_stations_df %>% select(FMISID, closest_area))

write_csv(df, "data/fmi/fmi_daily.csv")
write_csv(selected_stations_df, "data/fmi/fmi_stations.csv")

