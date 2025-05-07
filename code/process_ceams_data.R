################################################################
# This script cleans and prepares continuous PM2.5 data from CEAMS server for plotting via Shiny web application.
################################################################

# Called in automation file AutomateApp.bat part I
  # processes data from get_ceams_data

library(dplyr)
library(lubridate)
source("get_ceams_data.R")

# List of device IDs
device_ids <- c(
  # Pretoria, South Africa
  'AD00103', 'AD00104', 'AD00405',
  # Taichung, Taiwan
  'AD00094',
  # Tainan, Taiwan
  'AD00092','AD00101','AD00403','AD00406',
  # Sonipat, India
  'AD00096', 'AD00415', 'AD00416',
  # Amity University Noida, India (88 starting in 2024)
  'AD00088','AD00090','AD00413','AD00414',
  # Amity University Manesar, India
  'AD00408','AD00409',
  # Adama, Ethiopia
  'AD00410','AD00412',
  #AASTU Addis Ababa, Ethiopia
  'AD00091','AD00098','AD00093',
  #JPL
  'AD00402', 
  #Atlanta early November
  'AD00417') 

all_data <- get_ceams_data(device_ids) #call function in get_ceams_data.R

# Keep only the specified columns
all_data <- all_data %>%
  select(cartridge_id, latitude, longitude, sn, timestamp_utc, pm25, fdpdp, volumetric_flowrate, pump_rh, pump_t)  

all_data <- rename(all_data, 
                 lat = latitude,
                 lng = longitude,
                 device = sn,
                 time = timestamp_utc,
                 filter = cartridge_id,
                 flow = volumetric_flowrate,
                 pressure = fdpdp,
                 rh = pump_rh,
                 t = pump_t,
                 pm = pm25)
  
all_data <- all_data %>%
    filter(!is.na(time)) %>%
    filter(pm > 0) %>%
    filter(!is.na(pm)) %>%
    filter(pm < quantile(pm, 0.9999))

all_data$time <- as.POSIXct(all_data$time, format= "%Y-%m-%dT%H:%M")
  
start_size = nrow(all_data) #size of all data pre-process

#Group location by device ID ------------------


  pretoria <- all_data %>%
    filter(device == 'AD00103' | device == 'AD00104' | device == 'AD00405' )%>%
    mutate(location = 'Pretoria, South Africa')
  pretoria_recent <- pretoria %>%
    filter(time >= Sys.Date() - 14)%>%
    mutate(location = 'Pretoria, South Africa')
  
  taichung <- all_data %>%
    filter(device == 'AD00094')%>%
    mutate(location = 'Taichung, Taiwan')
  taichung_recent <- taichung %>%
    filter(time >= Sys.Date() - 14)%>%
    mutate(location = 'Taichung, Taiwan')
  
  tainan <- all_data %>%
    filter(device == 'AD00092' | device == 'AD00101' | device == 'AD00403' | device == 'AD00406')%>%
    mutate(location = 'Tainan, Taiwan')
  tainan_recent <- tainan %>%
    filter(time >= Sys.Date() - 14)%>%
    mutate(location = 'Tainan, Taiwan')
  
  sonipat <- all_data %>%
    filter(device == 'AD00096' | device == 'AD00415' | device == 'AD00416')%>%
    mutate(location = 'Sonipat, India')
  sonipat_recent <- sonipat %>%
    filter(time >= Sys.Date() - 14)%>%
    mutate(location = 'Sonipat, India')
  
  noida <- all_data %>%
    filter(device == 'AD00088' | device == 'AD00090' | device == 'AD00413' | device == 'AD00414')%>%
    mutate(location = 'Amity University Noida, India')
  noida_recent <- noida %>%
    filter(time >= Sys.Date() - 14)%>%
    mutate(location = 'Amity University Noida, India')
  
  manesar <- all_data %>%
    filter(device == 'AD00408' | device == 'AD00409')%>%
    mutate(location = 'Amity University Manesar, India')
  manesar_recent <- manesar %>%
    filter(time >= Sys.Date() - 14)%>%
    mutate(location = 'Amity University Manesar, India')
  
  adama <- all_data %>%
    filter(device == 'AD00410' | device == 'AD00412')%>%
    mutate(location = 'Adama, Ethiopia')
  adama_recent <- adama %>%
    filter(time >= Sys.Date() - 14)%>%
    mutate(location = 'Adama, Ethiopia')
  
  aastu <- all_data %>%
    filter(device == 'AD00091' | device == 'AD00098' | device == 'AD00093')%>%
    mutate(location = 'AASTU, Ethiopia')
  aastu_recent <- aastu %>%
    filter(time >= Sys.Date() - 14)%>%
    mutate(location = 'AASTU, Ethiopia')
  
  jpl <- all_data %>%
    filter(device == 'AD00402') %>%
    mutate(location = 'JPL')  
  jpl_recent <- jpl %>%
    filter(time >= Sys.Date() - 14)%>%
    mutate(location = 'JPL')
  
  atl <- all_data %>%
    filter(device == 'AD00417') %>%
    mutate(location = 'Atlanta')  
  atl_recent <- atl %>%
    filter(time >= Sys.Date() - 14)%>%
    mutate(location = 'Atlanta')
  

all_recent_df <- rbind(pretoria_recent, taichung_recent, tainan_recent, sonipat_recent, noida_recent, manesar_recent, adama_recent, aastu_recent, jpl_recent, atl_recent)
all_df <- rbind(pretoria, taichung, tainan, sonipat, noida, manesar, adama, aastu, jpl, atl) 

#write all data tables
write.csv(all_recent_df, "all_recent_data.csv", row.names = FALSE) #for plot
write.csv(all_df, "all_data.csv", row.names = FALSE) #all data 

#Run this if not pulling from get_ceams_data
#all_data <- read.csv("all_data.csv") #comment
#summary statistics:

#24-hour groups for recent data -----------------------------

all_recent_df <- all_recent_df %>%
  group_by(location, device, filter) %>% # group by location, device ID, filter ID
  mutate(
    start_time = min(time), #start time for cartridge ID
    latest_time = max(time), #end time for cartridge ID
    total_run_time = as.numeric(difftime(latest_time, start_time, units = "hours")), #total run time for cartridge ID
    elapsed_time = as.numeric(difftime(time, start_time, units = "hours")) #elapsed time of current row for cartridge ID
  ) %>%
  mutate(interval = (elapsed_time %/% 24) + 1) %>% #create index for number of 24-hours periods (1-based)
  ungroup()

recent_daily <- all_recent_df %>% #create stats by interval
  group_by(location, device, filter, interval) %>% #for each unique 24-hour interval by device...
  
  reframe(    start_time = min(time),
              end_time = max(time),
              n_readings = n(),
              elapsed_time = difftime(end_time, start_time, units = c("hours")),
              daily_mean_pm = mean(pm),
              daily_95_pm = quantile(pm, .95),
              daily_mean_rh = mean(rh),
              daily_mean_t = mean(t))

recent_daily <- recent_daily %>%
  mutate(
    day = as.Date(start_time),
    across(c('elapsed_time', 'daily_mean_pm', 'daily_95_pm', 'daily_mean_rh', 'daily_mean_t'), round, 1)) %>%
  select(location, device, filter, day, elapsed_time, n_readings, daily_mean_pm, daily_95_pm, daily_mean_rh, daily_mean_t)%>%
  arrange(desc(day))

#24-hour groups for ALL data -----------------------------
all_df <- all_df %>%
  group_by(location, device, filter) %>% # group by location, device ID, filter ID
  mutate(
    start_time = min(time), #start time for cartridge ID
    latest_time = max(time), #end time for cartridge ID
    total_run_time = as.numeric(difftime(latest_time, start_time, units = "hours")), #total run time for cartridge ID
    elapsed_time = as.numeric(difftime(time, start_time, units = "hours")) #elapsed time of current row for cartridge ID
  ) %>%
  mutate(interval = (elapsed_time %/% 24) + 1) %>% #create index for number of 24-hours periods (1-based)
  ungroup()

all_daily <- all_df %>% #create stats by interval
  group_by(location, device, filter, interval) %>% #for each unique 24-hour interval by device...
  
  reframe(    start_time = min(time),
              end_time = max(time),
              n_readings = n(),
              elapsed_time = difftime(end_time, start_time, units = c("hours")),
              daily_mean_pm = mean(pm),
              daily_95_pm = quantile(pm, .95),
              daily_mean_rh = mean(rh),
              daily_mean_t = mean(t))

all_daily <- all_daily %>%
  mutate(
    day = as.Date(start_time),
    across(c('elapsed_time', 'daily_mean_pm', 'daily_95_pm', 'daily_mean_rh', 'daily_mean_t'), round, 1)) %>%
  select(location, device, filter, day, elapsed_time, n_readings, daily_mean_pm, daily_95_pm, daily_mean_rh, daily_mean_t)%>%
  arrange(desc(day))


#write daily tables
write.csv(recent_daily, "recent_daily.csv", row.names = FALSE) #for table
write.csv(all_daily, "all_daily.csv", row.names = FALSE)
   


