################################################################
# This script cleans and prepares gravimetric PM2.5 data from GitHub (manual upload) for plotting via Shiny web application.
################################################################

library(pacman)
p_load(tidyverse, openxlsx, here, jsonlite)

gravi <- read_csv("maia_gravi.csv") 

initialrows = nrow(gravi) #number of rows at start

### INITIAL CLEAN ######################

#remove unnecessary data
gravi <- gravi %>% 
  select(!c(error_flag, shutdown_mode, run_time_hrs:gravi_diff_ug)) %>% 
  rename(deploy_start = `deploy_start`) %>% #if there is a naming issue 11/18
  mutate(deploy_start = as.POSIXct(deploy_start, format = "%m/%d/%Y %H:%M"))

# UNCLEANED DATA : gravi ~

#remove rows with empty gravimetric concentration
gravi_nonzero <- gravi %>%
  filter(!is.na(gravi_con_ugm3))

rows_complete = nrow(gravi_nonzero)
emptyrows = initialrows - rows_complete #number of rows with NA for concentration: emptyrows ~


##### DATA CLEANING ########

#remove rows with not 24-hr run time or 2 L/min flowrate
gravi_valid <- gravi_nonzero %>% 
  filter(round(run_time_filter_hrs) > 18, # if runtime rounded to nearest whole <=18, delete row
         round(flow_avg_l_min) == 2) # if flowrate rounded to nearest whole /= 2, delete row

badrows <- rows_complete - nrow(gravi_valid) #number of rows with bad runtime or flowrate: badrows ~

#remove outliers
gravi_valid1 <- gravi_valid %>%
  filter(gravi_con_ugm3 <= 1000)

outliers <- anti_join(gravi_valid, gravi_valid1)
print(outliers) #outliers removed (values of outliers): outliers ~

outlier_num <- nrow(outliers) #number of outliers removed: outlier_num ~

###### ERROR COLUMN ###########

#make new variable; checkforerror; if gravi_con lower than 12 or filter pressure above 550, it's 1

gravi_new <- gravi_valid1 %>%
  mutate(checkforerror = case_when(gravi_con_ugm3<=12~"1",
                                   gravi_con_ugm3>12~"0", 
                                   #filter_dp>550~"1",
                                   #filter_dp<=550~"0",
                                   
  ),
  checkforerror = as.character(checkforerror),
  checkforerror = as.numeric(checkforerror)) 

total_flag <- sum(gravi_new$checkforerror, na.rm = TRUE) #number of concentrations at or below 12 ug/m3: total_flag ~


#check for duplicate filter ID, none if 0
if (sum(duplicated(gravi_new$filter)) == 0) { #are there duplicate filters after cleaning? Y/N duplicates ~
  duplicates <- "No" 
} else {
  duplicates <- "Yes"
} 

#Removing dates out of PTA: 

#convert to datetime
gravi_new <- gravi_new %>%
  mutate(deploy_start = as.POSIXct(deploy_start))

alldates = nrow(gravi_new)

#remove early dates (earliest is May 2022)
gravi_new <- gravi_new %>%
  filter(deploy_start >= as.POSIXct("2022-05-03"))

#dates removed before correct date
too_early = alldates - nrow(gravi_new)

#total rows removed
rows_removed <- nrow(gravi) - nrow(gravi_new)

#message for clean
cat("\n", "Starting size:", initialrows, "\n",
    "Number of valid rows:", nrow(gravi_new), "\n",
    "Total number of rows removed:", rows_removed, "\n", 
    "Rows with N/A for concentration:", emptyrows,"\n",
    "Rows with runtime <18 h or flowrate not ~2 L/min:", badrows,"\n",
    "Number of outliers:", outlier_num, "\n",
    "Duplicate filters?", duplicates,"\n",
    "Number of flagged runs for low concentration:", total_flag, "\n")


#check locations
gravi_new$lat <- round(gravi_new$lat, 2)
gravi_new$lng <- round(gravi_new$lng, 2)

gravi_new <- gravi_new %>%
  mutate(Coordinates = paste(gravi_new$lat, gravi_new$lng, sep = ","))

print(unique(gravi_new$Coordinates))


# #GROUP BY LOCATION
# 
# #     lat     lng  location
# #  33.69  -84.29  Atlanta - 37
# #  33.78  -84.35  ^^
# #  34.20 -118.17  Pasadena - 99

# 
# # -25.90   28.11  South Africa - 83
# # -25.91   28.13  ^^
# 
# #  28.54   77.34  Amity Noida, India - 19
# 
# #  28.95   77.10  Sonipat, India - 9
#
# #  28.32   76.92   Gurugram, India - 6
# 
# #  23.00  120.22  Tainan, Taiwan - 45
# #  22.99, 120.22  ^^
# 
# #  24.16  120.62  Taichung, Taiwan - 5
#
# #  8.54   39.28    Adama, Ethiopia - 5
# #  8.99   38.71    Addis Ababa, Ethiopia - 3



atl <- gravi_new %>%
  mutate(Location = "Atlanta") %>%
  filter(lng == -84.29) #37

jpl <- gravi_new %>%
  mutate(Location = "JPL") %>%
  filter(lng == -118.17) # 99

sa <- gravi_new %>%
  mutate(Location = "Pretoria, South Africa") %>%
  filter(lng == 28.11 | lng == 28.13) #83

noida <- gravi_new %>% #19
  mutate(Location = "Amity University Noida, India") %>%
  filter(lng == 77.34)

guru <- gravi_new %>% #6
  mutate(Location = "Amity University Manesar, India") %>%
  filter(lng == 76.92)

sonipat <- gravi_new %>% #9
  mutate(Location = "Sonipat, India") %>%
  filter(lng == 77.10)

tainan <- gravi_new %>% #45
  mutate(Location = "Tainan, Taiwan") %>%
  filter(lng == 120.22)

taichung <- gravi_new %>% #5
  mutate(Location = "Taichung, Taiwan") %>%
  filter(lng == 120.62)

adama <- gravi_new %>% #5
  mutate(Location = "Adama, Ethiopia") %>%
  filter(lng == 39.28)

aa <- gravi_new %>% #3
  mutate(Location = "AASTU, Ethiopia") %>%
  filter(lng == 38.71)



#STATS: make final full dataset and summary dataset at the same time
# List of dataset variable names
variable_names <- c("atl", "sa", "noida", "guru", "sonipat", "tainan", "taichung", "adama", "aa", "jpl")

# Initialize an empty list to store results
results_list <- list()
all_data = data.frame()

# Loop through each dataset
for (var_name in variable_names) {

  # Get the dataset from the variable name
  dataset <- get(var_name)
  all_data <- rbind(all_data, dataset)
  
  # Compute number of points and mean for each unique entry in 'device'
  summary_df <- dataset %>%
    group_by(device) %>%
    summarize(
      count = n(),
      mean_gravi_con_ugm3 = mean(gravi_con_ugm3, na.rm = TRUE), 
      med = quantile(gravi_con_ugm3, 0.5),
      s5 = quantile(gravi_con_ugm3, 0.75),
      n5 = quantile(gravi_con_ugm3, 0.95),
      max = max(gravi_con_ugm3),
      Location = Location
    )
  
  # Add a column to identify the dataset
  summary_df <- summary_df %>%
    mutate(dataset = var_name)
  
  # Append the result to the list
  results_list[[var_name]] <- summary_df
}

# Concatenate all results into one dataframe
summary_results <- do.call(rbind, results_list)
summary_results <- unique(summary_results) 

summary_results <- summary_results %>% rename("Device" = device, 
                                              "Number of valid filters" = count,
                                              "Mean PM2.5 Concentration (µg/m³)" = mean_gravi_con_ugm3,
                                              "Median" = med,
                                              "75th percentile" = s5,
                                              "95th percentile" = n5,
                                              "Max" = max) 
summary_results <- summary_results %>% select(-dataset) #final

  
all_data <- all_data %>%
  select(filter, device, deploy_start, gravi_con_ugm3, checkforerror, Location) %>%
  rename ('Filter ID' = filter,
          'Device' = device, 
          'PM2.5 Concentration (µg/m³)' = gravi_con_ugm3) %>%
  mutate('Day' = as.Date(deploy_start)) %>%
  select(-deploy_start, -checkforerror) #final
        
write.csv(all_data, "all_grav.csv", row.names = FALSE) #grav table
write.csv(summary_results, "grav_summary.csv", row.names = FALSE) #summary

