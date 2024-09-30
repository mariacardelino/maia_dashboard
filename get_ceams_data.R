library(httr)
library(jsonlite)
library(dplyr)

get_ceams_data <- function(x) {
  
auth <- authenticate("MCV3HA0YGPTMZGUT6L5HBBH6", "")

# Initialize an empty data frame to hold all the data
all_data <- data.frame()

# Loop through each device ID
for (device_id in x) {
  page <- 1
  repeat {
    url <- paste0('https://csu-ceams.com/api/v1/devices/', device_id, '/data/')
    response <- GET(url, auth, query = list(per_page = 1000, page = page))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"), flatten = TRUE)
      
      # Convert the JSON response to a data frame
      df <- as.data.frame(data$data)
      
      # Append the data to the main data frame
      all_data <- bind_rows(all_data, df)
      
      # Check if there are more pages
      if (data$meta$page < data$meta$pages) {
        print(page)
        page <- page + 1
        Sys.sleep(1)
      } else {
        break
      }
    } else {
      print(paste("Failed to retrieve data for device", device_id, ":", status_code(response)))
      break
    }
  }
}

return(all_data)
}
