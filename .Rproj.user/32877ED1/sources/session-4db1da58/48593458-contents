

# translate NASA data to communicate AP information in remote areas
# build satellites
output$plotsUI <- renderUI({
  # Filter data based on selected location
  selected_data <- all_recent_df %>% filter(location == input$location)
  
  # Check if there is data for the selected location
  if (nrow(selected_data) == 0) {
    # Display a popup message if no data is available
    showModal(modalDialog(
      title = "No Data Available",
      "There is no data available from the past 2 weeks at this location.",
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL) # Return NULL to avoid rendering plots
  }
  
  # Generate a plot for each variable with extra space and a divider
  plot_output_list <- lapply(c("pm", "flow", "pressure", "rh", "t"), function(variable) {
    plotname <- paste0(variable, "Plot")
    
    # Return the plotOutput along with more space and a thicker divider
    tagList(
      plotOutput(plotname),
      br(), br(),  # Add extra space
      hr(style = "border-top: 3px solid #000000;")  # Thicker horizontal line
    )
  })
  
  # Arrange plots in a vertical layout
  do.call(tagList, plot_output_list)
})

observe({
  # Render each plot
  output$pmPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = pm)) +
      labs(title = "PM2.5 Concentration",
           y = "PM2.5 (ug/m3)",
           x = "Time") +
      theme_minimal()
  })
  
  output$flowPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = flow)) +
      labs(title = "Flowrate",
           y = "Flowrate (L/min)",
           x = "Time") +
      theme_minimal()
  })
  
  output$pressurePlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = pressure)) +
      labs(title = "Pressure",
           y = "Pressure (dp)",
           x = "Time") +
      theme_minimal()
  })
  
  output$rhPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = rh)) +
      labs(title = "Relative Humidity",
           y = "Relative Humidity",
           x = "Time") +
      theme_minimal()
  })
  
  output$tPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = t)) +
      labs(title = "Temperature",
           y = "Temperature (C)",
           x = "Time") +
      theme_minimal()
  })
})











# PANEL TWO ######################


# PANEL THREE ######################

output$alltableUI <- renderTable({
  filtered_data <- all_daily %>% filter(location == input$location)
  
  formatted_data <- filtered_data %>%
    mutate(
      day = format(as.Date(day), "%Y-%m-%d"),
      n_readings = format(round(n_readings, digits = 0), nsmall = 0),
      elapsed_time = format(round(elapsed_time, digits = 0), nsmall = 0),
      daily_mean_pm = format(round(daily_mean_pm, digits = 1), nsmall = 1),
      daily_95_pm = format(round(daily_95_pm, digits = 1), nsmall = 1),
      daily_mean_rh = format(round(daily_mean_rh, digits = 1), nsmall = 1),
      daily_mean_t = format(round(daily_mean_t, digits = 1), nsmall = 1)
    )
  
  colnames(formatted_data) <- c('Location', 'Device', 'Filter', 'Day', 'Elapsed Time (Hours)', 'Total # Readings', 'Daily Mean PM2.5 Conc. (ug/m3)', '...95th pctl', 'Daily Mean RH', 'Daily Mean Temp (C)')
  
  formatted_data #show table
})

# PANEL FOUR ######################

output$visualsUI <- renderUI({
  # Dropdown for selection
  fluidPage(
    fluidRow(
      column(12,
             selectInput("variables", "Select Variables (up to 3):",
                         choices = c("PM2.5 Concentration" = "pm", "Relative Humidity" = "rh", "Temperature" = "t"),
                         selected = c("pm"),
                         multiple = TRUE,
                         selectize = TRUE)
      )
    ),
    plotOutput("dynamicPlot"),
    br(),  # Add space
    plotOutput("pmScatterPlot")
  )
})

# Render the dynamic plot
output$dynamicPlot <- renderPlot({
  selected_data <- all_recent_df %>% filter(location == input$location)
  selected_vars <- input$variables
  
  # Check if selected variables are not empty
  if (length(selected_vars) == 0) {
    return(NULL)
  }
  
  data_long <- selected_data %>%
    select(time, device, all_of(selected_vars)) %>%
    pivot_longer(cols = all_of(selected_vars), names_to = "Variable", values_to = "Value")
  
  color_mapping <- c(
    "pm" = "red",
    "rh" = "darkblue",
    "t" = "black"
  )
  
  legend_labels <- c(
    "pm" = "PM2.5 Concentration",
    "rh" = "Relative Humidity",
    "t" = "Temperature"
  )
  
  ggplot(data_long, aes(x = time, y = Value, color = Variable, shape = device)) +
    geom_point() +
    scale_color_manual(values = color_mapping, labels = legend_labels) +
    labs(title = "Previous 2 Weeks Time Series Data", x = "Time", y = "Value") +
    theme_minimal() +
    theme(legend.position = "bottom")
})
#PANEL FIVE ##############################

# Render plots based on selected location
output$coloUI <- renderUI({
  
  
  
  # Render the PM2.5 scatter plot
  output$pmScatterPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    
    # Get the unique devices
    unique_devices <- unique(selected_data$device)
    
    # Check if there are at least two unique devices
    if (length(unique_devices) >= 2) {
      # Filter data for PM2.5 from the first to the second unique device
      pm_data_device1 <- selected_data %>%
        filter(device == unique_devices[1]) %>%
        select(time, pm_device1 = pm)
      
      pm_data_device2 <- selected_data %>%
        filter(device == unique_devices[2]) %>%
        select(time, pm_device2 = pm)
      
      # Merge data on time
      merged_pm_data <- inner_join(pm_data_device1, pm_data_device2, by = "time")
      
      # Generate the PM2.5 scatter plot
      ggplot(merged_pm_data, aes(x = pm_device1, y = pm_device2)) +
        geom_point() +
        labs(title = paste("PM2.5 Comparison: Device", unique_devices[1], "vs", unique_devices[2]),
             x = paste("PM2.5 (Device", unique_devices[1], ")"),
             y = paste("PM2.5 (Device", unique_devices[2], ")")) +
        geom_abline(slope = 1, intercept = 0, color = "grey", linetype = "dashed") +
        theme_minimal()
    } else {
      # Return an empty plot if there aren't enough devices
      ggplot() + geom_blank() +
        labs(title = "Not enough devices for comparison",
             x = NULL,
             y = NULL)
    }
  })
})

}


# NEW 
#### BUILD APP ----------------------------------------------
rm(list = ls())
library(dplyr)
library(tidyr)
library(lubridate)
library(shiny)
library(bslib)
library(ggplot2)
library(readr)

# Global variables for app
locations <- c("Pretoria, South Africa", "Taichung, Taiwan", "Tainan, Taiwan", "Delhi, India", 
               "Amity University Noida, India", "Amity University Manesar, India", "Adama, Ethiopia", 
               "AASTU, Ethiopia")

all_recent_df <- read_csv("all_recent_data.csv", show_col_types = FALSE) #for plotting recent data
recent_daily <- read_csv("recent_daily.csv", show_col_types = FALSE) # for recent stats
all_daily <- read_csv("all_daily.csv", show_col_types = FALSE)


# Define UI
ui <- navbarPage(
  title = 'AMOD/S Data Visualization App for MAIA + CEAMS',
  
  # Location selection at the top
  tabPanel(
    fluidPage(
      fluidRow(
        column(12,
               selectInput("location", "Select A Location:", choices = locations, selected = "Amity University Noida, India")
        )
      )
    )
  ),
  
  # Panel 1: Recent Continuous data
  tabPanel(
    "Recent Plantower Data Visualization",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Select plot type"),
          radioButtons("plot_type", "Select Data View:",
                       choices = list(
                         "Previous two weeks timeseries of continuous PM2.5 and meteorological data" = "timeseries",
                         "Co-location plot" = "colocation"
                       ),
                       selected = "timeseries")
        ),
        mainPanel(
          uiOutput("combinedUI")  # Dynamic UI for the combined panel
        )
      )
    )
  ),
  
  # Panel 2: Continuous Stats
  tabPanel(
    "24-hour Averaged Plantower Stats",
    fluidPage(
      # Add the date range selection controls in this panel
      sidebarLayout(
        sidebarPanel(
          h4("Select timeframe for 24-hour averaged stats to display in table and figure"),
          radioButtons("date_range", "Select Date Range:",
                       choices = list("Past Two Weeks" = "two_weeks",
                                      "Past Two Months" = "two_months",
                                      "Custom" = "custom")),
          uiOutput("custom_date_ui"),  # Conditionally display custom date range input
          
          p(strong("Note:"), "Averages shown for days with greater than 18 hours of continuous data.")
        ),
        
        mainPanel(
          plotOutput("timeseries"), #timeseries to match the table stats
          uiOutput("tableUI")  # Table to display filtered stats
          
        )
      )
    )
  ),
  
  # Panel 3: FILTER DATA
  tabPanel(
    "Gravimetric Filter Stats" #,
    #uiOutput() #put gravimetric data 
  ),
  
  # Panel 5: All-time colo
  tabPanel(
    "Co-locations"
    #uiOutput() #put plots of continuous vs. gravi
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # PANEL ONE: RECENT PLOTS ##############################
  # User selection combined UI #####
  output$combinedUI <- renderUI({
    if (input$plot_type == "timeseries") {
      tagList(
        
        h4("Dynamic Plot: Previous 2 Weeks Time Series Data"),
        plotOutput("dynamicPlot"),  
        hr(style = "border-top: 3px solid #000000;"),  
        
        h4("Timeseries Plots for PM2.5 and Meteorological Data"),
        plotOutput("pmPlot"),
        plotOutput("flowPlot"),
        plotOutput("pressurePlot"),
        plotOutput("rhPlot"),
        plotOutput("tPlot")
      )
    } else if (input$plot_type == "colocation") {
      h4("Co-location Plot"),
      plotOutput("coloplot")
    }
  })
  
  # Dynamic plot #####
  
  output$dynamicPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    selected_vars <- c("pm", "rh", "t")
    
    data_long <- selected_data %>%
      select(time, device, all_of(selected_vars)) %>%
      pivot_longer(cols = all_of(selected_vars), names_to = "Variable", values_to = "Value")
    
    color_mapping <- c(
      "pm" = "red",
      "rh" = "darkblue",
      "t" = "black"
    )
    
    legend_labels <- c(
      "pm" = "PM2.5 Concentration",
      "rh" = "Relative Humidity",
      "t" = "Temperature"
    )
    
    ggplot(data_long, aes(x = time, y = Value, color = Variable, shape = device)) +
      geom_point() +
      scale_color_manual(values = color_mapping, labels = legend_labels) +
      labs(title = "Previous 2 Weeks Time Series Data", x = "Time", y = "Value") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Timeseries plots for PM2.5 and meteorological data ##############
  output$pmPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = pm)) +
      labs(title = "PM2.5 Concentration", y = "PM2.5 (ug/m3)", x = "Time") +
      theme_minimal()
  })
  
  output$flowPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = flow)) +
      labs(title = "Flowrate", y = "Flowrate (L/min)", x = "Time") +
      theme_minimal()
  })
  
  output$pressurePlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = pressure)) +
      labs(title = "Pressure", y = "Pressure (dp)", x = "Time") +
      theme_minimal()
  })
  
  output$rhPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = rh)) +
      labs(title = "Relative Humidity", y = "Relative Humidity", x = "Time") +
      theme_minimal()
  })
  
  output$tPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = t)) +
      labs(title = "Temperature", y = "Temperature (C)", x = "Time") +
      theme_minimal()
  })
  
  # Co-location plot #######################
  output$coloplot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    
    unique_devices <- unique(selected_data$device)
    
    if (length(unique_devices) >= 2) {
      pm_data_device1 <- selected_data %>%
        filter(device == unique_devices[1]) %>%
        select(time, pm_device1 = pm)
      
      pm_data_device2 <- selected_data %>%
        filter(device == unique_devices[2]) %>%
        select(time, pm_device2 = pm)
      
      merged_pm_data <- inner_join(pm_data_device1, pm_data_device2, by = "time")
      
      ggplot(merged_pm_data, aes(x = pm_device1, y = pm_device2)) +
        geom_point() +
        labs(title = paste("PM2.5 Comparison: Device", unique_devices[1], "vs", unique_devices[2]),
             x = paste("PM2.5 (Device", unique_devices[1], ")"),
             y = paste("PM2.5 (Device", unique_devices[2], ")")) +
        geom_abline(slope = 1, intercept = 0, color = "grey", linetype = "dashed") +
        theme_minimal()
    } else {
      ggplot() + geom_blank() +
        labs(title = "Not enough devices for comparison")
    }
  })
  
  
  
  ## PANEL TWO: STATS #####################################
  
  # Date range UI for the second panel ####
  output$custom_date_ui <- renderUI({
    if (input$date_range == "custom") {
      dateRangeInput("custom_dates", "Select Date Range:",
                     start = Sys.Date() - 30, end = Sys.Date())
    }
  })
  
  # Filtering based on date selection
  output$tableUI <- renderTable({
    # Filter data by location
    filtered_data <- all_daily %>% filter(location == input$location)
    
    # Date filtering based on selection
    if (input$date_range == "two_weeks") {
      filtered_data <- filtered_data %>%
        filter(day >= Sys.Date() - 14 & day <= Sys.Date())
    } else if (input$date_range == "two_months") {
      filtered_data <- filtered_data %>%
        filter(day >= Sys.Date() - 60 & day <= Sys.Date())
    } else if (input$date_range == "custom") {
      filtered_data <- filtered_data %>%
        filter(day >= input$custom_dates[1] & day <= input$custom_dates[2])
    }
    
    # Formatting data
    formatted_data <- filtered_data %>%
      mutate(
        day = format(as.Date(day), "%Y-%m-%d"),
        n_readings = format(round(n_readings, digits = 0), nsmall = 0),
        elapsed_time = format(round(elapsed_time, digits = 0), nsmall = 0),
        daily_mean_pm = format(round(daily_mean_pm, digits = 1), nsmall = 1),
        daily_95_pm = format(round(daily_95_pm, digits = 1), nsmall = 1),
        daily_mean_rh = format(round(daily_mean_rh, digits = 1), nsmall = 1),
        daily_mean_t = format(round(daily_mean_t, digits = 1), nsmall = 1)
      )
    
    colnames(formatted_data) <- c('Location', 'Device', 'Filter', 'Day', 'Elapsed Time (Hours)', 'Total # Readings', 'Daily Mean PM2.5 Conc. (ug/m3)', '...95th pctl', 'Daily Mean RH', 'Daily Mean Temp (C)')
    
    formatted_data # Show table
  })
  
  # Timeseries plot for selected dates ####
  output$pm25_plot <- renderPlot({
    filtered_data <- all_daily %>% 
      filter(location == input$location) %>%
      filter(elapsed_time > 18 & n_readings > 30)
    
    #edit dataframe so days with much time in between are not connected by line
    # TO- DO?
    
    if (input$date_range == "two_weeks") {
      filtered_data <- filtered_data %>%
        filter(day >= Sys.Date() - 14 & day <= Sys.Date())
    } else if (input$date_range == "two_months") {
      filtered_data <- filtered_data %>%
        filter(day >= Sys.Date() - 60 & day <= Sys.Date())
    } else if (input$date_range == "custom") {
      filtered_data <- filtered_data %>%
        filter(day >= input$custom_dates[1] & day <= input$custom_dates[2])
    }
    
    #Plot
    ggplot(filtered_data, aes(x = as.Date(day), y = daily_mean_pm, colour = device)) +
      geom_line(linetype = "dashed") +
      geom_point()+
      labs(title = "24-h Averaged Continuous PM2.5 Concentration (ug/m3)", x = "Day", y = "PM2.5 (ug/m3)")+
      theme_minimal()
  })
  
}
# END SERVER LOGIC #######

# Run the application 
shinyApp(ui = ui, server = server)