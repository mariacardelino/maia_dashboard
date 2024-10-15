#### BUILD APP ----------------------------------------------
rm(list = ls())
library(dplyr)
library(tidyr)
library(lubridate)
library(shiny)
library(bslib)
library(ggplot2)
library(readr)
library(rsconnect)

# Global variables for app
locations <- c("Pretoria, South Africa", "Taichung, Taiwan", "Tainan, Taiwan", "Delhi, India", 
               "Amity University Noida, India", "Amity University Manesar, India", "Adama, Ethiopia", 
               "AASTU, Ethiopia")

all_recent_df <- read_csv("all_recent_data.csv", show_col_types = FALSE) #for plotting recent data
recent_daily <- read_csv("recent_daily.csv", show_col_types = FALSE) # for recent stats
all_daily <- read_csv("all_daily.csv", show_col_types = FALSE)

all_grav <- read_csv("all_grav.csv", show_col_types = FALSE) #for grav
grav_summary <- read_csv("grav_summary.csv", show_col_types = FALSE) 

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
  
  # Panel 1: Recent Continuous data BY LOCATION
  tabPanel(
    "Recent Plantower Data Visualization",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("AMOD/S Continuous Data from Previous Two Weeks"),
          radioButtons("plot_type", "Select Plot Type:",
                       choices = list(
                         "Timeseries for all variables" = "timeseries",
                         "Dynamic plot with selectable variables" = "dynamic",
                         "Co-location plot" = "colocation"
                       ),
                       selected = "timeseries")
        ),
        mainPanel(
          uiOutput("paneloneUI")  # Dynamic UI for the combined panel
        )
      )
    )
  ),
  
  # Panel 2: Continuous Stats BY LOCATION
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
          plotOutput("pm25_plot"), #timeseries to match the table stats
          uiOutput("tableUI")  # Table to display filtered stats
          
        )
      )
    )
  ),
  
  # Panel 3: Filter data BY LOCATION
  tabPanel(
    "Gravimetric Filter Stats",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Gravimetric Filter Data Visualization: Click to see data from selected location."),
          radioButtons("gravi_choice", "Select:",
                       choices = list(
                         "Summary table for filters" = "gravi_table", 
                         "Timeseries of filter concentrations (*in progress*)" = "time_grav"),
                       selected = "gravi_table")
        ),
        mainPanel(
          uiOutput("gravipanelUI")  # Dynamic UI for the combined panel
        )
      )
    )
  ), 
# Panel 4: Summary ALL LOCATIONS
  tabPanel(
    tags$div(
      tags$div(style = "font-weight: bold;", "All-location Summaries")
    ),
    sidebarLayout(
      sidebarPanel(
        h4("Gravimetric and continuous data visualization for all locations"),
        radioButtons("all_choice", "Select:",
                     choices = list(
                       "Scatterplot of raw continuous vs. gravimetric concentrations" = "combined_plot",
                       "Summary for all processed filters" = "gravi_sum",
                       "Calendar of gravimetric filters by location and device" = "calendar"),
                     selected = "calendar")
      ),
      mainPanel(
        uiOutput("sumUI")  # Dynamic UI for the combined panel
      )
    )
  )
) 
# end UI section

# Define server logic
server <- function(input, output, session) {
  
  # PANEL ONE: RECENT PLOTS ##############################
    
  # User selection combined UI 
  output$paneloneUI <- renderUI({
    #timeseries
    if (input$plot_type == "timeseries") {
      tagList(
        h4("Timeseries Plots for PM2.5 and Meteorological Data"),
        br(),
        
        wellPanel(
          style = "border: 1px solid #000; padding: 10px;",
          h4("PM2.5 Concentration"),
          plotOutput("pmPlot") ),
        br(),
        
        wellPanel(
          style = "border: 1px solid #000; padding: 10px;",
          h4("Flowrate"),
          plotOutput("flowPlot") ),
        br(), 
        
        wellPanel(
          style = "border: 1px solid #000; padding: 10px;",
          h4("Pressure"),
          plotOutput("pressurePlot") ),
        br(),
        
        wellPanel(
          style = "border: 1px solid #000; padding: 10px;",
          h4("Relative Humidity"),
          plotOutput("rhPlot") ),
        br(), 
        
        wellPanel(
          style = "border: 1px solid #000; padding: 10px;",
          h4("Temperature"),
          plotOutput("tPlot") ),
        br()
      )
      
    } else if (input$plot_type == "dynamic") {
      tagList(
        wellPanel(
          style = "border: 1px solid #000; padding: 10px;",
          h4("Dynamic Timeseries Plot"),
          uiOutput("dynamicUI"),  # Add this line to render the dynamic UI
          plotOutput("dynamicPlot")
        )
      )
    } else if (input$plot_type == "colocation") {
      tagList(
        wellPanel(
          style = "border: 1px solid #000; padding: 10px;",
          h4("PM2.5 Concentration between Co-located Devices (Previous Two Weeks)"),
          plotOutput("coloplot") )
      )
    } 
  }) #end panel one UI
  
  # Dynamic plot UI and rendering 
  
  #Dynamic plot UI
  output$dynamicUI <- renderUI({
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
      )
    )
  }) #end dynamic plot UI
  
  # Render the dynamic plot
  output$dynamicPlot <- renderPlot({
    
    req(input$variables)
    
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
    
    ggplot(data_long, aes(x = time, y = Value, color = Variable)) +
      facet_wrap(~device, ncol = 1, nrow =2)+
      geom_point() +
      scale_color_manual(values = color_mapping, labels = legend_labels) +
      labs(title = "Previous 2 Weeks Time Series Data", x = "Time", y = "Value") +
      theme_grey() +
      theme(legend.position = "bottom")
  }) #End dynamic plot rendering
  
  
  
    # Timeseries plots for PM2.5 and meteorological data #
  output$pmPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = pm)) +
      labs(title = NULL, y = "PM2.5 (µg/m³)", x = "Time") +
      theme_minimal()
  })
  
  output$flowPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = flow)) +
      labs(title = NULL, y = "Flowrate (L/min)", x = "Time") +
      theme_minimal()
  })
  
  output$pressurePlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = pressure)) +
      labs(title = NULL, y = "Pressure (dp)", x = "Time") +
      theme_minimal()
  })
  
  output$rhPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = rh)) +
      labs(title = NULL, y = "Relative Humidity", x = "Time") +
      theme_minimal()
  })
  
  output$tPlot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    ggplot(selected_data, aes(x = time, color = device)) +
      geom_point(aes(y = t)) +
      labs(title = NULL, y = "Temperature (C)", x = "Time") +
      theme_minimal()
  })
  
    # Co-location plot ##
  output$coloplot <- renderPlot({
    selected_data <- all_recent_df %>% filter(location == input$location)
    
    unique_devices <- unique(selected_data$device)
    
    if (length(unique_devices) >= 2) {
      pm_data_device1 <- selected_data %>%
        filter(device == unique_devices[1]) %>%
        select(time, pm_device1 = pm, flow1 = flow)
      
      pm_data_device2 <- selected_data %>%
        filter(device == unique_devices[2]) %>%
        select(time, pm_device2 = pm, flow2 = flow)
      
      # Merge the data
      merged_pm_data <- inner_join(pm_data_device1, pm_data_device2, by = "time")
      
      # Create a new column for color based on the condition for both flow rates
      merged_pm_data <- merged_pm_data %>%
        mutate(color = case_when(flow1 > 1 & flow2 > 1  ~ "Pumps on",
                                 flow1 < 1 & flow2 < 1 ~ "Pumps off"))
      
      # Calculate R-squared for both groups
      r_squared_both_on <- summary(lm(pm_device2 ~ pm_device1, data = merged_pm_data %>% filter(color == "Pumps on")))$r.squared
      r_squared_one_off <- summary(lm(pm_device2 ~ pm_device1, data = merged_pm_data %>% filter(color == "Pumps off")))$r.squared

      ggplot(merged_pm_data, aes(x = pm_device1, y = pm_device2, color = color)) +
        geom_point() +
        labs(title = paste("Device", unique_devices[2], "vs.", unique_devices[1]),
             x = paste("PM2.5 Concentration (µg/m³), Device", unique_devices[1]),
             y = paste("PM2.5 Concentration (µg/m³), Device", unique_devices[2])) +
        geom_abline(slope = 1, intercept = 0, color = "grey", linetype = "dashed") +
        theme_grey() +
        # Add the R-squared annotations to the plot
        annotate("text", x = Inf, y = Inf,
                 label = paste("R² Pumps on =", round(r_squared_both_on, 3), "\nR² Pumps off =", round(r_squared_one_off, 3)),
                 hjust = 1.1, vjust = 1.5,
                 size = 5, color = "black",
                 fontface = "bold") +
        scale_color_manual(values = c("Pumps on" = "red", "Pumps off" = "blue")) +
        theme(legend.title = element_blank())
    } else {
      ggplot() + geom_blank() +
        labs(title = "Not enough devices for comparison")
    }
  })
  
  
  
  
  ## PANEL TWO: PLANTOWER STATS #####################################
  
  # Date range UI for the second panel ##
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
    
    colnames(formatted_data) <- c('Location', 'Device', 'Filter', 'Day', 'Elapsed Time (Hours)', 'Total # Readings', 'Daily Mean PM2.5 Conc. (µg/m³)', '...95th pctl', 'Daily Mean RH', 'Daily Mean Temp (C)')
    
    formatted_data # Show table
  })
  
  # Timeseries plot for selected dates ##
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
      labs(title = "24-h Averaged Continuous PM2.5 Concentration (µg/m³)", x = "Day", y = "PM2.5 (µg/m³)")+
      theme_minimal()
  })
  
  # PANEL THREE: GRAVIMETRIC #################

  #uiOutput: gravipanelUI (all choices), gravi_choice
  #choices: "gravi_table", "time_grav"
  
    # User selection combined UI #####
    output$gravipanelUI <- renderUI({
      # Timeseries
      if (input$gravi_choice == "gravi_table") {
        tagList(
          h4("Gravimetric Filters Analyzed from Selected Location"),
          uiOutput("gtable")
        )
      } else if (input$gravi_choice == "time_grav") { 
        wellPanel(
          style = "border: 1px solid #000; padding: 10px;",
          h4("Timeseries of PM2.5 Concentrations from Filters at Selected Location"),
          plotOutput("grav_plot")
        )
      }
        
    })
  
  #Panel 3 Option 1: table of data by selected location
  output$gtable <- renderTable({
    
    # Filter all gravimetric data by location
    filtered_grav <- all_grav %>% filter(Location == input$location)
    filtered_grav <- filtered_grav %>% select(`Filter ID`, Device, Day, `PM2.5 Concentration (µg/m³)`, Location) %>%
      mutate(
        Day = format(as.Date(Day), "%Y-%m-%d"),
        `PM2.5 Concentration (µg/m³)` = format(round(`PM2.5 Concentration (µg/m³)`, digits = 1), nsmall = 0))
    filtered_grav

  })
  
  #Panel 3 option 2: timeseries of gravimetric - FIX LATER
  output$grav_plot <- renderPlot ({
    ggplot(y= `PM2.5 Concentration (µg/m³)`, x= Day, data=filtered_grav)+
      geom_point 
  })
  

  

  
  
  #PANEL 4: all-location statistics ########################

  # Panel 4: Summary ALL LOCATIONS
  
  #UI for PANEL 4:
  # User selection combined UI #####
  output$sumUI <- renderUI({
    # Timeseries
    if (input$all_choice == "combined_plot") {
      tagList(
        wellPanel(
          style = "border: 1px solid #000; padding: 10px;",
          h4("Continuous vs. Gravimetric PM2.5 Concentrations Across Locations"),
          plotOutput("combinedgplot")
        )
      )
    } else if (input$all_choice == "gravi_sum") { 
      tagList(
        h4("Summary of ALL processed gravimetric filters"),
        uiOutput("sumtable")
      )
    } else if (input$all_choice == "calendar") {
    tagList(
      wellPanel(
        style = "border: 1px solid #000; padding: 10px;",
        h4("Temporal Coverage For Gravimetric Filters By Location and Device"),
        plotOutput("gcalendar",height = "1000px", width = "1000px")
      )
    )
    }
  })

  #Panel 4 Option 1: Continuous vs. gravimetric scatterplot #################
  output$combinedgplot <- renderPlot({
    
    filtered_all_daily <- all_daily 
    filtered_all_grav <- all_grav %>%
      rename(grav = "PM2.5 Concentration (µg/m³)")
    
    matched_data <- filtered_all_grav %>%
      inner_join(filtered_all_daily, by = c("Day" = "day", `Filter ID` = "filter")) 
    
    # Identify points that will be cut off
    cut_off_data <- matched_data %>%
      filter(grav > 200 | daily_mean_pm > 200)
    
    # Create the plot
    p <- ggplot(matched_data, aes(x = grav, y = daily_mean_pm, color = Location)) +
      geom_point(size = 2.5) +
      geom_vline(xintercept = 12, linetype = "dashed", color = "gray", size = 0.5) +
      geom_hline(yintercept = 12, linetype = "dashed", color = "gray", size = 0.5) +
      annotate("segment", x = 0, y = 0, xend = 200, yend = 200, color = "gray") +  # Fixed segment for diagonal line
      labs(title = "Daily Averaged Continuous PM2.5 vs. Daily Gravimetric PM2.5 Concentrations",
           x = "Daily Gravimetric Concentration (µg/m³)",
           y = "Daily Averaged Continuous PM2.5 (µg/m³)",
           color = "Location") +
      scale_colour_brewer(palette = "Set1") +
      xlim(0, 200) +  # Limit x-axis
      ylim(0, 200)    # Limit y-axis
    
    print(p)  # Print the plot
  })
  
  # Reactive expression to capture cut-off message
  cutoff_message <- reactive({
    filtered_all_daily <- all_daily 
    filtered_all_grav <- all_grav %>%
      rename(grav = "PM2.5 Concentration (µg/m³)")
    
    matched_data <- filtered_all_grav %>%
      inner_join(filtered_all_daily, by = c("Day" = "day", `Filter ID` = "filter")) 
    
    # Identify points that will be cut off
    cut_off_data <- matched_data %>%
      filter(grav > 200 | daily_mean_pm > 200)
    
    if (nrow(cut_off_data) > 0) {
      locations_cut_off <- unique(cut_off_data$Location)
      message <- paste("Points cut off from the following locations:", 
                       paste(locations_cut_off, collapse = "; "))
    } else {
      message <- "No points were cut off."
    }
    
    return(message)
  })
  
  # Render the message in the UI
  output$cutoff_message <- renderText({
    cutoff_message()  # Call the reactive expression to get the message
  })
  
  
  
  #Panel 4 option 2: summary table; all gravimetric summary data ######################
  output$sumtable <- renderTable({
    
    grav_summary <- grav_summary %>%
      mutate(
        `Number of valid filters` = format(round(`Number of valid filters`, digits = 0), nsmall = 0),
        `Mean PM2.5 Concentration (µg/m³)` = format(round(`Mean PM2.5 Concentration (µg/m³)`, digits = 1), nsmall = 0),
        `Median` = format(round(`Median`, digits = 1), nsmall = 0),
        `75th percentile` = format(round(`75th percentile`, digits = 1), nsmall = 0),
        `95th percentile` = format(round(`95th percentile`, digits = 1), nsmall = 0),
        `Max` = format(round(`Max`, digits = 1), nsmall = 0))
    grav_summary 
  })
  
 #Panel 4 option 3: Gravimetric calendar ##############################
  output$gcalendar<- renderPlot({
    
    all_grav <- all_grav %>%
      mutate(color = case_when(
        `PM2.5 Concentration (µg/m³)` > 140 ~ "red",
        `PM2.5 Concentration (µg/m³)` >= 40 & `PM2.5 Concentration (µg/m³)` <= 140 ~ "orange",
        TRUE ~ "black"  # default color
      ))
    
  #PLOT ALL-LOCATION CALENDAR
    c <- ggplot(all_grav, aes(x = Day, y = Device, color = color)) +
      geom_point(size = 2) +
      facet_wrap(~Location, scales = "free_y", ncol = 1) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", date_minor_breaks = "1 month",
                   sec.axis = dup_axis()) +  # Add secondary x-axis
      scale_color_manual(
        name = "24-h mean PM2.5 concentration",
        values = c("black" = "black", "orange" = "orange", "red" = "red"),
        breaks = c("black", "orange", "red"),
        labels = c("Less than 40 µg/m³", "40-140 µg/m³", "Greater than 140 µg/m³")) + 
      labs(x = NULL, y = NULL, title = NULL) +
      theme_minimal() + 
      theme(legend.position = "top",
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.x.top = element_text(size = 12),  # Style for top x-axis text
            axis.text.y = element_text(size = 9),
            strip.text = element_text(size = 10, face = "bold"),  
            strip.placement = "outside",  
            panel.spacing = unit(1, "lines"))
    c
    
  })
  
}
# END SERVER LOGIC #######

# Run the application 
shinyApp(ui = ui, server = server)
#shiny::runGitHub('maia_shiny','mariacardelino', ref="main")