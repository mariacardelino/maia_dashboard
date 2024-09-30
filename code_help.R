# SKELETON #
library(shiny)
library(bslib)

# Define UI ----
ui <- page_sidebar(
  
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
###########################################


#### UI ######################## 

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  # App title ----
  title = "Hello Shiny!",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = "bins",
      label = "Number of bins:",
      min = 1,
      max = 50,
      value = 30
    )
  ),
  # Output: Histogram ----
  plotOutput(outputId = "distPlot")
)
#example
ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar("Sidebar"),
  card(
    card_header("Card header"),
    "Card body"
  )
) 

#Define UI
ui <- page_sidebar(
  title = "title panel",
  sidebar = sidebar("Sidebar"),
  layout_columns(
    col_width = 1,
    card(
      card_header("Buttons"),
      actionButton("action", "Action"),
      submitButton("Submit")
    ),
    card(
      card_header("Date range input"),
      dateRangeInput("dates", "Select dates")
    ),
    card(
      card_header("File input"),
      fileInput("file", label = NULL)
    )
    
  )
)

###### SERVER ############

# Define server logic ----
server <- function(input, output) {
  
}

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

####### RUN ##############
#Run the app ----
shinyApp(ui = ui, server = server)

