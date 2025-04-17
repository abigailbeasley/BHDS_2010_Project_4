#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(scales)
library(usmap)
library(sf)
library(tigris)
library(plotly)
library(DT)

file.exists("OverweightPrevalenceData.csv")

# Load data before ui and server so it's accessible to both
data <- read.csv("OverweightPrevalenceData.csv")

# Filtering on state level
state_level <- data %>%
  filter(location_level == 'State')

# Adding state variable
state_level$state <- state_level$location_name

# # Define UI for application that draws a histogram
ui <- fluidPage(
  
  #Application title
  titlePanel("Trends in Overweight Prevalence Among Younger U.S. Demographics"),
  
  #Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        selectInput("gender",
                    label = "Select Gender",
                    choices = c('Female', 'Male', 'Both'),
                    selected = 'Both'),
        selectInput("age",
                    label = "Select Age Group",
                    choices = unique(state_level$age_group_name),
                    selected = '2 to 4'),
        sliderInput("year", "Select Year:",
                    min = min(data$year_id), max = max(data$year_id),
                    value = 2000, step = 1, sep = ""),
        selectInput("states",
                    label = "Select States to Display",
                    choices = sort(unique(state_level$state)),
                    selected = c("California", "New York"),  # or any reasonable default
                    multiple = TRUE)
    ),
    # Show a plot of the generate distribution. Adding tabs to switch between
    # map and histogram
    mainPanel(
      tabsetPanel(
        tabPanel("Map", plotOutput("mapPlot")),
        tabPanel("Histogram", plotlyOutput("histPlot")),
        tabPanel("Time Series", plotlyOutput("timeSeriesPlot")),
        tabPanel("Grouped Summary Statistics", DT::dataTableOutput("summaryTable"))
        
        
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # creating map output
  output$mapPlot <- renderPlot({
    # filtering data for map
    plot_data <- state_level %>%
      filter(sex == input$gender & year_id == input$year & age_group_name == input$age)
    
    # generate bins based on input$bins from ui.R
    x    <- plot_data$mean_prev
    #bins <- seq(min(x), max(x), length.out = input$bins +1)
    
    plot_usmap(
      color = "white",
      linewidth = 0.1,
      regions = "states",
      data = plot_data,
      values = "mean_prev"
    )
  })
  
  #Creating the interactive histogram
  
  #Filtering data for plot.
  output$histPlot <- renderPlotly({
    plot_data <- state_level %>%
      filter(sex == input$gender & year_id == input$year & age_group_name == input$age)
  
    #Creating histogram using ggplot and plotly. 
    hp <- ggplot(plot_data, aes(x = mean_prev)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(
        title = "Distribution of Overweight Prevalence",
        x = "Mean Prevalence (%)",
        y = "Frequency"
      ) +
      theme_minimal()
    
    ggplotly(hp)
  })
  #Adding time series plot
  output$timeSeriesPlot <- renderPlotly({
    req(input$states)  # only render if at least one state is selected
  #Filtering the data  
    plot_data <- state_level %>%
      filter(sex == input$gender,
             age_group_name == input$age,
             state %in% input$states)
    
    ts_plot <- ggplot(plot_data, aes(x = year_id, y = mean_prev, color = state)) +
      geom_line(size = 1) +
      labs(
        title = "Overweight Prevalence Over Time",
        x = "Year",
        y = "Mean Prevalence (%)",
        color = "State"
      ) +
      theme_minimal()
    #Outputting the plot
    ggplotly(ts_plot)
  })
  #Adding summary statistics
  output$summaryTable <- DT::renderDataTable({
    summary_data <- state_level %>%
      filter(state %in% input$states)
  #Grouping data  
    summary_stats <- summary_data %>%
      group_by(year_id, sex, age_group_name) %>%
      summarise(
        Count = n(),
        Mean = round(mean(mean_prev, na.rm = TRUE), 2),
        Median = round(median(mean_prev, na.rm = TRUE), 2),
        SD = round(sd(mean_prev, na.rm = TRUE), 2),
        Min = round(min(mean_prev, na.rm = TRUE), 2),
        Max = round(max(mean_prev, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      arrange(year_id, sex, age_group_name) %>%
      rename(
        Year = year_id,
        Gender = sex,
        `Age Group` = age_group_name
      )
    #Outputting the summary statistics
    DT::datatable(summary_stats,
                  options = list(pageLength = 15, autoWidth = TRUE),
                  rownames = FALSE)
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)