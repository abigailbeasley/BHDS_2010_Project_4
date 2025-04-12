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

# Load data before ui and server so it's accessible to both
data <- read.csv('OverweightPrevalenceData.csv')

# Filtering on state level
state_level <- data %>%
  filter(location_level == 'State')

# Adding state variable
state_level$state <- state_level$location_name

# Define UI
ui <- fluidPage(
  titlePanel("Trends in Overweight Prevalence Among Younger U.S. Demographics"),
  
  sidebarLayout(
    sidebarPanel(
      inputPanel(
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
                    value = 2000, step = 1, sep = "")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", plotOutput("mapPlot")),
        tabPanel("Histogram", plotlyOutput("histPlot"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Map plot
  output$mapPlot <- renderPlot({
    plot_data <- state_level %>%
      filter(sex == input$gender & year_id == input$year & age_group_name == input$age)
    
    plot_usmap(
      color = "white",
      linewidth = 0.1,
      regions = "states",
      data = plot_data,
      values = "mean_prev"
    )
  })
  
  # Histogram plot
  output$histPlot <- renderPlotly({
    plot_data <- state_level %>%
      filter(sex == input$gender & year_id == input$year & age_group_name == input$age)
    
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
}

# Run the app
shinyApp(ui = ui, server = server)