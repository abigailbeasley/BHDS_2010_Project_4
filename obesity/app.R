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

# loading in data
data <- read.csv('../data/IHME_USA_OVERWEIGHT_OBESITY_PREVALENCE_1990_2050/IHME_USA_OVERWEIGHT_OBESITY_PREVALENCE_1990_2050_AGES_2_24_OW_OB_Y2024M11D07.CSV')


# filtering on state level
state_level <- data %>%
  filter(location_level=='State')

# adding state variable
state_level$state <- state_level$location_name

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Trends in Overweight Prevalence Among Younger U.S. Demographics"),

    # Sidebar with a slider input for number of bins 
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
                        value = 2000, step = 1, sep = "")  # `sep=""` removes comma in numbers
          )
          ),

        # Show a plot of the generated distribution
        mainPanel(
         tabsetPanel(
           tabPanel("Map", plotOutput("mapPlot")), #Map tab
            tabPanel("Histogram", plotlyOutput("histPlot")) #Histogram tab
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # creating map output
    output$mapPlot <- renderPlot({
        # filtering data for map
        plot_data <- state_level %>%
          filter(sex==input$gender & year_id==input$year & age_group_name==input$age)
      
        # generate bins based on input$bins from ui.R
        x    <- plot_data$mean_prev
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        plot_usmap(
          color = "white",
          linewidth = 0.1,
          regions = "states",
          data = plot_data,
          values = "mean_prev")
        
    })
    #Creating the interactive histogram. 
    
    #Filtering data for plot as well.
    output$histPlot <- renderPlotly({
        plot_data <- state_level %>%
        filter(sex == input$gender & year_id == input$year & age_group_name == input$age)
    
    #Creating histogram using ggplot
    ggplot(plot_data, aes(x = mean_perv)) + geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
        labs(title = "Distribution of Overweight Prevalence",
             x = "Mean Prevalence (%)"
             y = "Frequency" +
             theme_minimal()
 })       
}

# Run the application 
shinyApp(ui = ui, server = server)
