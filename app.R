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
library(jsonlite)

file.exists("OverweightPrevalenceData.csv")

# Overweight Populations Data
data <- read.csv("OverweightPrevalenceData.csv")


# Food Scarcity Data
food_scarcity <- read.csv("food_scarcity_2019.csv")

# Diabetes Mortality Data 
diabetes_mortality <- read.csv("diabetes_mortality_2019.csv")

# Filter to state-level data and add state codes
state_mortality <- diabetes_mortality %>%
  filter(location_level == 'State') %>%
  mutate(state_code = state.abb[match(location_name, state.name)])

# Filtering on state level
state_level <- data %>%
  filter(location_level == 'State')

# Adding state variable
state_level$state <- state_level$location_name

# adding state code for plotly map
state_level$state_code <- state.abb[match(state_level$location_name, state.name)]

# # Define UI for application that draws a histogram
ui <- navbarPage("Overweight Population Trends",
                 
                 # page 1: overweight trends
                 tabPanel('Overweight Trends',
                          fluidPage(
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
                                  tabPanel("Map", plotlyOutput("mapPlot")),
                                  tabPanel("Histogram", plotlyOutput("histPlot")),
                                  tabPanel("Time Series", plotlyOutput("timeSeriesPlot"))
                                )
                              )
                            ))),
                 
                 # page 2: mortality rates
                 tabPanel('Mortality Rates',
                          fluidPage(
                            titlePanel('Mortality Rates'),
                            
                            # Sidebar with a inputs
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("mort_age",
                                            label = "Select Age Group",
                                            choices = unique(state_mortality$age_name),
                                            selected = 'All Ages'),
                                selectInput("mort_race",
                                          label = "Select Race",
                                          choices = unique(state_mortality$race_name),
                                          selected = 'Total')),
                              # Plots
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Map", plotlyOutput("mapMortality"))
                                  )
                                ) # end main panel
                          ) # end side bar layout
          ) # end page
          ) # end tab panel
  ) # end ui
                            

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Map of Overweight trends
  output$mapPlot <- renderPlotly({
    # Filter data for selected gender, year, and age group
    plot_data <- state_level %>%
      filter(sex == input$gender & year_id == input$year & age_group_name == input$age)
    
    # Plotting the chorepleth map
    plot_ly(
      data = plot_data,
      z = ~mean_prev,
      locations = ~state_code,  # state abbreviations (e.g., "CA")
      type = "choropleth",
      locationmode = "USA-states",
      colorscale = "Blues",
      colorbar = list(
        title = "Prevalence",
        tickformat = ".0%"  # show legend ticks as percentages
      ),
      text = ~paste0(
        location_name,
        "<br>Mean Overweight Prevalence: ",
        round(mean_prev * 100, 2), "%"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Prevalence of Overweight Populations by State",
        geo = list(scope = "usa")
      )
  })
  
  # Map of Overweight trends
  output$mapMortality <- renderPlotly({
    
    # Filter data for selected gender, year, and age group
    mort_data <- state_mortality %>%
      filter(age_name==input$mort_age & race_name==input$mort_race)
    
    # Plotting the chorepleth map
    plot_ly(
      data = mort_data,
      z = ~val * 100000, # mortality count per 100k
      locations = ~state_code,  # state abbreviations (e.g., "CA")
      type = "choropleth",
      locationmode = "USA-states",
      colorscale = "Blues",
      colorbar = list(
        title = "Mortality Rate (Per 100k)"
      ),
      text = ~paste0(
        location_name,
        "<br> Mortality Rate (per 100k People): ",
        round(val * 100000, 2), ""
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Prevalence of Overweight Populations by State",
        geo = list(scope = "usa")
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
    
    ggplotly(ts_plot)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


