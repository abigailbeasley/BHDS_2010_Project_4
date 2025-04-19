#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Abigail To DO:
# add mortality by county
# add description and 

library(shiny)
library(tidyverse)
library(scales)
library(usmap)
library(sf)
library(tigris)
library(plotly)
library(jsonlite)
library(DT)
file.exists("OverweightPrevalenceData.csv")

# Overweight Populations Data
data <- read.csv("OverweightPrevalenceData.csv")

# adding percent change in obesity YoY
data <- data %>%
  arrange(location_name, sex, age_group_name, year_id) %>%
  group_by(location_name, sex, age_group_name) %>%
  mutate(percent_change = (mean_prev - lag(mean_prev)) / lag(mean_prev) * 100)

# Food Scarcity Data

# loading GeoJSON data for county maps
geojson_url <- "https://cdn.jsdelivr.net/gh/plotly/datasets@master/geojson-counties-fips.json"
geojson_data <- fromJSON(geojson_url, simplifyVector = FALSE)

# on state level
food_disparity_state_level <- read.csv("food_disparities_state_agg.csv")

# on county level
food_disparity_county_level <- read.csv("food_disparities_county_agg.csv")

# making sure fips load in correctly and are strings
# converting to character for plotting
food_disparity_county_level$fips <- as.character(food_disparity_county_level$fips)

# some fips are missing trailing 0--adding it in
food_disparity_county_level$fips <- ifelse(nchar(food_disparity_county_level$fips) == 4,
                                           paste0('0',food_disparity_county_level$fips),
                                           food_disparity_county_level$fips)

## MAP DATA

# adding geo-data
# Make sure geo_id column exists
food_disparity_county_level$geo_id <- paste0("0500000US", food_disparity_county_level$fips)

# adding geo ID for mapping
food_disparity_county_level$geo_id <- paste0("0500000US", food_disparity_county_level$fips)

base_marker <- list(
  line = list(width = 0)
)

# for column names
label_dict <- c(
  PovertyRate = 'Poverty Rate',
  MedianFamilyIncome = 'Median Family Income',
  percent_low_access = "Percent of People with Low Access to Food",
  percent_houses_SNAP = "Percent of Households Receiving SNAP",
  percent_house_no_vehicle_access = "Percent of Households Without Vehicle Access"
)

overweight_label_dict <- c(mean_prev = 'Estimated Obesity Prevalence (%)',
                           percent_change ='Estimated Increase in Obesity (%)')

# county level

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
                 
                 # Map Page
                 tabPanel("Overweight Map",
                          fluidPage(
                            titlePanel("Overweight Prevalence Map"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("map_gender", "Select Gender",
                                            choices = c('Female', 'Male', 'Both'), selected = 'Female'),
                                selectInput("map_age", "Select Age Group",
                                            choices = unique(state_level$age_group_name), selected = '2 to 4'),
                                selectInput('map_metric', 'Choose a Metric',
                                            choices = c('Estimated Obesity Prevalence (%)' = 'mean_prev',
                                                        'Estimated Increase in Obesity (%)' = 'percent_change'),
                                            selected = 'mean_prev')
                              ),
                              mainPanel(
                                plotlyOutput("mapPlot")
                              )
                            )
                          )
                 ),
                 
                 # Histogram Page
                 tabPanel("Overweight Histogram",
                          fluidPage(
                            titlePanel("Distribution of Overweight Prevalence"),
                            p("The histogram displays the distirbution of overweight prevalence among different demographic groups in the United States. Users can select age groups and gender to examine how frequently certain overweight rates occur. This visualization helps identify whether overweight prevalence tends to cluster within certain ranges and reveals differences between population subgroups."),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("hist_gender", "Select Gender",
                                            choices = c('Female', 'Male', 'Both'), selected = 'Female'),
                                selectInput("hist_age", "Select Age Group",
                                            choices = unique(state_level$age_group_name), selected = '2 to 4'),
                                sliderInput("hist_year", "Select Year:",
                                            min = min(data$year_id), max = max(data$year_id),
                                            value = 2000, step = 1, sep = "")
                              ),
                              mainPanel(
                                plotlyOutput("histPlot")
                              )
                            )
                          )
                 ),
                 
                 # Time Series Page
                 tabPanel("Time Series",
                          fluidPage(
                            titlePanel("Overweight Prevalence Over Time"),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("ts_gender", "Select Gender",
                                            choices = c('Female', 'Male', 'Both'), selected = 'Female'),
                                selectInput("ts_age", "Select Age Group",
                                            choices = unique(state_level$age_group_name), selected = '2 to 4'),
                                selectInput("ts_states", "Select States to Display",
                                            choices = sort(unique(state_level$state)),
                                            selected = c("California", "New York"),
                                            multiple = TRUE),
                                actionButton("ts_select_all_states", "Select All States"),
                                actionButton("ts_deselect_all_states", "Deselect All States")
                              ),
                              mainPanel(
                                plotlyOutput("timeSeriesPlot")
                              )
                            )
                          )
                 ),
                 
                 # Summary Stats Page
                 tabPanel("Summary Statistics",
                          fluidPage(
                            titlePanel("Summary Statistics"),
                            p("Operating summary statistics: Select 
                      multiple age groups or genders to compare trends across groups."), 
                            p("The summary statistics section provides an overview of the data, including the following values: minimum, maximum, mean, standard devation, median, and count. The data is organized by select years and age groups. Compared to the plots, this helps quantify the variation found across different demographics and time periods, offering a 'snapshot' of the dataset's characteristics."),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("sum_gender", "Select Gender",
                                            choices = c('Female', 'Male', 'Both'), selected = 'Female', multiple = TRUE),
                                selectInput("sum_age", "Select Age Group",
                                            choices = unique(state_level$age_group_name), selected = '2 to 4', multiple = TRUE),
                                selectInput("sum_states", "Select States to Display",
                                            choices = sort(unique(state_level$state)),
                                            selected = c("California", "New York"), multiple = TRUE),
                                actionButton("sum_select_all_states", "Select All States"),
                                actionButton("sum_deselect_all_states", "Deselect All States")
                              ),
                              mainPanel(
                                DT::dataTableOutput("summaryTable")
                              )
                            )
                          )
                 ),
                 
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
                 ), # end tab panel
                 # page 3: Food Scarcity
                 tabPanel('Food Scarcity',
                          fluidPage(
                            titlePanel('Measures of Poverty and Access to Food by Geographic Location'),
                            
                            # Sidebar with a inputs
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("variable",
                                            label = "Select Variable",
                                            choices = c('Poverty Rate (%)'='PovertyRate',
                                                        'Median Family Income'='MedianFamilyIncome',
                                                        "Percent of Population with Low Access to Food" = "percent_low_access",
                                                        "Percent of Households Receiving SNAP" = "percent_houses_SNAP",
                                                        "Percent of Households Without Vehicle Access" = "percent_house_no_vehicle_access"),
                                            selected = 'PovertyRate'),
                                selectInput("geo",
                                            label = "Select Geographic Region",
                                            choices = c('State', 'County'),
                                            selected = 'State')),
                              # Plots
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Map", plotlyOutput("mapDisparities"))
                                )
                              ) # end main panel
                            ) # end side bar layout
                          ) # end page
                 ) # end tab panel
                 
 ) # end ui


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Map of Overweight trends
    output$mapPlot <- renderPlotly({
      #Filtering the data
      plot_data <- state_level %>%
        filter(sex == input$map_gender & age_group_name == input$map_age)
      
      overweight_label = overweight_label_dict[input$map_metric]
      #Creating map plot
      plot_ly(data = plot_data,
              z = plot_data[[input$map_metric]],
              frame = ~year_id,
              locations = ~state_code,
              type = "choropleth",
              locationmode = "USA-states",
              colorscale = "Reds",
              colorbar = list(title = "Prevalence", tickformat = ".0%"),
              text = ~paste0(location_name, "<br>",
                             round(plot_data[[input$map_metric]] * 100, 2), "%"),
              marker = list(line = list(color = "black", width = 0.5)),
              hoverinfo = "text",
              hoverlabel = list(bgcolor = "#000080")) %>%
        layout(title = paste(overweight_label, "by State"),
               geo = list(scope = "usa"))
    })
    
    # Histogram
    output$histPlot <- renderPlotly({
      plot_data <- state_level %>%
        filter(sex == input$hist_gender,
               year_id == input$hist_year,
               age_group_name == input$hist_age)
      
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
    
    # Time Series Plot
    output$timeSeriesPlot <- renderPlotly({
      req(input$ts_states)
      
      plot_data <- state_level %>%
        filter(sex == input$ts_gender,
               age_group_name == input$ts_age,
               state %in% input$ts_states)
      
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
    
    # Observer for select all states
    observeEvent(input$ts_select_all_states, {
      updateSelectInput(session, "ts_states",
                        selected = sort(unique(state_level$state)))
    })
    
    # Observer for deselect all states
    observeEvent(input$ts_deselect_all_states, {
      updateSelectInput(session, "ts_states", selected = character(0))
    })
    
    # Summary Table
    output$summaryTable <- DT::renderDataTable({
      summary_data <- state_level %>%
        filter(
          state %in% input$sum_states,
          sex %in% input$sum_gender,
          age_group_name %in% input$sum_age
        )
      
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
        rename(
          Year = year_id,
          Gender = sex,
          `Age Group` = age_group_name
        )
      
      DT::datatable(summary_stats,
                    options = list(pageLength = 15, autoWidth = TRUE),
                    rownames = FALSE)
      
    })
    # Select all states for Summary Statistics
    observeEvent(input$sum_select_all_states, {
      updateSelectInput(session, "sum_states", selected = sort(unique(state_level$state)))
    })
    
    # Deselect all states for Summary Statistics
    observeEvent(input$sum_deselect_all_states, {
      updateSelectInput(session, "sum_states", selected = character(0))
    })
}

# Run the application
shinyApp(ui = ui, server = server)


