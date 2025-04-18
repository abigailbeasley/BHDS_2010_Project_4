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
library(bslib)

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

# Filter to state-level data and add state codes 
county_mortality <- diabetes_mortality %>%
  filter(location_level == 'County')

# adding geo ID for mapping
county_mortality$geo_id <- paste0("0500000US", county_mortality$fips)

# Filtering on state level
state_level <- data %>%
  filter(location_level == 'State')

# Adding state variable
state_level$state <- state_level$location_name

# adding state code for plotly map
state_level$state_code <- state.abb[match(state_level$location_name, state.name)]

## style guidelines
plot_background_color = "#101010"

# # Define UI for application that draws a histogram
ui <- navbarPage("Overweight Population Trends",
                 theme = bs_theme(bg = "#101010", # background color
                                  fg = "#FFF", # txt color
                                  primary = "#E69F00", # accent used in buttons
                                  secondary = "#0072B2", # for secondary buttons
                                  success = "#009E73", # color for badges and alerts
                                  base_font = font_google("Inter") # font color
                                  ),
                 # page 1: overweight trends
                 tabPanel('Overweight Trends',
                          fluidPage(
#                            tags$head( # styling
#                              tags$style(HTML(".well {
#                                                background-color: #e0f7fa !important;  /* light blue */
#                                                border: none;
 #                                               box-shadow: none;
#                                              }"))),
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
                                selectInput('metric',
                                            label='Choose a Metric',
                                            choices = c('Estimated Obesity Prevalence (%)' = 'mean_prev',
                                                        'Estimated Increase in Obesity (%)' = 'percent_change'),
                                            selected='mean_prev'),
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
                                  tabPanel("Geographic View: US Map", plotlyOutput("mapPlot")),
                                  tabPanel("Histogram", plotlyOutput("histPlot")),
                                  tabPanel("Time Series", plotlyOutput("timeSeriesPlot"))
                                )
                              ) # end main panel
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
                                          selected = 'Total'),
                                selectInput("mort_geo",
                                            label = "Select Geographic Level",
                                            choices = c('State', 'County'),
                                            selected = 'State')),
                              # Plots
                              mainPanel(plotlyOutput("mapMortality")
                                        ) # end main panel
                          ) # end side bar layout
          ) # end page
          ), # end tab panel
          # page 3: Food Scarcity
          tabPanel('Poverty, Food Scarcity and Economic Inequality',
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
                       mainPanel(plotlyOutput("mapDisparities"),
                                 plotlyOutput("top10disparities")
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
      filter(sex == input$gender & age_group_name == input$age)
    
    overweight_label = overweight_label_dict[input$metric]

    # Animated plot of the prevalence of overweight pop
    plot_ly(data = plot_data,
            z = plot_data[[input$metric]],
            frame = ~year_id,
            locations = ~state_code,  # state abbreviations (e.g., "CA")
            type = "choropleth",
            locationmode = "USA-states",
            colorscale = "Reds",
            colorbar = list(title = "Prevalence",
                            tickformat = ".0%"),
                            text = ~paste0(location_name,
                            "<br>",
                            round(plot_data[[input$metric]] * 100, 2), "%"),
            marker = list(line = list(color = "black", width = 0.5)),
            hoverinfo = "text",
            hoverlabel=list(bgcolor = "#000080")) %>%
            layout(title = paste(overweight_label, "by State"),
                   font = list(color = "#ffffff",size = 12, family = "Arial"),
                   geo = list(scope = "usa",
                              bgcolor = plot_background_color),
                   plot_bgcolor = plot_background_color,
                   paper_bgcolor = plot_background_color)
  })
  
  # Map of Diabetes Mortality
  output$mapMortality <- renderPlotly({
    
    if(input$mort_geo == 'State') {
      # Filter data for selected gender, year, and age group
      mort_data_state <- state_mortality %>%
        filter(age_name==input$mort_age & race_name==input$mort_race)
      
      # Plotting the chorepleth map
      plot_ly(data = mort_data_state, z = ~val * 100000, # mortality count per 100k
              locations = ~state_code,  # state abbreviations (e.g., "CA")
              type = "choropleth", locationmode = "USA-states",
              colorscale = "Reds",
              colorbar = list(title = "Mortality Rate (per 100k)"),
              marker = list(line = list(color = "black", width = 0.5)),
              text = ~paste0(location_name, "<br> Mortality Rate (per 100k): ", 
                             round(val * 100000, 2), ""),
              hoverinfo = "text") %>%
        layout(title = "Mortality Rates for Diabetes by State (per 100k)",
               font = list(color = "#ffffff",size = 12, family = "Arial"),
               plot_bgcolor = plot_background_color,
               paper_bgcolor=plot_background_color,
               geo = list(scope = "usa",
                          bgcolor = plot_background_color))
      
    } else {
      # Filter data for selected gender, year, and age group
      mort_data_county <- county_mortality %>%
        filter(age_name==input$mort_age & race_name==input$mort_race)
      
      # county level plot 
      plot_ly(data = mort_data_county,
              type = "choropleth",
              geojson = geojson_data,
              featureidkey = "properties.GEO_ID",
              locations = ~geo_id,
              z = ~val * 100000,
              text = ~paste0(location_name, "<br> Mortality Rate (per 100k): ", 
                             round(val * 100000, 2), ""),
              hoverinfo = "text",
              colorscale = "Reds",
              colorbar = list(title = "Deaths per 100k"),
              marker = list(line = list(width = 0))) %>%
        layout(title = paste(label, 'by U.S. County'),
               plot_bgcolor = plot_background_color,
               paper_bgcolor=plot_background_color,
               
               geo = list(scope = "usa",
                          bgcolor = plot_background_color))
      
    }
    
  })
  
  # Map of Food Disparity
  output$mapDisparities <- renderPlotly({
    
    # get pretty label for variable to use for title
    label <- label_dict[[input$variable]]
    
    # If user wants to see data on state level
    if (input$geo == 'State') {
      # state level plot
      plot_ly(data = food_disparity_state_level, # data on date level
              type = "choropleth", # type of plot--map with filled in values reflecting variable of interest
              z = food_disparity_state_level[[input$variable]], # getting user-selected variable
              locations = ~state_code, # abbreviations in data to map to US map
              locationmode = "USA-states", # map is of the US
              marker = list(line = list(color = "black", width = 0.5)), # changing line width and color
              colorscale = "Reds", # adding scale for color fill
              # custom hover info
              text = ~paste0(State,"<br>", label, ': ', 
                             round(food_disparity_state_level[[input$variable]], 2), ""),
              hoverinfo = "text", #  assigning text as custom hover text
              colorbar=list(title=label)) %>%
        layout(title = paste(label, 'by U.S. State'),  # Map title 
               font = list(color = "#fff",size = 12, family = "Arial"), #font
               plot_bgcolor = plot_background_color,
               paper_bgcolor=plot_background_color,
               geo = list(scope = "usa", # zoom on US
                          bgcolor = plot_background_color))
    } else { # If user asks for county level
      # county level plot 
      plot_ly(data = food_disparity_county_level,
              type = "choropleth",
              geojson = geojson_data, # JSON file with data to build counties
              featureidkey = "properties.GEO_ID", # merge key in json file for data
              locations = ~geo_id, # geo file in data to map to counties
              z = food_disparity_county_level[[input$variable]], # variable of interest
              # custom text for hover
              text = ~paste("Location:", County,
                            "<br>", label, ":",
                            round(food_disparity_county_level[[input$variable]], 1)),
              hoverinfo = "text",
              colorscale = "Reds", #custom fill gradient
              colorbar = list(title = label), # adding formatted variable name to legend
              marker = list(line = list(width = 1))) %>% # custom lines (very small for counties)
        layout(title = paste(label, 'by U.S. County'), # Map title 
               font = list(color = "#fff",size = 12, family = "Arial"), # font
               geo = list(scope = "usa", # zoom on US
                          bgcolor = plot_background_color),
               plot_bgcolor = plot_background_color,
               paper_bgcolor=plot_background_color)
      
    }
  })
  
  # adding a table of top 10
  output$top10disparities <- renderPlotly({
    
    # get pretty label for variable to use for title
    label <- label_dict[[input$variable]]
    
    
    # converting input to symbol not string for filtering
    var_sym <- sym(input$variable)
    
    if (input$geo == 'State') {
      
      # filtering and sorting table data
      table_data = food_disparity_state_level %>%
        arrange(desc(!!var_sym)) %>% 
        mutate(disp_var = comma(!!var_sym)) %>% # adding commas to big nums
        select(State, disp_var) %>%
        head(10)
      
      # Create plotly table
      plot_ly(type = 'table',
              header = list(values = c("State", label),
                            fill = list(color = "#E69F00"),
                            font = list(size = 18, color = "black")),
        cells = list(values = list(table_data$State,
                                   table_data$disp_var),
                     fill = list(color = plot_background_color),
                     font = list(size = 12, color = "fff"),
                     size=60)) %>% 
        layout( plot_bgcolor = plot_background_color,
                paper_bgcolor=plot_background_color)
      
    } else { # county level
      # filtering and sorting table data
      table_data = food_disparity_county_level %>%
        arrange(desc(!!var_sym)) %>% 
        mutate(disp_var = comma(!!var_sym)) %>% # adding commas to big nums
        select(State, disp_var) %>%
        head(10)
      
      # Create plotly table
      plot_ly(type = 'table',
              header = list(values = c("State", label),
                            fill = list(color = "peach"),
                            font = list(size = 18, color = "black")),
              cells = list(values = list(table_data$State,
                                         table_data$disp_var),
                           fill = list(color = background_color),
                           font = list(size = 12, color = "#FFF"),
                           size=60)) %>% 
        layout(plot_bgcolor = plot_background_color,
               paper_bgcolor=plot_background_color
        )
      
    }
  })
    
  
  #Creating the interactive histogram
  
  #Filtering data for plot.
  output$histPlot <- renderPlotly({
    plot_data <- state_level %>%
      filter(sex == input$gender & year_id == input$year & age_group_name == input$age)
    
    # Creating histogram using ggplot and plotly. 
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


