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

# Adding state code for plotly map
state_level$state_code <- state.abb[match(state_level$location_name, state.name)]

# State-to-Region Mapping
state_to_region <- data.frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
            "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
            "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
            "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
            "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  
  region = c("South", "West", "West", "South", "West", "West", "Northeast", "Northeast", "South", 
             "South", "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "South", "South", 
             "Northeast", "Northeast", "Northeast", "Midwest", "Midwest", "South", "Midwest", 
             "West", "Midwest", "West", "Northeast", "Northeast", "West", "Northeast", "South", 
             "Midwest", "Midwest", "South", "West", "Northeast", "Northeast", "South", 
             "Midwest", "South", "South", "West", "Northeast", "South", "West", "South", "Midwest", "West")
  
)

# # Define UI for application that draws a histogram
ui <- navbarPage("Overweight Population Trends",
                 
                 # Combined Summary + Map Page
                 tabPanel("Summary & Map",
                          fluidPage(
                            titlePanel("Summary Statistics and Overweight Prevalence Map"),
                            sidebarLayout(
                              sidebarPanel(
                                h4("Map Settings"),
                                selectInput("combo_map_gender", "Select Gender",
                                            choices = c('Female', 'Male', 'Both'), selected = 'Female'),
                                selectInput("combo_map_age", "Select Age Group",
                                            choices = unique(state_level$age_group_name), selected = '2 to 4'),
                                selectInput('combo_map_metric', 'Choose a Metric',
                                            choices = c('Estimated Obesity Prevalence (%)' = 'mean_prev',
                                                        'Estimated Increase in Obesity (%)' = 'percent_change'),
                                            selected = 'mean_prev'),
                                hr(),
                                h4("Summary Table Settings"),
                                #Adding gender selection
                                selectInput("combo_sum_gender", "Select Gender",
                                            choices = c('Female', 'Male', 'Both'), selected = 'Female', multiple = TRUE),
                                #Adding age selection
                                selectInput("combo_sum_age", "Select Age Group",
                                            choices = unique(state_level$age_group_name), selected = '2 to 4', multiple = TRUE),
                                #Adding region selection instead of state (fixing summary statistics values)
                                selectInput("combo_sum_regions", "Select Region(s)",
                                            choices = unique(state_to_region$region),
                                            selected = c("South", "West"), multiple = TRUE),
                                actionButton("combo_select_all_regions", "Select All Regions"),
                                actionButton("combo_deselect_all_regions", "Deselect All Regions"),
                                actionButton("toggle_view", "Switch to Summary View")
        
                              ),
                              mainPanel(
                                fluidRow(
                                  column(12,
                                         p("Overweight and obesity prevalence in younger demographics within the United States, particularly those between the ages of 2-19 has become an escalating concern in public health. These conitions are influence by a variety of factors including biological, environmental, and behavioral. Additionally, socioeconimic status and access to food only intensify these factors. The rising rates of younger individuals considered overweight and obese increase the risk of chronic conditions such as Type 2 diabetes, cardiovascular diease, and premature mortality later in life."),
                                         p("The following plots seek to detail trends and analyze the data associted with the increasing number of young obese and overweight individuals in the United States. First, the interactive map below explores state-level patterns of overweight prevalence by age group and gender over time."),       
                                         plotlyOutput("combo_mapPlot"))
                                ),
                                hr(),
                                fluidRow(
                                  column(12, 
                                         p("Operating summary statistics: Select 
                      multiple age groups or genders to compare trends across groups. States are grouped into the following regions: Northeast, Midwest, South, and West. Toggle between summary and detailed view to expand or condense the table. Use search for specific years."), p("The summary statistics section provides an overview of the data, including the following values: minimum, maximum, mean, standard devation, median, and count. The data is organized by select years and age groups. Compared to the plots, this helps quantify the variation found across different demographics and time periods, offering a 'snapshot' of the dataset's characteristics."),     
                                         DT::dataTableOutput("combo_summaryTable"))
                                )
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
                 tabPanel("Overweight Time Series",
                          fluidPage(
                            titlePanel("Overweight Prevalence Over Time"),
                            p("The time series plot shows how overweight prevalence has changed over time for select age groups and genders. Users are able to observe trends such as rising or falling rates within specific populations. This plot is useful for identifying long-term patterns, comparing increases and decreases amongst states, and the impact of public health initiatives."),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("ts_gender", "Select Gender",
                                            choices = c('Female', 'Male', 'Both'), selected = 'Female'),
                                selectInput("ts_age", "Select Age Group",
                                            choices = unique(state_level$age_group_name), selected = '2 to 4'),
                                # Update UI to select regions instead of individual states
                                selectInput("states",
                                            label = "Select States to Display",
                                            choices = sort(unique(state_level$state)),
                                            selected = c("California", "New York"),  
                                            multiple = TRUE),
                                actionButton("select_all_states", "Select All States"),
                                actionButton("deselect_all_states", "Deselect All States")
                              ),
                              mainPanel(
                                plotlyOutput("timeSeriesPlot")
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
  
  # Combined Map Plot
  output$combo_mapPlot <- renderPlotly({
    plot_data <- state_level %>%
      #Filtering data
      filter(sex == input$combo_map_gender & age_group_name == input$combo_map_age)
    
    overweight_label = overweight_label_dict[input$combo_map_metric]
  # Creating plot and altering data for settings to match
    plot_ly(data = plot_data,
            z = plot_data[[input$combo_map_metric]],
            frame = ~year_id,
            locations = ~state_code,
            type = "choropleth",
            locationmode = "USA-states",
            colorscale = "Reds",
            colorbar = list(title = "Prevalence", tickformat = ".0%"),
            text = ~paste0(location_name, "<br>",
                           round(plot_data[[input$combo_map_metric]] * 100, 2), "%"),
            marker = list(line = list(color = "black", width = 0.5)),
            hoverinfo = "text",
            hoverlabel = list(bgcolor = "#000080")) %>%
      layout(title = paste(overweight_label, "by State"),
             geo = list(scope = "usa"))
  })
    
   # Combined Summary Table with Region Grouping
output$combo_summaryTable <- DT::renderDataTable({
  # Filtering data based on the selected regions
  summary_data <- state_level %>%
    # Merge the state data with the state-to-region mapping
    left_join(state_to_region, by = "state") %>%
    filter(
      region %in% input$combo_sum_regions,  # Filter by selected regions
      sex %in% input$combo_sum_gender,
      age_group_name %in% input$combo_sum_age
    )

  # Create the summary statistics, grouping by region instead of state
  summary_stats <- summary_data %>%
    group_by(year_id, sex, age_group_name, region) %>%
    summarise(
      Count = n(),
      Mean = format(mean(mean_prev, na.rm = TRUE), nsmall = 3, digits = 3, trim = TRUE),
      Median = format(median(mean_prev, na.rm = TRUE), nsmall = 3, digits = 3, trim = TRUE),
      SD = format(sd(mean_prev, na.rm = TRUE), nsmall = 3, digits = 3, trim = TRUE),
      Min = format(min(mean_prev, na.rm = TRUE), nsmall = 3, digits = 3, trim = TRUE),
      Max = format(max(mean_prev, na.rm = TRUE), nsmall = 3, digits = 3, trim = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      Year = year_id,
      Gender = sex,
      `Age Group` = age_group_name
    )

  # Conditionally modify the columns based on the toggle button state
  if (input$toggle_view %% 2 == 1) {
    # Detailed view: show all columns, including 'region'
    DT::datatable(summary_stats,
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(5, 10, 15, 20, 25, 50, 100),
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  } else {
    # Summary view: show key metrics only (Mean, Median, SD, etc.)
    summary_stats_summary <- summary_stats %>%
      select(Year, Gender, `Age Group`, Region = region, Mean, Median, SD)
    
    DT::datatable(summary_stats_summary,
                  options = list(
                    pageLength = 10,
                    lengthMenu = c(5, 10, 15, 20, 25, 50, 100),
                    autoWidth = TRUE
                  ),
                  rownames = FALSE)
  }
})

  
  # Toggle Button Action: Switch the view between detailed and summary
  observeEvent(input$toggle_view, {
    # Toggle the button text based on the current view
    if (input$toggle_view %% 2 == 1) {
      updateActionButton(session, "toggle_view", label = "Switch to Summary View")
    } else {
      updateActionButton(session, "toggle_view", label = "Switch to Detailed View")
    }
  })
  
  # When the "Select All Regions" button is pressed, select all regions
  observeEvent(input$combo_select_all_regions, {
    updateSelectInput(session, "combo_sum_regions", selected = unique(state_to_region$region))
  })
  
  # When the "Deselect All Regions" button is pressed, deselect all regions
  observeEvent(input$combo_deselect_all_regions, {
    updateSelectInput(session, "combo_sum_regions", selected = character(0))
  })
  
    # Interactive histogram
    output$histPlot <- renderPlotly({
      plot_data <- state_level %>%
        filter(sex == input$hist_gender,
               year_id == input$hist_year,
               age_group_name == input$hist_age)
    # Creating the histogram plot
      hp <- ggplot(plot_data, aes(x = mean_prev)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
        labs(
          title = "Distribution of Overweight Prevalence",
          x = "Mean Prevalence (%)",
          y = "Frequency"
        ) +
        theme_minimal()
    # Outputting the plot  
      ggplotly(hp)
    })
    
    # Time Series Plot
    output$timeSeriesPlot <- renderPlotly({
      req(input$states)  # ✅ Use correct input ID
      
      # Creating data to plot  
      plot_data <- state_level %>%
        filter(sex == input$ts_gender,
               age_group_name == input$ts_age,
               state %in% input$states)  # ✅ Use correct input ID
      
      # Creating the time series plot  
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
    
    # Observe for select all states
    observeEvent(input$ts_select_all_states, {
      updateSelectInput(session, "ts_states",
                        selected = sort(unique(state_level$state)))
    })
    
    # Observe for deselect all states
    observeEvent(input$ts_deselect_all_states, {
      updateSelectInput(session, "ts_states", selected = character(0))
    })
    
    # Map of Diabetes Mortality
    output$mapMortality <- renderPlotly({
      
      # Filter data for selected gender, year, and age group
      mort_data <- state_mortality %>%
        filter(age_name==input$mort_age & race_name==input$mort_race)
      
      # Plotting the chorepleth map
      plot_ly(data = mort_data, z = ~val * 100000, # mortality count per 100k
              locations = ~state_code,  # state abbreviations (e.g., "CA")
              type = "choropleth", locationmode = "USA-states",
              colorscale = "Reds",
              colorbar = list(title = "Mortality Rate (Per 100k)"),
              marker = list(line = list(color = "black", width = 0.5)),
              text = ~paste0(location_name, "<br> Mortality Rate (per 100k People): ", 
                             round(val * 100000, 2), ""),
              hoverinfo = "text") %>%
        layout(title = "Prevalence of Overweight Populations by State",
               geo = list(scope = "usa"))
    })
    
    # Map of Food Disparity
    output$mapDisparities <- renderPlotly({
      
      # get label
      label <- label_dict[[input$variable]]
      if (is.null(label)) label <- input$variable  # fallback
      
      if (input$geo == 'State') {
        plot_ly(data = food_disparity_state_level,
                z = food_disparity_state_level[[input$variable]],
                locations = ~state_code, type = "choropleth",
                marker = list(line = list(color = "black", width = 0.5)),
                locationmode = "USA-states", colorscale = "Reds",
                text = ~paste0(State,"<br>", label, ': ',
                               round(food_disparity_state_level[[input$variable]], 2), ""),
                hoverinfo = "text") %>%
          layout(title = paste(label, 'by U.S. State'),
                 geo = list(scope = "usa"))
      } else { # county level
        # Now build the plot
        plot_ly(data = food_disparity_county_level,
                type = "choropleth",
                geojson = geojson_data,
                featureidkey = "properties.GEO_ID",
                locations = ~geo_id,
                z = ~PovertyRate,
                text = ~paste("Location:", County,
                              "<br>", label, ":", round(PovertyRate, 1)),
                hoverinfo = "text",
                colorscale = "Reds",
                colorbar = list(title = "Death Count"),
                marker = list(line = list(width = 0))) %>%
          layout(title = paste(label, 'by U.S. County'),
                 geo = list(scope = "usa",
                            showlakes = TRUE,
                            lakecolor = "white"))
        
      }
      
    })
}

# Run the application
shinyApp(ui = ui, server = server)


