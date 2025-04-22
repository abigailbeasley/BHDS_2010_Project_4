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
library(scales) # 
library(usmap) # plot data
library(sf) # more plot data
library(tigris) # for plot data
library(plotly) # interactive plots
library(jsonlite) # for reading in JSON files (map data)
library(DT) # for interactive tables
library(bslib) # for dashboard theme
library(lintr) # formatting code to tidyverse specifications

## ------------------------ OVERWEIGHT POP DATA --------------------------------

file.exists("OverweightPrevalenceData.csv")

# Overweight Populations Data
data <- read.csv("OverweightPrevalenceData.csv")

# adding percent change in obesity YoY
data <- data %>%
  arrange(location_name, sex, age_group_name, year_id) %>%
  group_by(location_name, sex, age_group_name) %>%
  mutate(percent_change = (mean_prev - lag(mean_prev)) / lag(mean_prev) * 100)

## ------------------------ FOOD ACCESS DATA -----------------------------------

# on state level
food_disparity_state_level <- read.csv("food_disparities_state_agg.csv")

# on county level
food_disparity_county_level <- read.csv("food_disparities_county_agg.csv")

# making sure fips load in correctly and are strings
# converting to character for plotting
food_disparity_county_level$fips <- as.character(food_disparity_county_level$fips)

# some fips are missing trailing 0--adding it in

# fips are loaded in as numeric, so trailing zeros disappear
# adding them back in wherever they are missing (when fips has 4 chars instead of 5)
food_disparity_county_level$fips <- ifelse(nchar(food_disparity_county_level$fips) == 4,
  paste0("0", food_disparity_county_level$fips),
  food_disparity_county_level$fips
)


## ------------------------ MAP DATA -------------------------------------------

# In order to draw county lines with the choropleth maps, data published by plotly
# (https://github.com/plotly/datasets) is pulled from github. Documentation is
# available here: https://plotly.com/python/choropleth-maps/

# loading GeoJSON data for county maps
geojson_url <- "https://cdn.jsdelivr.net/gh/plotly/datasets@master/geojson-counties-fips.json"

# reading in JSON file
geojson_data <- fromJSON(geojson_url,
  simplifyVector = FALSE
)


# in order to get county lines, data was

# adding geo-data
# Make sure geo_id column exists
food_disparity_county_level$geo_id <- paste0("0500000US", food_disparity_county_level$fips)

# adding geo ID for mapping
food_disparity_county_level$geo_id <- paste0("0500000US", food_disparity_county_level$fips)

# for column names
label_dict <- c(
  PovertyRate = "Poverty Rate",
  MedianFamilyIncome = "Median Family Income",
  percent_low_access = "Percent of People with Low Access to Food",
  percent_houses_SNAP = "Percent of Households Receiving SNAP",
  percent_house_no_vehicle_access = "Percent of Households Without Vehicle Access"
)

overweight_label_dict <- c(
  mean_prev = "Estimated Obesity Prevalence (%)",
  percent_change = "Estimated Increase in Obesity (%)"
)

# county level

# dict of labels for input display and plot titles
overweight_label_dict <- c(
  mean_prev = "Estimated Obesity Prevalence (%)",
  percent_change = "Estimated Increase in Obesity (%)"
)

## ------------------------ MORTALITY DATA -------------------------------------


# Diabetes Mortality Data
diabetes_mortality <- read.csv("diabetes_mortality_2019.csv")

# Filter to state-level data and add state codes
state_mortality <- diabetes_mortality %>%
  filter(location_level == "State") %>%
  mutate(state_code = state.abb[match(location_name, state.name)])

# Filter to state-level data and add state codes
county_mortality <- diabetes_mortality %>%
  filter(location_level == "County")

# adding geo ID for mapping
county_mortality$geo_id <- paste0("0500000US", county_mortality$fips)


# Filtering on state level
state_level <- data %>%
  filter(location_level == "State")

# Adding state variable
state_level$state <- state_level$location_name

# Adding state code for plotly map
state_level$state_code <- state.abb[match(state_level$location_name, state.name)]

# State-to-Region Mapping to reduce size of stats table and provide more meaningful results
state_to_region <- data.frame(
  state = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
    "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
    "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
    "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
    "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ),
  region = c(
    "South", "West", "West", "South", "West", "West", "Northeast", "Northeast", "South",
    "South", "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "South", "South",
    "Northeast", "Northeast", "Northeast", "Midwest", "Midwest", "South", "Midwest",
    "West", "Midwest", "West", "Northeast", "Northeast", "West", "Northeast", "South",
    "Midwest", "Midwest", "South", "West", "Northeast", "Northeast", "South",
    "Midwest", "South", "South", "West", "Northeast", "South", "West", "South", "Midwest", "West"
  )
)

# adding state code for plotly map
state_level$state_code <- state.abb[match(state_level$location_name, state.name)]

#### ---------------------- STYLE GUIDELINES -----------------------------------

# background color used to change the plots (same as background specified in
# the bs_theme function)
plot_background_color <- "#101010"

# # Define UI for application that draws a histogram
ui <- navbarPage("Overweight Population Trends",
  # theme from: https://rstudio.github.io/bslib/articles/theming/index.html
  theme = bs_theme(
    bg = "#101010", # background color
    fg = "#FFF", # txt color
    primary = "#E69F00", # accent used in buttons
    secondary = "#0072B2", # for secondary buttons
    success = "#009E73", # color for badges and alerts
    base_font = font_google("Inter") # font color
  ),
  header = tags$style(HTML(
    "table.dataTable tbody td {
                  background-color: #101010 !important;
                  color: #FFF !important;
                  font-family: 'Inter', sans-serif;
                  font-size: 14px;}

                  table.dataTable thead {
                  background-color: #101010 !important;
                  color: #E69F00 !important;
                  font-family: 'Inter', sans-serif;}

                  .dataTables_length select {
                  background-color: #101010;
                  color: #fff;
                  border: 1px solid #444;}

                 "
  )),
  # Combined Summary + Map Page
  tabPanel(
    "Summary & Map",
    fluidPage(
      titlePanel("Summary Statistics and Overweight Prevalence Map"),
      sidebarLayout(
        sidebarPanel(
          h4("Map Settings"),
          # Adding gender selection
          selectInput("combo_map_gender", "Select Gender",
            choices = c("Female", "Male", "Both"), selected = "Female"
          ),
          # Adding age selection
          selectInput("combo_map_age", "Select Age Group",
            choices = unique(state_level$age_group_name), selected = "2 to 4"
          ),
          # Adding metric selection
          selectInput("combo_map_metric", "Choose a Metric",
            choices = c(
              "Estimated Obesity Prevalence (%)" = "mean_prev",
              "Estimated Increase in Obesity (%)" = "percent_change"
            ),
            selected = "mean_prev"
          ),
          hr(),
          h4("Summary Table Settings"),
          # Adding gender selection
          selectInput("combo_sum_gender", "Select Gender",
            choices = c("Female", "Male", "Both"), selected = "Female", multiple = TRUE
          ),
          # Adding age selection
          selectInput("combo_sum_age", "Select Age Group",
            choices = unique(state_level$age_group_name), selected = "2 to 4", multiple = TRUE
          ),
          # Adding region selection instead of state (fixing summary statistics values)
          selectInput("combo_sum_regions", "Select Region(s)",
            choices = unique(state_to_region$region),
            selected = c("South", "West"), multiple = TRUE
          ),
          # Adding select button for region and to toggle between detailed (expanded table) and a summary view (simplified table)
          actionButton("combo_select_all_regions", "Select All Regions"),
          actionButton("combo_deselect_all_regions", "Deselect All Regions"),
          actionButton("toggle_view", "Switch to Summary View")
        ),
        mainPanel(
          fluidRow(
            column(
              12,
              p("Overweight and obesity prevalence in younger demographics within the United States, particularly those between the ages of 2-19 has become an escalating concern in public health. These conitions are influence by a variety of factors including biological, environmental, and behavioral. Additionally, socioeconimic status and access to food only intensify these factors. The rising rates of younger individuals considered overweight and obese increase the risk of chronic conditions such as Type 2 diabetes, cardiovascular diease, and premature mortality later in life (Sanyaolu et al., 2019)."),
              p("The following plots seek to detail trends and analyze the data associted with the increasing number of young obese and overweight individuals in the United States. First, the interactive map below explores state-level patterns of overweight prevalence by age group and gender over time."),
              p("Reference: Sanyaolu, A., Okorie, C., Qi, X., Locke, J., & Rehman, S. (2019). Childhood and Adolescent Obesity in the United States: A Public Health Concern. Global pediatric health, 6, 2333794X19891305."),
              plotlyOutput("combo_mapPlot")
            )
          ),
          # Summary table section
          hr(),
          fluidRow(
            column(
              12,
              p("Operating summary statistics: Select
                      multiple age groups or genders to compare trends across groups. States are grouped into the following regions: Northeast, Midwest, South, and West. Toggle between summary and detailed view to expand or condense the table. Use search for specific years."), p("The summary statistics section provides an overview of the data, including the following values: minimum, maximum, mean, standard devation, median, and count. The data is organized by select years and age groups. Compared to the plots, this helps quantify the variation found across different demographics and time periods, offering a 'snapshot' of the dataset's characteristics."),
              DT::dataTableOutput("combo_summaryTable")
            )
          )
        )
      )
    )
  ),
  # Histogram Page
  tabPanel(
    "Distribution of Overweight Prevalence",
    fluidPage(
      titlePanel("Distribution of Overweight Prevalence"),
      p("The histogram displays the distirbution of overweight prevalence among different demographic groups in the United States. Users can select age groups and gender to examine how frequently certain overweight rates occur. This visualization helps identify whether overweight prevalence tends to cluster within certain ranges and reveals differences between population subgroups."),
      #Sidebar with inputs
      sidebarLayout(
        sidebarPanel(
          selectInput("hist_gender", "Select Gender",
            choices = c("Female", "Male", "Both"), selected = "Female"
          ),
          selectInput("hist_age", "Select Age Group",
            choices = unique(state_level$age_group_name), selected = "2 to 4"
          ),
          sliderInput("hist_year", "Select Year:",
            min = min(data$year_id), max = max(data$year_id),
            value = 2000, step = 1, sep = ""
          )
        ),
        mainPanel(
          plotlyOutput("histPlot")
        )
      )
    )
  ),

  # Time Series Page
  tabPanel(
    "Overweight Populations over Time",
    fluidPage(
      titlePanel("Overweight Prevalence Over Time"),
      p("The time series plot shows how overweight prevalence has changed over time for select age groups and genders. Users are able to observe trends such as rising or falling rates within specific populations. This plot is useful for identifying long-term patterns, comparing increases and decreases amongst states, and the impact of public health initiatives."),
      #Sidebar with inputs
      sidebarLayout(
        sidebarPanel(
          selectInput("ts_gender", "Select Gender",
            choices = c("Female", "Male", "Both"), selected = "Female"
          ),
          selectInput("ts_age", "Select Age Group",
            choices = unique(state_level$age_group_name), selected = "2 to 4"
          ),
          # Update UI to select states instead of regions
          selectInput("states",
            label = "Select States to Display",
            choices = sort(unique(state_level$state)),
            selected = c("California", "New York"),
            multiple = TRUE
          ),
          # Adding select all button
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
  tabPanel(
    "Diabetes Mortality Rates: 2019",
    fluidPage(
      titlePanel("Mortality Rates"),

      # Sidebar with a inputs
      sidebarLayout(
        sidebarPanel(
          selectInput("mort_age",
            label = "Select Age Group",
            choices = unique(state_mortality$age_name),
            selected = "All Ages"
          ),
          selectInput("mort_race",
            label = "Select Race",
            choices = unique(state_mortality$race_name),
            selected = "Total"
          ),
          selectInput("mort_geo",
            label = "Select Geographic Level",
            choices = c("State", "County"),
            selected = "State"
          )
        ),
        # Plots
        mainPanel(
          plotlyOutput("mapMortality"),
          h3("Diabetes Mortality Rates in the U.S."),
          p("One of the most common diseases associated with higher BMI is diabetes, and
                                        it a leading cause of mortality in the U.S.,
                                          especially amongst older populations. As the prevalence of obesity and being overweight
                                          increases, diabetes will likely become more
                                          common and mortality rates will rise as well.

                                          The map above shows the mortality rate (number of people per 100k) that died of diabetes in 2019.
                                          We can see that not only are mortalities not equal across states and counties,
                                          but they also tend to differ based on racial demographics as well.")
        ) # end main panel
      ) # end side bar layout
    ) # end page
  ), # end tab panel
  # page 3: Food Scarcity
  tabPanel(
    "Poverty, Food Scarcity and Economic Inequality: 2010",
    fluidPage(
      titlePanel("Measures of Poverty and Access to Food by Geographic Location"),

      # Sidebar with a inputs
      sidebarLayout(
        sidebarPanel(
          selectInput("variable",
            label = "Select Variable",
            choices = c(
              "Poverty Rate (%)" = "PovertyRate",
              "Median Family Income" = "MedianFamilyIncome",
              "Percent of Population with Low Access to Food" = "percent_low_access",
              "Percent of Households Receiving SNAP" = "percent_houses_SNAP",
              "Percent of Households Without Vehicle Access" = "percent_house_no_vehicle_access"
            ),
            selected = "PovertyRate"
          ),
          selectInput("geo",
            label = "Select Geographic Region",
            choices = c("State", "County"),
            selected = "State"
          ),
          p("Note: County maps take a moment render")
        ),
        # Plots
        mainPanel(
          plotlyOutput("mapDisparities"),
          h4('Top 10 Regions:'),
          plotlyOutput("top10disparities"),
          h3("Median Family Income & Poverty Rates"),
          p(
            "One of the primary predictors of childhood obesity is poverty",
            tags$a("as noted by Kim Eagle",
              href = "https://ihpi.umich.edu/news/low-income-communities-more-likely-face-childhood-obesity",
              target = "_blank"
            ),
            "M.D. at the University of Michigan. Childhood obesity is more common in Hispanic and African-American
                                   children, but Kim Eagle notes that this relationship no longer exists when researchers account for family
                                   income. Therefore, to understand the trajectory of childhood obesity and overweight populations, researchers
                                   must examine trends in poverty and economic inequality at a geographic level."
          ),
          p("\n"),
          h3("Percent of Households Recieving SNAP Benefits"),
          p(),
          p("As of 2010, Oregon had the largest percentage of households using SNAP benefits, despite the fact that
                                   it is not even in the top 10 states in terms of the percentage of people living in poverty. One reason for this
                                   may be the fact that eligibility for SNAP benefits is based on state policies and some states may do more to
                                   fascilitate enrollment than others."),
          h3("Percent of Population with Low Access to Food"),
          p("Food access for this study was defined as:"),
          p("\n"),
          p(
            '"Low-income census tracts where a significant number (at least 500 people) or share (at least 33 percent) of the
                                 population is greater than 1 mile from the nearest supermarket, supercenter, or large grocery store for an urban
                                 area or greater than 10 miles for a rural area. This measure shows that an estimated 18.8 million people, or 6.1
                                 percent of the U.S. population, live in low-income and low access tracts and are more than 1 mile or 10 miles from
                                 a supermarket." ', tags$a("(USDA ERS, 2019)",
              href = "https://www.ers.usda.gov/data-products/food-access-research-atlas/download-the-data",
              target = "_blank"
            )
          ),
          p("\n"),
          h3("Percent of Households without Access to a Vehicle"),
          p("As expected, areas with low vehicle access tend to be cities, which does not necessarily indicate low access to food
                                 as walkability and public transport make grocery stores easily accessible. Families with low access to vehicles in
                                   rural areas where public transport is limited are more likely to find food less accessible. Thus, household accessibility
                                   to a vehicle is an important indicator for understanding access to food."),
          h3("Data Source"),
          p(
            "Please visit ",
            tags$a("the USDA website",
              href = "https://www.ers.usda.gov/data-products/food-access-research-atlas/download-the-data",
              target = "_blank"
            ), # new window
            " to access the raw data."
          )
        ) # end main panel
      ) # end side bar layout
    ) # end page
  ) # end tab panel
) # end ui


# Define server logic
server <- function(input, output, session) {
  # Combined Summary Table with Region Grouping
  output$combo_summaryTable <- DT::renderDataTable({
    # Filtering data based on the selected regions
    summary_data <- state_level %>%
      # Merge the state data with the state-to-region mapping
      left_join(state_to_region, by = "state") %>%
      filter(
        region %in% input$combo_sum_regions, # Filter by selected regions
        sex %in% input$combo_sum_gender,
        age_group_name %in% input$combo_sum_age
      )

    # Create the summary statistics, grouping by region instead of state
    summary_stats <- summary_data %>%
      group_by(year_id, sex, age_group_name, region) %>%
      summarise(
        Count = n(),
        # Adding nsmall, digits, and trim to keep figures displaying at 0.000, removes just 0 from occuring in the table
        Mean = format(mean(mean_prev, na.rm = TRUE), nsmall = 3, digits = 3, trim = TRUE), # Mean
        Median = format(median(mean_prev, na.rm = TRUE), nsmall = 3, digits = 3, trim = TRUE), # Median
        SD = format(sd(mean_prev, na.rm = TRUE), nsmall = 3, digits = 3, trim = TRUE), # SD
        Min = format(min(mean_prev, na.rm = TRUE), nsmall = 3, digits = 3, trim = TRUE), # MInimum
        Max = format(max(mean_prev, na.rm = TRUE), nsmall = 3, digits = 3, trim = TRUE), # Maximum
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
        rownames = FALSE
      )
    } else {
      # Summary view: show key metrics only (Mean, Median, SD, etc.)
      summary_stats_summary <- summary_stats %>%
        select(Year, Gender, `Age Group`, Region = region, Mean, Median, SD)

      # Using DT:: function to create a more visually appearling and interactive table
      DT::datatable(summary_stats_summary,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20, 25, 50, 100),
          autoWidth = TRUE
        ),
        rownames = FALSE
      )
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
    # Filtering data 
      filter(
        sex == input$hist_gender,
        year_id == input$hist_year,
        age_group_name == input$hist_age
      )
    # Creating the histogram plot
    hp <- ggplot(plot_data, aes(x = mean_prev)) +
      geom_histogram(bins = 30, fill = "skyblue") + # Color
      labs(
        title = "Distribution of Overweight Prevalence", # Title
        x = "Mean Prevalence (%)", # x-axis
        y = "Frequency" # y-axis
      ) +
    # Adding theme and changing background to match app
      theme(
        panel.background = element_rect(fill = plot_background_color, color = NA),
        plot.background = element_rect(fill = plot_background_color),
        plot.title = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white")
      )
    # Outputting the plot
    ggplotly(hp)
  })

   # Time Series Plot
  output$timeSeriesPlot <- renderPlotly({
    req(input$states)
    
    # Creating data to plot
    plot_data <- state_level %>%
      filter(
        sex == input$ts_gender,
        age_group_name == input$ts_age,
        state %in% input$states
      )
    
    # Creating the time series plot
    ts_plot <- ggplot(plot_data, aes(x = year_id, y = mean_prev, color = state)) +
      geom_line(size = 1) +
      labs(
        title = "Overweight Prevalence Over Time", # Title
        x = "Year", # x-axis
        y = "Mean Prevalence (%)", # y-axis
        color = "State" # color
      ) +
      # Adding theme and changing background to match the rest of the app
      theme(
        panel.background = element_rect(fill = plot_background_color, color = NA),
        plot.background = element_rect(fill = plot_background_color),
        plot.title = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"), # makes legend labels white too
        legend.background = element_rect(fill = plot_background_color),
        legend.position = "right",
        legend.key = element_rect(fill = plot_background_color, color = NA) # ensures background is uniform
      )
    # Outputting the plot
    ggplotly(ts_plot)
  })
  
  # Select All States button functionality
  observeEvent(input$select_all_states, {
    updateSelectInput(session, "states", selected = unique(state_level$state))
  })
  
  # Deselect All States button functionality
  observeEvent(input$deselect_all_states, {
    updateSelectInput(session, "states", selected = character(0))
  })
  
  # Map of Overweight trends
  output$combo_mapPlot <- renderPlotly({
    # Filter data for selected gender, year, and age group based on user input
    plot_data <- state_level %>%
      filter(sex == input$combo_map_gender & age_group_name == input$combo_map_age)

    overweight_label <- overweight_label_dict[input$combo_map_metric]

    # Note: Checkout gganimate package for animations (might not be interactive)

    # Animated plot of the prevalence of overweight pop
    plot_ly(
      data = plot_data,
      type = "choropleth", # US map with interactive hover
      locationmode = "USA-states", # map of us states
      z = plot_data[[input$combo_map_metric]], # fill metric
      frame = ~year_id, # animation: each frame is a year
      locations = ~state_code, # state abbreviations (e.g., "CA")
      colorscale = "Reds", # colorscale for fill
      colorbar = list(
        title = "Prevalence", # Title of legend
        tickformat = ".0%"
      ), # Adds percentages
      text = ~ paste0(
        location_name, # custom hover text
        "<br>", # makes a line break
        round(plot_data[[input$combo_map_metric]] * 100, 2), "%"
      ), # value to display in hover
      marker = list(line = list(color = "black", width = 0.5)), # adjusting color/width of state outlines
      hoverinfo = "text", # adding hover text to plot
      hoverlabel = list(bgcolor = "#000080")
    ) %>% # change hover background
      layout(
        title = paste(overweight_label, "by State"), # map title
        font = list(color = "#ffffff", size = 12, family = "Arial"), # title font/color
        geo = list(
          scope = "usa", # zoom on US,
          bgcolor = plot_background_color
        ), # fixing plot background
        plot_bgcolor = plot_background_color, # fixing other part of plot background
        paper_bgcolor = plot_background_color
      ) # fixing another part of plot background
  })

  # Map of Diabetes Mortality
  output$mapMortality <- renderPlotly({
    if (input$mort_geo == "State") {
      # Filter data for selected gender, year, and age group
      mort_data_state <- state_mortality %>%
        filter(age_name == input$mort_age & race_name == input$mort_race)

      # Plotting the chorepleth map
      plot_ly(
        data = mort_data_state, z = ~ val * 100000, # mortality count per 100k
        locations = ~state_code, # state abbreviations (e.g., "CA")
        type = "choropleth", # map plot
        locationmode = "USA-states", # US state map
        colorscale = "Reds", # color scheme for fill
        # adding legend formatting
        colorbar = list(title = "Mortality Rate (per 100k)"),
        # customizing state lines
        marker = list(line = list(color = "black", width = 0.5)),
        # custom hover text
        text = ~ paste0(
          location_name, "<br> Mortality Rate (per 100k): ",
          round(val * 100000, 2), ""
        ),
        # adding custom text to plot
        hoverinfo = "text"
      ) %>%
        # adding title, adjusting font, and changing background as well
        # as zooming in on US
        layout(
          title = "Mortality Rates for Diabetes by State (per 100k)",
          font = list(color = "#fff", size = 12, family = "Arial"),
          plot_bgcolor = plot_background_color,
          paper_bgcolor = plot_background_color,
          geo = list(
            scope = "usa",
            bgcolor = plot_background_color
          )
        )
    } else {
      # Filter data for selected gender, year, and age group
      mort_data_county <- county_mortality %>%
        filter(age_name == input$mort_age & race_name == input$mort_race)

      # county level plot
      plot_ly(
        data = mort_data_county,
        type = "choropleth", # specifying plot type
        geojson = geojson_data, # adding data to build county lines
        featureidkey = "properties.GEO_ID", # specifying ID in json file
        locations = ~geo_id, # geoid in mortality data
        z = ~ val * 100000, # adding fill metric
        # custom hover text
        text = ~ paste0(
          location_name, "<br> Mortality Rate (per 100k): ",
          round(val * 100000, 2), ""
        ),
        hoverinfo = "text",
        # adding color scheme for fill
        colorscale = "Reds",
        # formatting legend
        colorbar = list(title = "Deaths per 100k"),
        # custom lines
        marker = list(line = list(width = 1, color = "Black"))
      ) %>%
        # adding title and background color
        layout(
          title = "Mortality Rates for Diabetes by County (per 100k)",
          plot_bgcolor = plot_background_color,
          paper_bgcolor = plot_background_color,
          geo = list(
            scope = "usa", # zoom to USA
            bgcolor = plot_background_color
          )
        )
    }
  })

  # Map of Food Disparity
  output$mapDisparities <- renderPlotly({
    # get pretty label for variable to use for title
    label <- label_dict[[input$variable]]

    # If user wants to see data on state level
    if (input$geo == "State") {
      # state level plot
      plot_ly(
        data = food_disparity_state_level, # data on date level
        type = "choropleth", # type of plot--map with filled in values reflecting variable of interest
        z = food_disparity_state_level[[input$variable]], # getting user-selected variable
        locations = ~state_code, # abbreviations in data to map to US map
        locationmode = "USA-states", # map is of the US
        marker = list(line = list(color = "black", width = 0.5)), # changing line width and color
        colorscale = "Reds", # adding scale for color fill
        # custom hover info
        text = ~ paste0(
          State, "<br>", label, ": ",
          round(food_disparity_state_level[[input$variable]], 2), ""
        ),
        hoverinfo = "text", #  assigning text as custom hover text
        colorbar = list(title = label)
      ) %>%
        layout(
          title = paste(label, "by U.S. State"), # Map title
          font = list(color = "#fff", size = 12, family = "Arial"), # font
          plot_bgcolor = plot_background_color,
          paper_bgcolor = plot_background_color,
          geo = list(
            scope = "usa", # zoom on US
            bgcolor = plot_background_color
          )
        )
    } else { # If user asks for county level
      # county level plot
      plot_ly(
        data = food_disparity_county_level,
        type = "choropleth",
        geojson = geojson_data, # JSON file with data to build counties
        featureidkey = "properties.GEO_ID", # merge key in json file for data
        locations = ~geo_id, # geo file in data to map to counties
        z = food_disparity_county_level[[input$variable]], # variable of interest
        # custom text for hover
        text = ~ paste(
          "Location:", County,
          "<br>", label, ":",
          round(food_disparity_county_level[[input$variable]], 1)
        ),
        hoverinfo = "text",
        colorscale = "Reds", # custom fill gradient
        colorbar = list(title = label), # adding formatted variable name to legend
        marker = list(line = list(width = 1))
      ) %>% # custom lines (very small for counties)
        layout(
          title = paste(label, "by U.S. County"), # Map title
          font = list(color = "#fff", size = 12, family = "Arial"), # font
          geo = list(
            scope = "usa", # zoom on US
            # lines below change white backgrounds to theme color
            bgcolor = plot_background_color
          ),
          plot_bgcolor = plot_background_color,
          paper_bgcolor = plot_background_color
        )
    }
  })

  # adding a table of top 10
  output$top10disparities <- renderPlotly({
    # get pretty label for variable to use for title
    label <- label_dict[[input$variable]]


    # converting input to symbol not string for filtering
    var_sym <- sym(input$variable)

    if (input$geo == "State") {
      # filtering and sorting table data
      table_data <- food_disparity_state_level %>%
        arrange(desc(!!var_sym)) %>%
        mutate(disp_var = comma(!!var_sym)) %>% # adding commas to big nums
        select(State, disp_var) %>%
        head(10)

      # Create plotly table
      plot_ly(
        type = "table",
        header = list(
          values = c("State", label),
          fill = list(color = "salmon"),
          font = list(size = 18, color = "black")
        ),
        cells = list(
          values = list(
            table_data$State,
            table_data$disp_var
          ),
          fill = list(color = plot_background_color),
          font = list(size = 12, color = "fff"),
          size = 60
        )
      ) %>%
        layout(
          plot_bgcolor = plot_background_color,
          paper_bgcolor = plot_background_color
        )
    } else { # county level
      # filtering and sorting table data to get top 10
      table_data <- food_disparity_county_level %>%
        arrange(desc(!!var_sym)) %>% # filtering by selected variable
        mutate(disp_var = comma(!!var_sym)) %>% # adding commas to big nums
        select(State, County, disp_var) %>%
        distinct(County, .keep_all = TRUE) %>%
        head(10) # getting just top 10

      # Create plotly table
      plot_ly(
        type = "table",
        # adding header info/styling
        header = list(
          values = c("State", "County", label),
          fill = list(color = "salmon"),
          font = list(size = 18, color = "black")
        ),
        # adding values
        cells = list(
          values = list(
            table_data$State,
            table_data$County,
            table_data$disp_var
          ), # styling
          fill = list(color = plot_background_color),
          font = list(size = 12, color = "#FFF"),
          size = 60
        )
      ) %>% # adding theme background color
        layout(
          plot_bgcolor = plot_background_color,
          paper_bgcolor = plot_background_color
        )
    }
  })
}
# Run the application
shinyApp(ui = ui, server = server)
