#Task 2.3
#Kate Forshaw
#25259628

#import accidents dataset
library(readr)
road_accident_dataset <- read.csv("/Users/kateforshaw/Downloads/dft-road-casualty-statistics-collision-2021.csv")

#clean accidents dataset
library(tidyverse) #load tidyverse

glimpse(road_accident_dataset)
summary(road_accident_dataset)
head(road_accident_dataset) #inspect the dataset

install.packages("janitor")
library(janitor) #load janitor

road_accident_dataset <- road_accident_dataset %>% clean_names() #clean names

road_accident_dataset <- road_accident_dataset %>% drop_na() #remove rows with missing values

road_accident_dataset <- road_accident_dataset %>% distinct() #remove duplicates

road_accident_dataset <- road_accident_dataset %>% select(-collision_year, -collision_ref_no, -location_easting_osgr, -location_northing_osgr, -local_authority_district, -local_authority_highway, -first_road_class, -first_road_number, -second_road_class, -second_road_number, -pedestrian_crossing_human_control_historic, -pedestrian_crossing_physical_facilities_historic, -carriageway_hazards_historic, -trunk_road_flag, -lsoa_of_accident_location, -collision_adjusted_severity_serious, -collision_adjusted_severity_slight) #remove unnecessary columns

#load vehicles dataset
vehicle_dataset <- read.csv("/Users/kateforshaw/Downloads/dft-road-casualty-statistics-vehicle-2021.csv")

#clean vehicle dataset
glimpse(vehicle_dataset)
summary(vehicle_dataset)
head(vehicle_dataset) #inspect the dataset

vehicle_dataset <- vehicle_dataset %>% clean_names() #clean names

vehicle_dataset <- vehicle_dataset %>% drop_na() #remove rows with missing values

vehicle_dataset <- vehicle_dataset %>% distinct() #remove duplicates

vehicle_dataset <- vehicle_dataset %>% select(-collision_year, -vehicle_manoeuvre_historic, -vehicle_location_restricted_lane_historic, -journey_purpose_of_driver_historic, -escooter_flag, -driver_distance_banding) #remove uneccessary columns

#join accidents and vehicle dataset together
library(dplyr) #load dplyr

accident_vehicle_joined <- vehicle_dataset %>% #one row per vehicle
  left_join(road_accident_dataset, by = "collision_index") #with accident details added

#load metadata
install.packages("readxl")
library(readxl)
road_safety_metadata <- read_excel("/Users/kateforshaw/Library/CloudStorage/OneDrive-EdgeHillUniversity/Data Analytics MSc - Kate/Programming DS & AI/road collision metadata.xlsx")

#combine metadata to joined accident and vehicle dataset
rsm_clean <- road_safety_metadata %>% #clean and prepare metadata
  clean_names() %>% #standardize column names
  rename( #rename field_name and code_formt
    variable = field_name,
    code = code_format
  ) %>%
  mutate (
    code = as.character(code) #ensure code is stored as character
  ) %>%
  filter(!is.na(code), !is.na(variable), !is.na(label)) #only keep rows with valid code, variable, and label

codebook <- rsm_clean %>% #build codebook for pre-variable mapping of codes to labels
  group_by(variable) %>%
  summarise(
    mapping = list(setNames(label, code)), #create named vector
    .groups = "drop"
  )

data_variables <- intersect(codebook$variable, names(accident_vehicle_joined)) #identify variables in both metadata and joined dataset

codebook_filter <- codebook %>% #only look up variables that exist in accident_vehicle_joined
  filter(variable %in% data_variables)

accident_vehicle_recode <- accident_vehicle_joined %>% #recode using codebook mappings
  mutate(across(
    all_of(codebook_filter$variable), #apply to relevant variables
    ~ {
      mapping <- codebook_filter$mapping[[match(cur_column(), codebook_filter$variable)]]
      recoded <- mapping[as.character(.)] #replace code with labels
      ifelse(is.na(recoded), as.character(.), recoded) #keep original if no match
    }
  )) 

#create metrics of recoded accident and vehicle dataset
library(lubridate) #load lubridate

accident_vehicle_metrics <- accident_vehicle_recode %>%
  mutate(
   
    #vehicle type
    car = grepl("car|cars", vehicle_type, ignore.case = TRUE), #grepl selects rows containing specifc words
    motorcycle = grepl("motorcycle", vehicle_type, ignore.case = TRUE),
    bus = grepl("bus|minibus", vehicle_type, ignore.case = TRUE),
    hgv = grepl("goods", vehicle_type, ignore.case = TRUE),
    bike = vehicle_type == "Pedal cycle",
    tractor = vehicle_type == "Agricultural vehicle",
    tram = vehicle_type == "Tram",
    mobility_scooter = vehicle_type == "Mobility scooter",
    other_vehicle = grepl("other|self|missing", vehicle_type, ignore.case = TRUE),
    
    #vehicle manoeuvre
    reversing = vehicle_manoeuvre == "Reversing",
    parking = grepl("parked|parking", vehicle_manoeuvre, ignore.case = TRUE),
    waiting = grepl("waiting", vehicle_manoeuvre, ignore.case = TRUE),
    stopping = vehicle_manoeuvre == "Slowing or stopping",
    moving_forward = grepl("moving|ahead", vehicle_manoeuvre, ignore.case = TRUE),
    turning = grepl("u-turn|turning", vehicle_manoeuvre, ignore.case = TRUE),
    changing_lane = grepl("changing", vehicle_manoeuvre, ignore.case = TRUE),
    overtaking = grepl("overtaking", vehicle_manoeuvre, ignore.case = TRUE),
    unknown_manoeuvre = grepl("unknown|missing", vehicle_manoeuvre, ignore.case = TRUE),
    
    #junction location
    no_junction = grepl("not|unknown|missing", junction_location, ignore.case = TRUE),
    approaching = junction_location == "Approaching junction or waiting/parked at junction approach", 
    cleared = junction_location == "Cleared junction or waiting/parked at junction exit",
    leaving = grepl("leaving", junction_location, ignore.case = TRUE),
    entering = grepl("entering", junction_location, ignore.case = TRUE),
    mid_junction = junction_location == "Mid Junction - on roundabout or on main road",
    
    #skidding and overturning
    no_skidding = grepl("none|unknown|missing", skidding_and_overturning, ignore.case = TRUE),
    skidded = grepl("skidded", skidding_and_overturning, ignore.case = TRUE),
    overturned = grepl("overturned", skidding_and_overturning, ignore.case = TRUE),
    jackknifed = grepl("jackknifed", skidding_and_overturning, ignore.case = TRUE),
    
    #hit object in/off carriageway
    did_not_hit_object = grepl("none|missing|unknown", hit_object_in_carriageway, ignore.case = TRUE) |
      grepl("none|missing|unknown", hit_object_off_carriageway, ignore.case = TRUE),
    hit_object = grepl("other|previous|road|bridge|bollard|vehicle|roundabout|kerb|animal|lamp|pole|tree|bus|barrier|water|ditch|wall", hit_object_in_carriageway, ignore.case = TRUE) |
      grepl("other|previous|road|bridge|bollard|vehicle|roundabout|kerb|animal|lamp|pole|tree|bus|barrier|water|ditch|wall", hit_object_off_carriageway, ignore.case = TRUE),
    
    #age of driver
    young_driver = age_of_driver < 25,
    middle_aged_driver = age_of_driver >= 25 & age_of_driver <= 65,
    old_driver = age_of_driver > 65,
    
    #age of vehicle
    old_car = age_of_vehicle > 15,
    young_car = age_of_vehicle < 16,
    
    #propulsion_code
    petrol = grepl("petrol", propulsion_code, ignore.case = TRUE),
    oil = propulsion_code == "Heavy oil",
    electric = grepl("electric", propulsion_code, ignore.case = TRUE),
    steam = propulsion_code == "Steam",
    gas = grepl("gas", propulsion_code, ignore.case = TRUE),
    diesel = grepl("diesel", propulsion_code, ignore.case = TRUE),
    other_propulsion = grepl("Undefined|cells|new", propulsion_code, ignore.case = TRUE)
  )

head(accident_vehicle_metrics) #check metrics

#install and load required packages
install.packages("shiny")
install.packages("shinythemes")
install.packages("plotly")
install.packages("DT")

library(shiny) #for build
library(shinythemes) #for design
library(ggplot2) #for visualisation
library(plotly) #for hover labels on plots
library(DT) #for hover labels on tables
library(tidyr) #for reshaping

#extract hour from time column for summary
accident_vehicle_metrics <- accident_vehicle_metrics %>%
  mutate(hour = as.numeric(substr(time, 1, 2)))

#create function for mode (most appeared word in column)
get_mode <- function(x) {
  x %>%
    na.omit() %>%
    table() %>% #how many times each unique value appears
    which.max() %>% #finds the most frequent one
    names() #extracts the label
}

#build interactive shiny dashboard
server <- function(input, output, session) { #server logic
  
  #reactive filtering
  filtered_data <- reactive({ #automatically re-runs whenever the user changes any filter
    data <- accident_vehicle_metrics #starts with full dataset
    
    #vehicle filters using predefined metrics
    if (input$vehicle_filter == "Car") {
      data <- data %>% filter(car)
    }
    if (input$vehicle_filter == "Motorcycle") {
      data <- data %>% filter(motorcycle)
    }
    if (input$vehicle_filter == "Bus") {
      data <- data %>% filter(bus)
    }
    if (input$vehicle_filter == "HGV") {
      data <- data %>% filter(hgv)
    }
    if (input$vehicle_filter == "Bike") {
      data <- data %>% filter(bike)
    }
    if (input$vehicle_filter == "Tractor") {
      data <- data %>% filter(tractor)
    }
    if (input$vehicle_filter == "Tram") {
      data <- data %>% filter(tram)
    }
    if (input$vehicle_filter == "Mobility scooter") {
      data <- data %>% filter(mobility_scooter)
    }
    if (input$vehicle_filter == "Other") {
      data <- data %>% filter(other_vehicle)
    }
    
    #manoeuvre filters using predefined metrics
    if (input$manoeuvre_filter == "Reversing") {
      data <- data %>% filter(reversing)
    }
    if (input$manoeuvre_filter == "Parking") {
      data <- data %>% filter(parking)
    }
    if (input$manoeuvre_filter == "Waiting") {
      data <- data %>% filter(waiting)
    }
    if (input$manoeuvre_filter == "Stopping") {
      data <- data %>% filter(stopping)
    }
    if (input$manoeuvre_filter == "Moving forward") {
      data <- data %>% filter(moving_forward)
    }
    if (input$manoeuvre_filter == "Turning") {
      data <- data %>% filter(turning)
    }
    if (input$manoeuvre_filter == "Changing lane") {
      data <- data %>% filter(changing_lane)
    }
    if (input$manoeuvre_filter == "Overtaking") {
      data <- data %>% filter(overtaking)
    }
    if (input$manoeuvre_filter == "Unknown") {
      data <- data %>% filter(unknown_manoeuvre)
    }
    
    #junction location filters using predefined metrics
    if (input$junction_location_filter == "No junction") {
      data <- data %>% filter(no_junction)
    }
    if (input$junction_location_filter == "Approaching") {
      data <- data %>% filter(approaching)
    }
    if (input$junction_location_filter == "Cleared") {
      data <- data %>% filter(cleared)
    }
    if (input$junction_location_filter == "Leaving") {
      data <- data %>% filter(leaving)
    }
    if (input$junction_location_filter == "Entering") {
      data <- data %>% filter(entering)
    }
    if (input$junction_location_filter == "Mid-junction") {
      data <- data %>% filter(mid_junction)
    }
    
    #skidding filters using predefined metrics
    if (input$skidding_filter == "No skidding") {
      data <- data %>% filter(no_skidding)
    }
    if (input$skidding_filter == "Skidded") {
      data <- data %>% filter(skidded)
    }
    if (input$skidding_filter == "Overturned") {
      data <- data %>% filter(overturned)
    }
    if (input$skidding_filter == "Jackknifed") {
      data <- data %>% filter(jackknifed)
    }
    
    #hit object filters using predefined metrics
    if (input$hit_object_filter == "Did hit object") {
      data <- data %>% filter(hit_object)
    }
    if (input$hit_object_filter == "Did not hit object") {
      data <- data %>% filter(did_not_hit_object)
    }
    
    #age of driver filters using predefined metrics
    if (input$age_of_driver_filter == "Young") {
      data <- data %>% filter(young_driver)
    }
    if (input$age_of_driver_filter == "Middle-aged") {
      data <- data %>% filter(middle_aged_driver)
    }
    if (input$age_of_driver_filter == "Old") {
      data <- data %>% filter(old_driver)
    }
    
    #age of vehicle filters using predefined metrics
    if (input$age_of_vehicle_filter == "Young") {
      data <- data %>% filter(young_car)
    }
    if (input$age_of_vehicle_filter == "Old") {
      data <- data %>% filter(old_car)
    }
    
    #propulsion filters using predefined metrics
    if (input$propulsion_filter == "Petrol") {
      data <- data %>% filter(petrol)
    }
    if (input$propulsion_filter == "Oil") {
      data <- data %>% filter(oil)
    }
    if (input$propulsion_filter == "Electric") {
      data <- data %>% filter(electric)
    }
    if (input$propulsion_filter == "Steam") {
      data <- data %>% filter(steam)
    }
    if (input$propulsion_filter == "Gas") {
      data <- data %>% filter(gas)
    }
    if (input$propulsion_filter == "Diesel") {
      data <- data %>% filter(diesel)
    }
    if (input$propulsion_filter == "Other") {
      data <- data %>% filter(other_propulsion)
    }
    data #return filtered dataset
  })
  
  #reactive summary table
  summary_reactive <- reactive({
    data <- filtered_data() #use filtered data
    data %>%
      summarise(
        total_crashes = n_distinct(collision_index),
        mode_severity = get_mode(collision_severity),
        mode_hour = get_mode(hour),
        mode_weather = get_mode(weather_conditions),
        mode_lad = get_mode(local_authority_ons_district)
      )
  })

  output$summary <- renderTable(summary_reactive()) #output summary table to user interface
  
  # reactive & interactive bar plot for time (hour)
  output$hour_plot <- renderPlotly({
    data <- filtered_data() %>%
      mutate(hour = as.numeric(substr(time, 1, 2))) %>% #ensure numeric
      count(hour) #creates hour + n
    plot_ly(
      data = data,
      x = ~hour,
      y = ~n,
      type = "bar",
      marker = list(color = "purple")
    ) %>%
      layout(
        title = "Crashes by Hour",
        xaxis = list(title = "Hour"),
        yaxis = list(title = "Count")
      )
  })
  
  
  #reactive severity table
  output$severity_table <- renderTable({
    filtered_data() %>%
      count(collision_severity) %>%
      mutate(
        percent = round(n / sum(n) * 100, 1)
      )
  })
  
  #reactive & interactive bar chart for weather conditions
  output$weather_bar <- renderPlotly({
    weather_count <- filtered_data() %>% count(weather_conditions)
    
   plot_ly(
      data = weather_count,
      x = ~n,
      y = ~weather_conditions,
      type = "bar",
      orientation = "h",
      marker = list(color = "lightskyblue")
    ) %>%
      layout(
        title = "Weather Conditions",
        xaxis = list(title = "Count"),
        yaxis = list(title = "Weather")
      )
  })
  
  #reactive & interactive tree map of severity distribution
  output$severity_treemap <- renderPlotly({
    severity_count <- filtered_data() %>% count(collision_severity)
    
    plot_ly(
      data = severity_count,
      labels = ~collision_severity,
      values = ~n,
      type = "treemap",
      textinfo = "label+value+percent entry",
      parents = NA,
      marker = list(
        colors = c(
          "Fatal" = "red",
          "Serious" = "orange",
          "Slight" = "yellow"
        )[severity_count$collision_severity]
      )
    ) %>%
      layout(title = "Distribution of Severity")
  })
  
  #reactive & interactive scatter plot of crash locations (longitude and latitude)
  output$long_lat_map <- renderPlotly({
    plot_ly(
      data = filtered_data(),
      x = ~longitude,
      y = ~latitude,
      type = "scatter",
      mode = "markers",
      marker = list(color = "forestgreen", size = 5, opacity = 0.4)
    ) %>%
      layout(
        title = "Crash Locations",
        xaxis = list(title = "Longitude"),
        yaxis = list(title = "Latitude"),
        yaxis = list(scaleanchor = "x", scaleratio = 1)  #keeps map aspect ratio
      )
  })
  
  #reactive local authority district (lad) table
  output$lad_table <- renderTable({
    filtered_data() %>%
      count(local_authority_ons_district, sort = TRUE) %>%
      mutate(
        percent = round(n / sum(n) * 100, 1)
      )
  })
  
  #reactive weather table
  output$weather_table <- renderTable({
    filtered_data() %>%
      count(weather_conditions) %>%
      mutate(
        percent = round(n / sum(n) * 100, 1)
      )
  })
  
  #reactive time (by hour) table
  output$time_table <- renderTable({
    filtered_data() %>%
      mutate(hour = as.numeric(substr(time, 1, 2))) %>%
      count(hour) %>%
      mutate(
        percent = round(n / sum(n) * 100, 1)
      )
  })
  
  #reactive and interactive summary lollipop chart
  output$mode_lollipop <- renderPlotly({
    summary_vals <- summary_reactive() #retrieve summary table
    mode_data <- tibble::tibble( 
      category = c("Severity", "Hour", "Weather", "LAD"), #build table for lollipop chart
      value = c(
        summary_vals$mode_severity, #extract mode values from summary table
        summary_vals$mode_hour,
        summary_vals$mode_weather,
        summary_vals$mode_lad
      ),
      col = c("orange", "purple", "lightskyblue", "forestgreen") #assign corresponding colours to individual plots
    )
    p <- plot_ly() #start empty plotly object
    for (i in seq_len(nrow(mode_data))) {
      p <- p %>% 
        add_trace( #add line segement
          x = c(0, mode_data$value[i]), #from 0 to mode value
          y = c(mode_data$category[i], mode_data$category[i]), 
          type = "scatter", 
          mode = "lines", 
          line = list(width = 4, color = mode_data$col[i]), #category specific colour
          showlegend = FALSE
        ) %>%
        add_trace( #add dot at the end of the line
          x = mode_data$value[i], 
          y = mode_data$category[i], 
          type = "scatter", 
          mode = "markers", 
          marker = list(size = 14, color = mode_data$col[i]), #same colour as line
          showlegend = FALSE
        )
    }
       p %>%
        layout(
          title = "Most Frequent Severity, Hour, Weather, and LAD",
          xaxis = list(title = "Mode Value"),
          yaxis = list(title = "")
        )
  })
}

#user interface (ui)
ui <- fluidPage( #defines what user sees and interacts with
  theme = shinytheme("flatly"), #clean modern theme
  titlePanel("UK Collision Trends & Insights 2021"),
  
  #sidebar and main content layout
  sidebarLayout(
    sidebarPanel(
      h3("Filters"), #sidebar with user inputs
      selectInput("vehicle_filter", "Vehicle:",
                  choices = c("All", "Car", "Motorcycle", "Bus", "HGV", "Bike", "Tractor", "Tram", "Mobility scooter", "Other")),
      selectInput("manoeuvre_filter", "Manoeuvre:",
                  choices = c("All", "Reversing", "Parking", "Waiting", "Stopping", "Moving forward", "Turning", "Changing lane", "Overtaking", "Unknown")),
      selectInput("junction_location_filter", "Junction location:",
                  choices = c("All", "No junction", "Approaching", "Cleared", "Leaving", "Entering", "Mid-junction")),
      selectInput("skidding_filter", "Skidding:",
                  choices = c("All", "No skidding", "Skidded", "Overturned", "Jackknifed")),
      selectInput("hit_object_filter", "Hit object:",
                  choices = c("All", "Did hit object", "Did not hit object")),
      selectInput("age_of_driver_filter", "Age of Driver:",
                  choices = c("All", "Young", "Middle-aged", "Old")),
      selectInput("age_of_vehicle_filter", "Age of Vehicle:",
                  choices = c("All", "Young", "Old")),
      selectInput("propulsion_filter", "Propulsion:",
                  choices = c("All", "Petrol", "Oil", "Electric", "Steam", "Gas", "Diesel", "Other"))
    ),
    mainPanel(
      tabsetPanel(
        
        #overview tab
        tabPanel("Overview",
                 #summary row
                 fluidRow(
                   column(12,
                          h3("Summary"),
                          tableOutput("summary")
                   )
                 ),
                 tags$hr(),
                 fluidRow(
                   12,
                   plotlyOutput("mode_lollipop")
                 )
        ),
        
        #severity tab
        tabPanel("Severity",
                 fluidRow(
                   column(12, plotlyOutput("severity_treemap"))
                 ),
                 tags$hr(), #seperates table and plot
                 fluidRow(
                   column(12, 
                          h3("Severity Breakdown"),
                          tableOutput("severity_table"))
                 )
        ),
        
        #time tab
        tabPanel("Time",
                 fluidRow(
                   column(12, plotlyOutput("hour_plot"))
                 ),
                 tags$hr(),
                 fluidRow(
                   column(12,
                          h3("Hourly Breakdown"),
                          tableOutput("time_table"))
                 )
        ),
        
        #weather tab
        tabPanel("Weather",
                 fluidRow(
                   column(12, plotlyOutput("weather_bar"))
                 ),
                 tags$hr(),
                 fluidRow(
                   column(12,
                          h3("Weather Breakdown"),
                          tableOutput("weather_table"))
                 )
        ),
        
        #location tab
        tabPanel("Location",
                 fluidRow(12, plotlyOutput("long_lat_map")),
                 tags$hr(),
                 fluidRow(
                   column(12, 
                          h3("Location Breakdown"),
                          tableOutput("lad_table"))
                 )
        )
      )
    )
  )
)

shinyApp(ui, server) #run the app