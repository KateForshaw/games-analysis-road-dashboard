#Task 2.2
#Kate Forshaw
#25259628

#find file path
file.choose()

#load CSV file
library(readr)
road_data_2021 <- read_csv("/Users/kateforshaw/Downloads/dft-road-casualty-statistics-collision-2021.csv")

#install dplyr
library(dplyr)

#show first few rows to understand data
head(road_data_2021)

#count missing values per column
road_data_2021 %>% summarise(across(everything(), ~ sum(is.na(.))))

#impute missing values using mutate and ifelse
#if value NA, replace with mean (na.rm = TRUE)
rd21_clean <- road_data_2021 %>%
  mutate(
    location_easting_osgr = ifelse(is.na(location_easting_osgr), mean(location_easting_osgr, na.rm = TRUE), location_easting_osgr)
  )

#show first rows and re-count NA
head(rd21_clean_head <- rd21_clean)
rd21_clean %>% summarise(across(everything(), ~ sum(is.na(.))))

#drop duplicate rows
rd21_distinct <- rd21_clean %>% distinct()

#confirm deduplication and view first rows
nrow(rd21_distinct)
head(rd21_distinct)

#delete irrelevant columns
rd21_reduce <- rd21_distinct %>%
  select(-collision_year, -collision_ref_no, -location_easting_osgr, -location_northing_osgr, -local_authority_district, -local_authority_highway, -first_road_class, -first_road_number, -second_road_class, -second_road_number, -pedestrian_crossing_human_control_historic, -pedestrian_crossing_physical_facilities_historic, -carriageway_hazards_historic, -trunk_road_flag, -lsoa_of_accident_location, -collision_adjusted_severity_serious, -collision_adjusted_severity_slight)

#check deletion
names(rd21_reduce)

#load metadata
install.packages("readxl")
library(readxl)
road_metadata <- read_excel("/Users/kateforshaw/Library/CloudStorage/OneDrive-EdgeHillUniversity/Data Analytics MSc - Kate/Programming DS & AI/road collision metadata.xlsx")

#install janitor package
install.packages("janitor")
library(janitor)

#combine metadata to clean, distinct, and reduced road dataset
road_metadata_clean <- road_metadata %>% #clean and prepare metadata
  clean_names() %>% #standardize column names
  rename( #rename field_name and code_formt
    variable = field_name,
    code = code_format
  ) %>%
  mutate (
    code = as.character(code) #ensure code is stored as character
  ) %>%
  filter(!is.na(code), !is.na(variable), !is.na(label)) #only keep rows with valid code, variable, and label

codebook <- road_metadata_clean %>% #build codebook for pre-variable mapping of codes to labels
  group_by(variable) %>%
  summarise(
    mapping = list(setNames(label, code)), #create named vector
    .groups = "drop"
  )

data_variables <- intersect(codebook$variable, names(rd21_reduce)) #identify variables in both metadata and reduced dataset

codebook_filter <- codebook %>% #only look up variables that exist in rd21_reduce
  filter(variable %in% data_variables)

rd21_recode <- rd21_reduce %>% #recode using codebook mappings
  mutate(across(
    all_of(codebook_filter$variable), #apply to relevant variables
    ~ {
      mapping <- codebook_filter$mapping[[match(cur_column(), codebook_filter$variable)]]
      recoded <- mapping[as.character(.)] #replace code with labels
      ifelse(is.na(recoded), as.character(.), recoded) #keep original if no match
    }
  )) 

#load ggplot2 library
library(ggplot2)

#plot bar chart for accidents by severity across different road types
rt_cs_count <- rd21_recode %>% #count combination of road type and collision severity
  count(road_type, collision_severity)

ggplot(rt_cs_count, aes(x = road_type, y = n, fill = collision_severity)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "Fatal" = "red",
    "Serious" = "orange",
    "Slight" = "yellow"
  )) +
  labs(title = "Accidents by Severity and Road Type",
       x = "Road Type",
       y = "Number of Accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust =1))

#create scatter plot of relationship between weather, road surface, and accident frequency
wc_rsc_count <- rd21_recode %>% #count accidents by weather conditions and road surface conditions
  group_by(weather_conditions, road_surface_conditions) %>%
  summarise(weather_surface_accidents = n(), .groups = "drop")

ggplot(wc_rsc_count, aes(x = weather_conditions,
                        y = road_surface_conditions,
                        size = weather_surface_accidents,
                        color = weather_surface_accidents)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Accident Frequency by Weather and Road Surface",
       x = "Weather Condition",
       y = "Road Surface",
       size = "Accident Count",
       color = "Accident Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#create dot map of accident density in Greater Manchester
gm_accidents <- rd21_recode %>%
  filter(police_force == "Greater Manchester") #filter to Greater Manchester police force

ggplot(gm_accidents, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.3, color = "purple", size = 1) +
  labs(title = "Accident Density in Greater Manchester",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

#create heatmap of accident density in Devon and Cornwall
dc_accidents <- rd21_recode %>%
  filter(police_force == "Devon and Cornwall") #filter to Devon and Cornwall police force

ggplot(dc_accidents, aes(x = longitude, y = latitude)) +
  stat_density2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.5) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Accident Density Heatmap of Devon & Cornwall",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

#plot line chart of relationship between average casualties and number of vehicles
cas_ver_summary <- rd21_recode %>% #summarise average casualties for each vehicle count
  group_by(number_of_vehicles) %>%
  summarise(avg_casualties = mean(number_of_casualties), .group = "drop")

ggplot(cas_ver_summary, aes(x = number_of_vehicles, y = avg_casualties)) +
  geom_line(color = "grey", size = 1.2) +
  geom_point(color = "black", size = 2) +
  labs(title = "Average Casualties by Number of Vehicles",
       x = "Number of Vehicles Involved",
       y = "Average Number of Casualties") +
  theme_minimal()
