
# Required packages -------------------------------------------------------

library(tidyverse)
library(janitor)
library(here)

library(viridis)
library(lubridate)


# data loading ------------------------------------------------------------


lw_data <- read_csv(here("data", "lake_winnipeg_chemistry_2002_2004.csv")) %>% 
  clean_names()

glimpse(lw_data)


# splitting the dataset ---------------------------------------------------

## Sites

sites_temp <- lw_data %>% 
  select(monitoring_location_id,
         monitoring_location_name,
         monitoring_location_latitude,
         monitoring_location_longitude,
         monitoring_location_horizontal_coordinate_reference_system,
         monitoring_location_waterbody) %>%
  distinct(monitoring_location_id, .keep_all = TRUE)

## Sample log

sample_log_temp <- lw_data %>%
  select(monitoring_location_id,
         activity_type,
         activity_media_name,
         activity_start_date,
         activity_start_time,
         activity_depth_height_measure,
         activity_depth_height_unit,
         laboratory_sample_id) %>% 
  distinct(laboratory_sample_id, .keep_all = TRUE)

## Results

results_temp <- lw_data %>%
  select(characteristic_name,
         method_speciation, 
         result_sample_fraction,
         result_value,
         result_unit,
         result_detection_condition,
         result_detection_quantitation_limit_measure,
         result_detection_quantitation_limit_unit,
         result_detection_quantitation_limit_type,
         result_status_id,
         result_comment,
         result_analytical_method_id,
         result_analytical_method_context,
         result_analytical_method_name,
         analysis_start_date,
         analysis_start_time,
         analysis_start_time_zone,
         laboratory_name,
         laboratory_sample_id)


# Data processing ---------------------------------------------------------

sites <- sites_temp %>% 
  mutate(basin = case_when(
    monitoring_location_latitude <= 51.738173 ~ "South basin",
    TRUE ~ "North basin"))

sample_log <- sample_log_temp %>% 
  mutate(day = day(activity_start_date),
         month = month(activity_start_date),
         year = year(activity_start_date),
         hour = hour(activity_start_time),
         minute = minute(activity_start_time)) %>% 
  select(-activity_start_date, -activity_start_time) %>% 
  rename(sample_id = laboratory_sample_id) %>% 
  select(sample_id, everything()) %>% 
  filter(activity_depth_height_measure == 0 |
           is.na(activity_depth_height_measure))


sample_log%>% 
  group_by(sample_id) %>% 
  distinct()


results <- results_temp %>% 
  rename(param = characteristic_name) %>% 
  filter(param == "Nitrate" |
           param == "Nitrite" |
           param == "Ammonia" |
           param == "Total Nitrogen, mixed forms" |
           param == "Soluble Reactive Phosphorus (SRP)" |
           param == "Total Phosphorus, mixed forms") %>% 
  select(param, everything(), -method_speciation) %>% 
  filter(!is.na(result_value)) %>% 
  rename(concentration = result_value,
         units = result_unit) %>% 
  select(laboratory_sample_id, param, concentration) %>% 
  rename(sample_id = laboratory_sample_id) %>% 
  distinct(sample_id, param, .keep_all = TRUE) %>% 
  pivot_wider(names_from = param, values_from = concentration, values_fill = NA ) %>% 
  clean_names()


samples_2002 <- sample_log %>% 
  select(sample_id, year) %>% 
  group_split(year) %>% 
    .[[1]]

samples_2004 <- sample_log %>% 
  select(sample_id, year) %>% 
  group_split(year) %>% 
  .[[2]]

  
results_2002 <- results %>% 
  semi_join(samples_2002)

results_2004 <- results %>% 
  semi_join(samples_2004)
  
  
# Save files --------------------------------------------------------------

write_csv(sites, here("data", "sites.csv"))

write_csv(sample_log, here("data", "sample_log.csv"))

write_csv(results_2002, here("data", "results_2002.csv"))

write_csv(results_2004, here("data", "results_2004.csv"))


  