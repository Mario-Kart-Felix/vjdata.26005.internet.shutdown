pacman::p_load('tidyverse','dplyr','ggplot2', 'bbplot2', 'lubridate', 'grid')
source(prepare_dataset.R)

tidy_data_geography <- tidy_data %>%
  select(ID, year, month, continent, country, geo_scope, shutdown_type_new, full.or.service.based, protest) %>%
  arrange(month)
