pacman::p_load('tidyverse','dplyr','ggplot2', 'bbplot2', 'lubridate')

##### new shutdowns by month #####

tidy_data_2019new <- tidy_data %>%
  filter(year == 2019) %>%
  select(ID, year, month, continent, country, geo_scope, shutdown_type_new, full.or.service.based) %>%
  arrange(month)

tidy_data_2019new <- months(tidy_data_2019new)

tidy_data_2019new_forchart <- tidy_data_2019new %>%
  group_by(year, month, month2) %>%
  count(month) %>%
  arrange(month)

names(tidy_data_2019new_forchart) <- c("year","month", "month2","count")
tidy_data_2019new_forchart$month <-factor(tidy_data_2019new_forchart$month) 

month.plot <- ggplot(tidy_data_2019new_forchart,
                     aes(x=month,
                         y=count,
                         fill = "#4D2C7A")) +
  geom_bar(stat = "identity",
           position = "identity") +
  bbc_style() +
  scale_fill_manual(values = c("#1380A1")) +
  scale_y_continuous(limits = c(0,40)) +
  scale_x_discrete("month",
                   labels= c("Jan","","Mar","","May","","Jul","","Sep","","Nov","")) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
  labs(title = "June saw the most new shutdowns in 2019")

month.plot

finalise_plot(month.plot,
              source = "Source: Access Now, 2019",
              save_filepath = "~/Dropbox (BBC)/Visual Journalism/Data/2019/vjdata.26005.internet.shutdown/output/shutdowns_by_month-nc.png",
              width_pixels = 640)

##### geography #####

tidy_data_geography <- tidy_data %>%
  #filter( == 2019) %>%
  select(ID, year, month, continent, country, geo_scope, shutdown_type_new, full.or.service.based, protest) %>%
  arrange(month)

tidy_data_geography <- months(tidy_data_geography)

geography_country_count <- tidy_data_geography %>%
  group_by(continent, country) %>%
  count(country) %>%
  arrange(desc(n))

geography_scope_count <- tidy_data_geography %>%
  group_by(continent, country, geo_scope) %>%
  count(geo_scope) %>%
  arrange(country) %>%
  spread(geo_scope, n)

geo.plot <- ggplot(tidy_data_geography_forchart,
                     aes(x=month,
                         y=count,
                         fill = "#4D2C7A")) +
  geom_bar(stat = "identity",
           position = "identity") +
  bbc_style() +
  scale_fill_manual(values = c("#1380A1")) +
  scale_y_continuous(limits = c(0,40)) +
  scale_x_discrete("month",
                   labels= c("Jan","","Mar","","May","","Jul","","Sep","","Nov","")) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
  labs(title = "June saw the most new shutdowns in 2019")

geo.plot

finalise_plot(month.plot,
              source = "Source: Access Now, 2019",
              save_filepath = "~/Dropbox (BBC)/Visual Journalism/Data/2019/vjdata.26005.internet.shutdown/output/shutdowns_by_month-nc.png",
              width_pixels = 640)
