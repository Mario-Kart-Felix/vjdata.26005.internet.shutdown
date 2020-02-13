pacman::p_load('tidyverse','dplyr','ggplot2', 'bbplot2', 'lubridate', 'grid')

source(prepare_dataset.R)


###### by geographic scope ######


# countries and number of shutdowns by level of geographic spread
geography_scope_count <- tidy_data_geography %>%
  group_by(continent, country, geo_scope) %>%
  count(geo_scope) %>%
  arrange(country) %>%
  spread(geo_scope, n)

# this step is only necessary if you notice 2 Level 3 columns. Not sure why it happens
# geography_scope_count <- geography_scope_count %>%  
#   mutate("Level 3" = sum(`Level 3` + `Level 3 `))

geography_scope_count[is.na(geography_scope_count)] <-0

geography_scope_forchart <- tidy_data_geography %>%
  group_by(continent, country, geo_scope) %>%
  count(geo_scope) %>%
  arrange(country) %>%
  group_by(geo_scope) %>%
  summarise(total = sum(n))


# Plot
scope.plot <- ggplot(geography_scope_forchart,
                     aes(x=geo_scope,
                         y=total,
                         #group = continent,
                         fill = "#4D2C7A")) +
  geom_bar(stat = "identity",
           position = "identity") +
  bbc_style() +
  scale_fill_manual(values = c("#1380A1")) +
  scale_y_continuous(limits = c(0,125)) +
  scale_x_discrete("month",
                   labels= c("Local","Regional","Multi-regional")) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
  labs(title = "Most shutdowns are targeted to specific areas")

scope.plot

finalise_plot(scope.plot,
              source = "Source: Access Now, 2019",
              save_filepath = "~/Dropbox (BBC)/Visual Journalism/Data/2019/vjdata.26005.internet.shutdown/output/shutdowns_by_geo_scope-nc.png",
              width_pixels = 640)
