pacman::p_load('tidyverse','dplyr','ggplot2', 'bbplot2', 'lubridate', 'grid')

source(prepare_dataset.R)

# get data and filter to relevant geographic columns
tidy_data_geography <- tidy_data %>%
  select(ID, year, month, continent, country, geo_scope) %>%
  arrange(month)

tidy_data_geography <- months(tidy_data_geography)

# number of shutdowns by country
geography_country_count <- tidy_data_geography %>%
  group_by(continent, country) %>%
  count(country) %>%
  arrange(desc(n))


###############################################################

# number of shutdowns by continent
geography_continent_count <- tidy_data_geography %>%
  group_by(continent) %>%
  count(continent) %>%
  arrange(desc(n))


# Plot
continent.plot <- ggplot(geography_continent_count,
                     aes(x=reorder(continent,-n),
                         y=n,
                         #group = continent,
                         fill = "#4D2C7A")) +
  geom_bar(stat = "identity",
           position = "identity") +
  bbc_style() +
  scale_fill_manual(values = c("#1380A1")) +
  scale_y_continuous(limits = c(0,150)) +
  scale_x_discrete("continent",
                   labels= c("Asia","MENA","Africa","South\nAmerica","Europe")) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
  labs(title = "Including India, Asia had the most shutdowns",
     subtitle = "Total number of shutdowns in 2019 by continent")

continent.plot

finalise_plot(continent.plot,
              source = "Source: Access Now, 2019",
              save_filepath = "~/Dropbox (BBC)/Visual Journalism/Data/2019/vjdata.26005.internet.shutdown/output/shutdowns_by_continent-nc.png",
              width_pixels = 640)



###############################################################

# remove India for comparison
noindia_continent_count <- tidy_data_geography %>%
  filter(country != "India ") %>%
  group_by(continent) %>%
  count(continent) %>%
  arrange(desc(n))


# Plot2
noindia.continent.plot <- ggplot(noindia_continent_count,
                         aes(x=reorder(continent,-n),
                             y=n,
                             #group = continent,
                             fill = "#4D2C7A")) +
  geom_bar(stat = "identity",
           position = "identity") +
  bbc_style() +
  scale_fill_manual(values = c("#1380A1")) +
  scale_y_continuous(limits = c(0,30)) +
  scale_x_discrete("continent",
                   labels= c("MENA","Africa","Asia", "South\nAmerica","Europe")) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
  labs(title = "Without India, Arab states used shutdowns most frequently",
       subtitle = "Total number of shutdowns in 2019 by continent")

noindia.continent.plot

finalise_plot(noindia.continent.plot,
              source = "Source: Access Now, 2019",
              save_filepath = "~/Dropbox (BBC)/Visual Journalism/Data/2019/vjdata.26005.internet.shutdown/output/shutdowns_by_continent_noindia-nc.png",
              width_pixels = 640)



###############################################################

# Treat India as its own continent
india_as_continent <- tidy_data_geography %>%
  filter(country == "India ") %>%
  group_by(country) %>%
  count(country)

names(india_as_continent) <- c("continent", "n")

continents_and_india <- rbind(noindia_continent_count, india_as_continent)


# Plot3
plusindia.continent.plot <- ggplot(continents_and_india,
                                 aes(x=reorder(continent,-n),
                                     y=n,
                                     #group = continent,
                                     fill = "#4D2C7A")) +
  geom_bar(stat = "identity",
           position = "identity") +
  bbc_style() +
  scale_fill_manual(values = c("#1380A1")) +
  scale_y_continuous(limits = c(0,125)) +
  scale_x_discrete("continent",
                   labels= c("India","MENA","Africa","Asia","South\nAmerica","Europe")) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") + 
  labs(title = "India accounted for half of all shutdowns",
       subtitle = "Total number of shutdowns in 2019 by continent")

plusindia.continent.plot

finalise_plot(plusindia.continent.plot,
              source = "Source: Access Now, 2019",
              save_filepath = "~/Dropbox (BBC)/Visual Journalism/Data/2019/vjdata.26005.internet.shutdown/output/shutdowns_by_continent_plusindia-nc.png",
              width_pixels = 640)

