pacman::p_load('dplyr','tidyverse','bbplot2')

data <- read.csv("~/BBC/Projects/blackouts/source/FOTN_2019_clean.csv")

#social <- data %>%
#  filter(Disruption == "Social or communications block") %>%
#  spread(Year, Occurred) 
#names(social) <- c("Continent", "Region", "Country", "Freedom_Score", "Disruption", "x2013", "x2014", "x2015", "x2016", "x2017", "x2018","x2019")
#social <- social %>%
#  mutate("Frequency" == rowSums("x2013":"x2019", na.rm=F))
#write.csv(data_long, "~/BBC/Projects/blackouts/source/FOTN_2019_clean.csv")

########################################################################################
# Network changes by type
########################################################################################

grouped_data <- data %>%
  group_by(Year, Disruption, Occurred) %>%
  summarise(n()) %>%
  filter(Disruption != "Disruption, any") %>%
  filter(Occurred != is.na(Occurred)) %>%
  filter(Disruption != "Web.2.0.blocked") %>%
  filter(Year != 2012) %>% 
  select(Year, Disruption, "n()")

names(grouped_data) <- c("Year", "Disruption", "Occurred")
            

multi.line.chart <- ggplot(grouped_data,
                           aes(
                           x = Year,
                           y = Occurred,
                           colour = Disruption)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#990000", "#1380A1")) +
  bbc_style() +
  labs(title="Test change over time",
       subtitle = "Are disruptions increasing?")

multi.line.chart  

area.chart <- ggplot(data = grouped_data,
                     aes(x=Year,
                         y=Occurred,
                         fill = Disruption)) +
  geom_area() +
  bbc_style() +
  scale_fill_manual(values = c("#FAAB18", "#1380A1","#990000", "#588300")) + 
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_y_continuous(limits = c(0,50),
                     breaks = seq(0, 50, by = 10)) +
  scale_x_continuous(limits = c(2013,2019),
                     breaks = seq(2013, 2019, by = 1)) +
  #theme(legend.position = "none") +
  labs(title="Are network disruptions increasing?",
       subtitle = "Number of countries with disruptions by year")

area.chart              

finalise_plot(area.chart,
              footnote = "Note: 65 countries observed", 
              save_filepath = "~/BBC/Projects/blackouts/output/network_change_area-nc.png",
              source = "Source: Freedom House, 2019",
              width_pixels = 640,
              height_pixels = 500
             )

###############################################################################
# Continent charts #
###############################################################################

regional_data <- data %>%
  filter(Disruption == "Disruption, any") %>%
  group_by(Year, Region, Occurred) %>%
  summarise(n()) %>%
  filter(Occurred != is.na(Occurred)) %>%
  filter(Year != 2012) %>% 
  select(Year, Region, "n()")

names(regional_data) <- c("Year", "Region", "Occurred")

regional_data$Region <- factor(regional_data$Region,
                                   levels = c("Americas",
                                              "Europe",
                                              "Eurasia",
                                              "Sub-Saharan Africa",
                                              "Asia-Pacific",
                                              "MENA"
                                              ))

area.chart.2 <- ggplot(data = regional_data,
                     aes(x=Year,
                         y=Occurred,
                         fill = Region)) +
  geom_area() +
  bbc_style() +
  scale_fill_manual(values = c("#FAAB18", "#1380A1","#990000", "#588300", "#4D2C7A", "#AD08F9")) + 
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_y_continuous(limits = c(0,35),
                     breaks = seq(0, 35, by = 5)) +
  scale_x_continuous(limits = c(2013,2019),
                     breaks = seq(2013, 2019, by = 1)) +
  #theme(legend.position = "none") +
  labs(title="Where are network disruptions increasing?",
       subtitle = "Number of countries with disruptions by year")

area.chart.2              

finalise_plot(area.chart.2,
              footnote = "Note: A total of 65 countries are observed", 
              save_filepath = "~/BBC/Projects/blackouts/output/regional_change_area-nc.png",
              source = "Source: Freedom House, 2019",
              width_pixels = 640,
              height_pixels = 500
)

multi.line.chart.2 <- ggplot(data = regional_data,
                       aes(x=Year,
                           y=Occurred,
                           colour = Region)) +
  geom_line(size = 1) +
  bbc_style() +
  scale_colour_manual(values = c("#FAAB18", "#1380A1","#990000", "#588300", "#4D2C7A", "#AD08F9")) + 
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_y_continuous(limits = c(0,11),
                     breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(limits = c(2013,2019),
                     breaks = seq(2013, 2019, by = 1)) +
  #theme(legend.position = "none") +
  labs(title="Where are network disruptions increasing?",
       subtitle = "Number of countries with disruptions by year")

multi.line.chart.2  

finalise_plot(multi.line.chart.2,
              footnote = "Note: A total of 65 countries are observed", 
              save_filepath = "~/BBC/Projects/blackouts/output/regional_change_line-nc.png",
              source = "Source: Freedom House, 2019",
              width_pixels = 640,
              height_pixels = 500
)


###############################################################################
# Freedom status data #
###############################################################################

freedom_data <- data %>%
  filter(Disruption == "Disruption, any") %>%
  select("Country", "X2019.Total.Score", "Year") %>%
  spread(Year, X2019.Total.Score)

write.csv(freedom_data, "~/BBC/Projects/blackouts/source/freedom_score_by_year.csv")

freedom_data2 <- data %>%
  filter(Disruption != "Disruption, any") %>%
  filter(Disruption != "Web.2.0.blocked") %>%
  select("Country", "X2019.Total.Score", "Disruption", "Occurred")  %>%
  group_by(Country, X2019.Total.Score, Disruption, Occurred) %>%
  summarise() %>%
  filter(Occurred != is.na(Occurred)) %>%
  group_by(X2019.Total.Score, Disruption, Occurred) %>%
  summarise(n()) %>%
  select("X2019.Total.Score","Disruption", "n()")

names(freedom_data2) <- c("Score", "Disruption","Count")

freedom_data2$Score <- factor(freedom_data2$Score,
                                   levels = c("Free",
                                              "Partly Free",
                                              "Not Free"))


freedom.bars <- ggplot(freedom_data2,
                       aes(x=Score,
                           y=Count,
                           fill = Disruption)) +
  geom_bar(stat = "identity",
           position = 'dodge') +
  bbc_style() +
  scale_fill_manual(values = c("#4D2C7A","#FAAB18", "#1380A1","#990000", "#588300", "#4D2C7A")) + 
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_y_continuous(limits = c(0,22),
                     breaks = seq(0, 20, by = 5)) +
  #theme(legend.position = 'none') +
  labs(title="Even 'Free' countries have network disruptions",
       subtitle = "Number of countries with disruptions between 2012 and 2019") 

freedom.bars

finalise_plot(freedom.bars,
              footnote = "Note: None of the 65 countries changed status during this period", 
              save_filepath = "~/BBC/Projects/blackouts/output/status_bars-nc.png",
              source = "Source: Freedom House, 2019",
              width_pixels = 640,
              height_pixels = 450
)

