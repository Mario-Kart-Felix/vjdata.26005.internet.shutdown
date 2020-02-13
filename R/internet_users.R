pacman::p_load('tidyverse','dplyr', 'bbplot2', 'bbmap', 'zoo')

# load in datasets
data <- read.csv('~/BBC/Projects/blackouts/source/Individuals_using_internet_2000-2018.csv')
WB_population <- read.csv('~/BBC/Projects/blackouts/source/WB_world_population.csv')
UN_population <- read.csv('~/BBC/Projects/blackouts/source/UN_world_population.csv')



# filter internet users dataset to each country's percentage by year
pct_users <- data %>%
  gather(year, value, 2:20) %>%
  arrange(Country, year, value) %>%
  filter(Country != "American Samoa")

pct_users[1, 3] = 0 # replaces Afghanistan 2000 with 0.0 so locf will run

# replace all NAs with the nearest value for the same country [could be year previous or 10, depending]
pct_users <- pct_users %>%
  mutate(latest = na.locf(value)) %>%
  select(-value) %>%
  spread(year, latest)


# load in world population datasets and join to include ISO codes
# filter out countries with no data, select 2018 UN data only
population <- WB_population %>%
  select(Country.Name, Country.Code)

population <- left_join(UN_population, population, by = c('Country' = 'Country.Name')) %>%
  select(Country, Country.Code, X2018) %>%
  filter(Country.Code != is.na(Country.Code))


# join population data to internet users dataset
internet_users <- left_join(pct_users, population, by = c('Country' = 'Country')) %>%
  filter(Country.Code != is.na(Country.Code)) %>%
  select(Country, Country.Code, X2018.x, X2018.y) %>%
  mutate(Population = X2018.y*1000) %>%
  select(Country, Country.Code, X2018.x, Population)

internet_users$Country.Code <- as.character(internet_users$Country.Code)
names(internet_users) = c('Country', 'Code', 'Internet', 'Population')

# calculate number of internet users per country
internet_users <- internet_users %>%
  mutate(Pop_Internet = c((Internet/100)*(Population))) 

# save out data
write.csv(internet_users, '~/BBC/Projects/blackouts/source/internet_users2.csv')  

