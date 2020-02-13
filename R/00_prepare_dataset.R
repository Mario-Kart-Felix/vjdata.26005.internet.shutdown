pacman::p_load('tidyverse','dplyr','ggplot2', 'bbplot2', 'lubridate')

# load in data from Access Now
an_data <- read.csv('~/Dropbox (BBC)/Visual Journalism/Data/2019/vjdata.26005.internet.shutdown/source/Access_Now_2019_data_full_list.csv',
                    stringsAsFactors = F)

# Begin tidying your data

# Remove all unnecessary columns 
an_data <- an_data %>%
  select(#-sub.region,
    -area_name, 
    -other_service_details..specify.,
    -SMS_and_phone_call_affected,
    -actual_cause,
    -other_cause_details
  )

# change all necessary columns to their respective necessary formats
an_data$start_date <- as.Date(an_data$start_date, format = "%m/%d/%Y")
an_data$end_date <- as.Date(an_data$end_date, format = "%m/%d/%Y")
an_data$duration <- c(an_data$end_date - an_data$start_date +1)

social_logical <- function(x){
  x$Facebook <- ifelse(an_data$Facebook_affected == "TRUE", 1, 0)
  x$Twitter <- ifelse(an_data$Twitter_affected == "TRUE", 1, 0)
  x$WhatsApp <- ifelse(an_data$WhatsApp_affected == "TRUE", 1, 0)
  x$Instagram <- ifelse(an_data$Instagram_affected == "TRUE", 1, 0)
  x$Telegram <- ifelse(an_data$Telegram_affected == "TRUE", 1, 0)
  x <- x %>% select(-Facebook_affected, -Twitter_affected, -WhatsApp_affected, -Instagram_affected, -Telegram_affected)
  return(x)
} 

months <- function(x){
  x <- x %>%
    mutate(month2 = case_when(
      month == 1 ~ "January",
      month == 2 ~ "February",
      month == 3 ~ "March",
      month == 4 ~ "April",
      month == 5 ~ "May",
      month == 6 ~ "June",
      month == 7 ~ "July",
      month == 8 ~ "August",
      month == 9 ~ "September",
      month == 10 ~ "October",
      month == 11 ~ "November",
      month == 12 ~ "December",
      TRUE ~ "NA")) 
  return(x)
}


tidy_data <- social_logical(an_data)

# add a column for the month and year in which each shutdown began
tidy_data$year <- year(tidy_data$start_date)
tidy_data$month <- month(tidy_data$start_date)


# then filter out date columsn
tidy_data <- tidy_data %>%
  select(-start_date, -end_date)
