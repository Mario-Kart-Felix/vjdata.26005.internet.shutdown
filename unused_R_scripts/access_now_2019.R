pacman::p_load('tidyverse','dplyr','ggplot2', 'bbplot2', 'lubridate')

# load in data from Access Now
an_data <- read.csv('~/Dropbox (BBC)/Visual Journalism/Data/2019/vjdata.26005.internet.shutdown/source/Access_Now_2019_data_full_list.csv',
                    stringsAsFactors = F)

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

tidy_data <- social_logical(an_data)

#write_csv(an_data, "~/Dropbox (BBC)/Visual Journalism/Data/2019/vjdata.26005.internet.shutdown/derived/an_data_limited.csv")

tidy_data_long <- tidy_data %>%
  gather(impacted, yes_no, broadband, mobile, specific_services, telephony) %>%
  filter(country != "India ")


tidy_byimpact <- tidy_data_long %>%
  group_by(country, impacted) %>%
  summarise(yes_no = sum(as.numeric(yes_no))) %>%
  filter(country != "India ") %>%
  filter(yes_no <= 50)

tidy_byimpact$impacted <- factor(tidy_byimpact$impacted,
                                 levels = c('telephony', 'specific_services', 'mobile','broadband'),
                                 labels = c("Telephony", "App services", "Mobile", "Broadband"))


tidy_byimpact$country <- factor(tidy_byimpact$country,
                                levels = c("Indonesia", "Eritrea", "Jordan", "Liberia", "Tajikistan", "SriLanka", "Pakistan",
                                           "Kazakhstan", "United Kingdom", 
                                           "Algeria", "Cameroon", "Chad", "China", "Gabon", "Malawi ", "Myanmar", 
                                           "Mauritania","DemocraticRepublicCongo", "Zimbabwe ", "Sudan",  "Russia",   "Benin ",  "Ethiopia "
                                ),
                                labels = c("Indonesia", "Eritrea", "Jordan", "Liberia", "Tajikistan", "Sri Lanka", "Pakistan", 
                                           "Kazakhstan", "United Kingdom", 
                                           "Algeria", "Cameroon", "Chad", "China", "Gabon", "Malawi", "Myanmar", 
                                           "Mauritania", "DRC", "Zimbabwe", "Sudan",  "Russia",  "Benin",  "Ethiopia"
                                ))


# number of times mobile is shut off vs broadband
test.chart <- ggplot(tidy_byimpact,
                     aes(country,
                         yes_no,
                         fill = impacted)) +
  geom_bar(stat = "identity",
           position = "stack") +
  bbc_style() +
  coord_flip() +
  scale_fill_manual(values = c("#4D2C7A","#FAAB18", "#1380A1","#990000", "#588300", "#4D2C7A")) + 
  theme(panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y=element_blank()) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  labs(title = "Total times a service was affected in 2019",
       subtitle = "Sometimes multiple services are affected in a single shutdown") 

test.chart

finalise_plot(test.chart,
              footnote = "Note: Shutdowns in India not shown",
              source_name = "Source: Access Now",
              save_filepath = "~/BBC/Projects/blackouts/output/type_by_country-nc.png",
              width_pixels = 640,
              height_pixels = 700)


####################################################################
# By month #
####################################################################

an_data_bymonth <- an_data %>%
  mutate(start_month = month(start_date)) %>%
  mutate(tally = 1)

an_data_bymonth <- an_data_bymonth %>%
  group_by(start_month, country) %>%
  tally(tally)

write.csv(an_data_bymonth, "~/BBC/Projects/vjdata.26005.internet.shutdown/output/tally_by_month.csv")











####################################################################
# India only #
####################################################################

tidy_data_india <- tidy_data %>%
  gather(impacted, yes_no, broadband, mobile, specific_services, telephony) %>%
  filter(country == "India ")

tidy_byimpact_india <- tidy_data_india %>%
  group_by(country, impacted) %>%
  summarise(yes_no = sum(as.numeric(yes_no)))

tidy_byimpact_india$impacted <- factor(tidy_byimpact_india$impacted,
                                 levels = c('broadband',  'mobile', 'specific_services', 'telephony'),
                                 labels = c("Broadband", "Mobile", "App services","Telephony"))



tidy_bytype <- tidy_data %>%
  group_by(shutdown_type) %>%
  summarise(n())

tidy_bycountry <- tidy_data %>%
  filter(country != "India ") %>%
  mutate(world = "world") %>%
  group_by(world, country) %>%
  summarise(sum = n()) %>%
  group_by(world) %>%
  summarise(total = sum(as.numeric(sum)))

names(tidy_bycountry) <- c("country", "total")

tidy_india <- tidy_data %>%
  filter(country == "India ") %>%
  group_by(country) %>%
  summarise(total = n())

tidy_all <- rbind(tidy_bycountry, tidy_india)

india.chart <- ggplot(tidy_all,
                      aes(country,
                          total,
                          fill = "#4D2C7A")) +
  geom_bar(stat = "identity", position = "identity") +
  bbc_style() +
  scale_fill_manual(values = c("#4D2C7A","#FAAB18", "#1380A1","#990000", "#588300", "#4D2C7A")) + 
  theme(panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  labs(title = "India disrupts the internet more often than\nanywhere else in the world")

india.chart  

  
finalise_plot(india.chart,
              #footnote = "Note: Shutdowns in India not shown",
              source_name = "Source: Access Now",
              save_filepath = "~/BBC/Projects/blackouts/output/india_shutdowns-nc.png",
              width_pixels = 640)
  

####################################################################
# Time periods #
#################################################################### 

tidy_data$start_date <- as.Date(tidy_data$start_date, format = "%m/%d/%Y")
tidy_data$end_date <- as.Date(tidy_data$end_date, format = "%m/%d/%Y")

tidy_data_time <- tidy_data %>%
  mutate(duration = end_date - start_date)




######## Extra

# tidy_data <- an_data %>%
#   select(-Info_source, -news_link, -geo_scope, -sub.region, -decision_maker, -telcos_involved, -gov_ack, -off_statement, -violence,
#          -hr_abuse_reported, -users_affected.targetted, -legal_justif, -legal_method, -telco_resp, -ordered_by,
#          -telco_.ack, -econ_impact, -an_link, -other_just_details,  -other_service_details..specify.)

