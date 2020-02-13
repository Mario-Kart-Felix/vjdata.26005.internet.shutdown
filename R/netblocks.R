pacman::p_load('dplyr','tidyverse','bbplot2', 'lubridate', 'chron', 'ggplot2')


IRQ_data_nov <- read.csv('~/BBC/Projects/vjdata.26005.internet.shutdown/data/NetBlocks/netblocks-iraq-diffscan-datasets/netblocks-diffscan-iraq-november-2019.csv',stringsAsFactors = F)
IRQ_data_oct <- read.csv('~/BBC/Projects/vjdata.26005.internet.shutdown/data/NetBlocks/netblocks-iraq-diffscan-datasets/netblocks-diffscan-iraq-october-2019.csv',stringsAsFactors = F)

IRQ_data <- rbind(IRQ_data_oct, IRQ_data_nov) %>%
  filter(connectivity <= 1000) %>%
  mutate(ID = seq_along(t)) 
IRQ_data <- IRQ_data %>%  mutate(t2 = as_datetime(IRQ_data$t))

# parse date-time data from time column into separate columns for analysis and isolation
IRQ_data <- IRQ_data %>%
  mutate(year = year(time), 
         month = month(time), 
         day = day(time),
         hour = hour(time),
         minute = minute(time)) %>%
  select(-time)

# get columns in order
IRQ_data <- IRQ_data[, c(3,5,6,7,8,9, 1,4,2)]

# calculate percentage in its own column
IRQ_data <- IRQ_data %>%
  mutate(pct_connectivity = connectivity/10)

##### This helps select a specific period to plot #######
IRQ_data_oct <- IRQ_data %>%
  filter(month == 10) %>%
  group_by(month, day) %>%
  mutate(xlabel = paste(day," Oct"))

IRQ_data_nov <- IRQ_data %>%
  filter(month == 11) %>%
  group_by(month, day) %>%
  mutate(xlabel = paste(day," Nov"))


nov_lim <- as.POSIXct(c("2019-11-01 00:00:00","2019-12-01 00:00:00"),  origin = "1970-01-01 00:00:00")
oct_lim <- as.POSIXct(c("2019-10-01 00:00:00","2019-11-01 00:00:00"),  origin = "1970-01-01 00:00:00")

##### Plot #####

IRQ.loess.nov <- ggplot(IRQ_data_nov,
                   aes(x=t2, #always use t2 column to plot, which will auto-break the x-axis labels in a readable format
                       y=pct_connectivity
                       )) +
  #geom_point()+
  geom_smooth(span = 0.075, # Ask Robert what's an appropriate span figure
              method = "loess",
              formula = y~x,
              size = 1,
              colour = 'black') +
  geom_vline(xintercept = as.POSIXct("2019-11-04 00:01:00"),
             colour = "#222222",
             linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2019-11-12 20:15:00"),
             colour = "#222222",
             linetype = "dashed") +
  bbc_style() +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(limits = c(0,100),
                     labels = function(x) paste0(x, "%")) +
  scale_x_datetime(limits = nov_lim,
                   breaks = waiver()) +
  labs(title="Iraq lost internet for more than a week",
       subtitle = "Connectivity data covering the month of November 2019") +
  theme(axis.title.x = element_blank())

IRQ.loess.nov


finalise_plot(IRQ.loess.nov,
              source = "Source: NetBlocks",
              save_filepath = "~/BBC/Projects/vjdata.26005.internet.shutdown/output/netblocks_irq_nov19-nc.png",
              width_pixels = 640)


### October ###


IRQ.loess.oct <- ggplot(IRQ_data_oct,
                    aes(x=t2, #always use t2 column to plot, which will auto-break the x-axis labels in a readable format
                        y=pct_connectivity
                    )) +
  #geom_point()+
  geom_smooth(span = 0.075, # Ask Robert what's an appropriate span figure
              method = "loess",
              formula = y~x,
              size = 1,
              colour = 'black') +
  geom_vline(xintercept = as.POSIXct("2019-10-01 20:00:00"),
             colour = "#222222",
             linetype = "dashed") +
  geom_vline(xintercept = as.POSIXct("2019-10-11 19:15:00"),
             colour = "#222222",
             linetype = "dashed") +
  bbc_style() +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_y_continuous(limits = c(0,100),
                     labels = function(x) paste0(x, "%")) +
  scale_x_datetime(limits = oct_lim,
                   breaks = waiver()) +
  labs(title="Iraq lost internet for more than a week",
       subtitle = "Connectivity data covering the month of October 2019") +
  theme(axis.title.x = element_blank())

IRQ.loess.oct



finalise_plot(IRQ.loess.oct,
              source = "Source: NetBlocks",
              save_filepath = "~/BBC/Projects/vjdata.26005.internet.shutdown/output/netblocks_irq_oct19-nc.png",
              width_pixels = 640)




cal_dates <- IRQ_data_oct %>%
  group_by(year, month, day) %>%
  summarise(connectivity = mean(pct_connectivity))
  
cal.oct <- ggcal(cal_dates$day, cal_dates$connectivity)













################### Alternate plots ####################


IRQ.line <- ggplot(IRQ_data,
                   aes(x=t2, #always use t2 column to plot, which will auto-break the x-axis labels in a readable format
                   y=pct_connectivity)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_colour_manual(values = c("#AD08F9", "#1380A1")) +
  scale_y_continuous(limits = c(0,100),
                     labels = function(x) paste0(x, "%")) +
  # scale_x_discrete(limits = c("2019-10-01 00:00:00","2019-11-01 00:00:00"),
  #                  breaks = c(by=5)) +
  bbc_style() +
  labs(title="Iraq internet connectivity - TEST",
       subtitle = "Data covering the months of October and November") +
  theme(axis.title.x = element_blank())
  

IRQ.line

IRQ.area <- ggplot(IRQ_data_oct,
                    aes(x=t2,
                        y=pct_connectivity,
                        fill = "#D46666")) +
  geom_area() +
  geom_line(size = 1) +
  bbc_style() +
  labs(title="Iraq internet connectivity - TEST",
       subtitle = "Data covering October 2-4") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  #scale_x_discrete(t, labels = NULL) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  theme(legend.position = "none")
  # scale_fill_manual(values = c("#FAAB18",
  #                             "#D46666",
  #                             "#D46666",
  #                             "#D46666",
  #                             "#D46666",
  #                             "#D46666",
  #                             "#1380A1",
  #                             "#4BA7BC",
  #                             "#A1CCD9",
  #                             "#4D2C7A",
  #                             "#F0B3FF",
  #                             "#5B0101"))
  

IRQ.area





#' Given dates and a fill variable, generate a calendar plot
#'
#' Given a collection of dates and values for each date, this
#' will generate a calendar for each month in the range of dates.
#' This will generate complete calendar months, so there is no need
#' to fill in any missing dates or anything.
#'
#' Note that the ggplot2 object returned will not have any scale
#' defined for the fill, so that can be added to the returned
#' ggplot2 object.
#'
#'
#' @param dates vector of \code{Date} values
#' @param fills vector of values to map onto the fill aesthetic
#'
#' @note If the dates span multiple years, the year will be appended
#' to the month name automatically, otherwise, it will not appear.
#'
#' @import ggplot2
#' @import dplyr
#' @import forcats
#' @import tibble
#'
#' @export
#'
#' @examples {
#' mydate <- seq(as.Date("2017-02-01"), as.Date("2017-07-22"), by="1 day")
#' myfills <- rnorm(length(mydate))
#'
#' print(ggcal(mydate, myfills))
#' }
ggcal <- function(dates, fills) {
  # get ordered vector of month names
  months <- format(seq(as.Date("2016-01-01"), as.Date("2016-12-01"), by="1 month"), "%B")
  
  # get lower and upper bound to fill in missing values
  mindate <- as.Date(format(min(dates), "%Y-%m-01"))
  maxdate <- (seq(as.Date(format(max(dates), "%Y-%m-01")), length.out = 2, by="1 month")-1)[2]
  # set up tibble with all the dates.
  filler <- tibble(date = seq(mindate, maxdate, by="1 day"))
  
  t1 <- tibble(date = dates, fill=fills) %>%
    right_join(filler, by="date") %>% # fill in missing dates with NA
    mutate(dow = as.numeric(format(date, "%w"))) %>%
    mutate(month = format(date, "%B")) %>%
    mutate(woy = as.numeric(format(date, "%U"))) %>%
    mutate(year = as.numeric(format(date, "%Y"))) %>%
    mutate(month = factor(month, levels=months, ordered=TRUE)) %>%
    arrange(year, month) %>%
    mutate(monlabel=month)
  
  if (length(unique(t1$year))>1) { # multi-year data set
    t1$monlabel <- paste(t1$month, t1$year)
  }
  
  t2 <- t1 %>%
    mutate(monlabel = factor(monlabel, ordered=TRUE)) %>%
    mutate(monlabel = fct_inorder(monlabel)) %>%
    mutate(monthweek = woy-min(woy),
           y=max(monthweek)-monthweek+1)
  
  weekdays <- c("S", "M", "T", "W", "T", "F", "S")
  ggplot(t2, aes(dow, y, fill=fill)) +
    geom_tile(color="gray80") +
    facet_wrap(~monlabel, ncol=3, scales="free") +
    scale_x_continuous(expand=c(0,0), position="top",
                       breaks=seq(0,6), labels=weekdays) +
    scale_y_continuous(expand=c(0,0)) +
    theme(panel.background=element_rect(fill=NA, color=NA),
          strip.background = element_rect(fill=NA, color=NA),
          strip.text.x = element_text(hjust=0, face="bold"),
          legend.title = element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          axis.text.y = element_blank(),
          strip.placement = "outsite")
}


