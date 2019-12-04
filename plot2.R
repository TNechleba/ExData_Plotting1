library(tidyverse)
library(lubridate)
library(data.table)


data <- fread("household_power_consumption.txt", na.strings = c("?")) %>%
    as_tibble() %>%
    filter(Date %in% c("1/2/2007", "2/2/2007"))

## Parse dates and times for convenient use in tidyverse

data_mod <- data %>%
    mutate(DateTime = dmy_hms(paste(Date, Time)),
        Date = parse_date_time(Date, "dmy"),
        Time = parse_date_time(Time, "HMS")) %>%
    print


## Define breaks for future chart

min <- data_mod %>%
    filter(DateTime == min(DateTime)) %>%
    pull(DateTime)

med <- data_mod %>%
    filter(day(DateTime) == 2) %>%
    filter(DateTime == min(DateTime)) %>%
    pull(DateTime)

max <- data_mod %>%
    filter(DateTime == max(DateTime)) %>%
    pull(DateTime) + minutes(1)


# Construct chart

e <- ggplot(data_mod, aes(x = DateTime, y = Global_active_power)) +
geom_step() + 
scale_x_datetime(breaks = c(min, med, max),
        labels = c("Thu","Fri","Sat")) +
ylab("Global Active Power (kilowatts") + 
theme(axis.title.x = element_blank(),
        panel.border = element_rect(linetype = "solid", colour = "black", fill = NA),
        panel.background = element_rect(fill = NA))

## Define resolution and save to png

res <- 72 ## Screen res as per ggsave() documentation

ggsave("plot2.png", width = 480/res, height = 480/res, dpi = res)
