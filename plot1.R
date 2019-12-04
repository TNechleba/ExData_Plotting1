library(tidyverse)
library(lubridate)
library(data.table)


data <- fread("household_power_consumption.txt", na.strings = c("?")) %>%
    as_tibble() %>%
    filter(Date %in% c("1/2/2007", "2/2/2007"))


data_mod <- data %>%
    mutate(DateTime = mdy_hms(paste(Date, Time)),
        Date = parse_date_time(Date, "mdy"),
        Time = parse_date_time(Time, "HMS")) %>%
    print
        

d <- ggplot(data_mod, aes(Global_active_power)) +
geom_histogram(binwidth = 0.5, 
        boundary = 0,
        color = "black",
        fill = "red") +
labs(x = "Global Active Power (kilowatts)",
        y = "Frequency") +
theme(plot.title = element_text(hjust = 0.5)) +
ggtitle("Global Active Power")




## Define resolution and save to png
res <- 72 ## Screen res as per ggsave() documentation

ggsave("plot1.png", width = 480/res, height = 480/res, dpi = res)
