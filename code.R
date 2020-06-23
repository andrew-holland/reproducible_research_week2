library(dplyr)
library(ggplot2)
library(stringr)
library(knitr)
data <- read.csv("activity.csv")

hist_data <- data %>%
    group_by(date) %>%
    summarise("DailySteps" = sum(steps, na.rm = T))

ggplot(hist_data, aes(x=DailySteps)) + 
    geom_histogram(binwidth=1000)

mean_daily_steps <- mean(hist_data$DailySteps, na.rm = T)

median_daily_steps <- median(hist_data$DailySteps, na.rm = T)


time_series_data <- data %>%
    group_by(interval) %>%
    summarise("AverageSteps" = mean(steps, na.rm=T)) %>%
    mutate(time1 = str_pad(as.character(interval), width = 4, side="left", pad = "0")) %>%
    mutate(time2 = as.POSIXct(time1, format="%H%M"), format="%H:%M:%S") %>%
    ungroup()

ggplot(time_series_data, aes(x=time2, y=AverageSteps)) +
    geom_line() +
    scale_x_datetime(date_labels = "%H:%M") +
    xlab("Time")  +
    ylab("Average Steps") +
    ggtitle("(4) Average Step Count Throughout the Day") +
    theme_bw()

max_steps_interval <- time_series_data %>%
    filter(AverageSteps == max(AverageSteps, na.rm = T)) %>%
    select(interval)
max_steps_interval <- max_steps_interval[[1]]


no_of_nas <- sum(is.na(data$steps))

data_no_na <- data

for(i in 1:nrow(data_no_na)){
    if(is.na(data_no_na[i, 1])){
        print(paste("NA found for", data_no_na[i, 2], data_no_na[i, 3]))
        newvalue <- time_series_data %>%
            filter(interval == data_no_na[i, 3]) %>%
            select(AverageSteps)
        data_no_na[i, 1] = newvalue[[1]]
        print(paste("replacing with new value", newvalue))
    }
}

data_no_na2 <- data_no_na %>%
    group_by(date) %>%
    summarise("DailySteps" = sum(steps, na.rm = T))

ggplot(data_no_na2, aes(x=DailySteps)) + 
    geom_histogram(binwidth=1000)


mean_daily_steps_no_na <- mean(data_no_na2$DailySteps, na.rm = T)

median_daily_steps_no_na <- median(data_no_na2$DailySteps, na.rm = T)

mean_na_impact <- mean_daily_steps - mean_daily_steps_no_na
median_na_impact <- median_daily_steps - median_daily_steps_no_na


data_no_na3 <- data_no_na %>%
    mutate(date2 = as.POSIXct(date, format="%Y-%m-%d")) %>%
    mutate("weekday" = weekdays(date2)) %>%
    mutate(Weekday = ifelse(weekday %in% c("Monday", "Tuesday", "Wednesday","Thursday", "Friday"), "Weekday", "Weekend")) %>%
    group_by(Weekday, interval) %>%
    summarise("AverageSteps" = mean(steps, na.rm=T)) %>%
    mutate(time1 = str_pad(as.character(interval), width = 4, side="left", pad = "0")) %>%
    mutate(time2 = as.POSIXct(time1, format="%H%M"), format="%H:%M:%S")
    

ggplot(data_no_na3, aes(x=time2, y=AverageSteps)) +
    geom_line(aes(group=Weekday, color=Weekday)) +
    scale_x_datetime(date_labels = "%H:%M") +
    xlab("Time")  +
    ylab("Average Steps") +
    ggtitle("(8) Average Step Count - Weekdays vs Weekends") +
    theme_bw()
