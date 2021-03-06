---
title: "Reproducible Research Assignment"
author: "Andrew Holland"
date: "23/06/2020"
output: html_document
---

## (1) Read in data

```{r echo=TRUE}
library(dplyr)
library(ggplot2)
library(stringr)

data <- read.csv("activity.csv")

head(data)
```

## (2) Histogram of total steps taken per day

```{r echo=TRUE}
hist_data <- data %>%
    group_by(date) %>%
    summarise("DailySteps" = sum(steps, na.rm = T))

ggplot(hist_data, aes(x=DailySteps)) + 
    geom_histogram(binwidth=1000) +
    xlab("Daily Steps")  +
    ylab("Count") +
    theme_bw()
    
```

## (3) Mean and Median steps taken each day

```{r echo=TRUE}
mean_daily_steps <- mean(hist_data$DailySteps, na.rm = T)

median_daily_steps <- median(hist_data$DailySteps, na.rm = T)

```

The mean number of steps taken each day was `r mean_daily_steps`

The median number of steps taken each day was `r median_daily_steps`

## (4) Time series plot of average steps taken per day

```{r echo=TRUE}
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
    theme_bw()

```


## (5) 5 minute interval with the most steps

```{r echo=TRUE}
max_steps_interval <- time_series_data %>%
    filter(AverageSteps == max(AverageSteps, na.rm = T)) %>%
    select(interval)
max_steps_interval <- max_steps_interval[[1]]

```

The interval with the highest step count was `r max_steps_interval`

## (6) Strategy for inputting missing data

The strategy used here was to use the average step count for each interval as the replacement value.

```{r echo=TRUE}
no_of_nas <- sum(is.na(data$steps))

data_no_na <- data

for(i in 1:nrow(data_no_na)){
    if(is.na(data_no_na[i, 1])){
        newvalue <- time_series_data %>%
            filter(interval == data_no_na[i, 3]) %>%
            select(AverageSteps)
        data_no_na[i, 1] = newvalue[[1]]
    }
}
no_of_nas2 <- sum(is.na(data_no_na$steps))

```

Number of NAs found: `r no_of_nas`

Post replacement NAs found: `r no_of_nas2`

## (7a) Histogram of the total number of steps taken each day after missing values are inputted

```{r echo=TRUE}
data_no_na2 <- data_no_na %>%
    group_by(date) %>%
    summarise("DailySteps" = sum(steps, na.rm = T))

ggplot(data_no_na2, aes(x=DailySteps)) + 
    geom_histogram(binwidth=1000) +
    xlab("Daily Steps")  +
    ylab("Count") +
    theme_bw()
```

## (7b) Mean and Median steps taken each day, with missing data replaced

```{r echo=TRUE}
mean_daily_steps_no_na <- mean(data_no_na2$DailySteps, na.rm = T)

median_daily_steps_no_na <- median(data_no_na2$DailySteps, na.rm = T)

mean_na_impact <- mean_daily_steps - mean_daily_steps_no_na
median_na_impact <- median_daily_steps - median_daily_steps_no_na

```

The mean number of steps taken each day was `r mean_daily_steps`

The median number of steps taken each day was `r median_daily_steps`

###Impact of replacing missing data

The mean changed by `r mean_na_impact`

The median changed by `r median_na_impact`

## (8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r echo=TRUE}
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
```






