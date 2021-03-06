# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Below is the code for unzipping and reading in the dataset, as well as transforming the 'date' row into a Date format.

```r
steps <- read.csv(unz("activity.zip", "activity.csv"))
steps$date <- as.Date(steps$date)
```

## What is mean total number of steps taken per day?

The histogram shows the distribution of the total number of steps taken by the subject each day in the period of two months. The data shows that most days fall between 10000 and 11000 steps (exluding the days when measurments where not taken).

```r
library(dplyr)
steps.by.date <- steps %>% 
                 group_by(date) %>% 
                 summarise(steps.per.day = sum(steps, na.rm = TRUE))
par(mar = c(5,4,6,2)+0.1)
hist(steps.by.date$steps.per.day, 
     breaks = 20,
     main = "10 out of 60 days the average number\nof steps is between 10k and 11k.\nFor 10 days data is missing or not measured.",
     xlab = "Average number of steps per day",
     ylab = "Frequency - Number of Days")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />


```r
mean.steps <- mean(steps.by.date$steps.per.day, na.rm = TRUE)
median.steps <- median(steps.by.date$steps.per.day, na.rm = TRUE)
```

Mean number of steps taken each day is 9354.2295082, while the median number of steps is 10395


## What is the average daily activity pattern?

```r
steps.by.interval <- steps %>% 
                     group_by(interval) %>% 
                     summarise(steps.per.interval = mean(steps, na.rm = TRUE))
with(steps.by.interval, plot(x = interval, 
                             y = steps.per.interval, type = 'l',
                             xlab = "Interval",
                             ylab = "Steps per interval",
                             main = "Highest average step count is for 8:35 AM"))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

```r
maximum.steps <- steps.by.interval[which(steps.by.interval$steps.per.interval == max(steps.by.interval$steps.per.interval)),]
```
The timeseries graph shows the variation of the average number of steps avareged across all days. The time period with the maximum number of steps (206.1698113) is 835 which corresponds to 8:35 (24-hour format).

## Imputing missing values

```r
missing.values <- sum(is.na(steps$steps))
steps.noNA <- steps
for (i in seq(nrow(steps.noNA))){
    if (is.na(steps.noNA[i,1]))
        steps.noNA[i,1] <- as.integer(steps.by.interval[steps.by.interval$interval == steps.noNA[i,3], 2])
}
steps.noNA.by.date <- steps.noNA %>% 
                      group_by(date) %>% 
                      summarise(steps.per.day = sum(steps))
par(mar = c(5,4,6,2)+0.1)
hist(steps.noNA.by.date$steps.per.day, breaks = 20,
     ylim = c(0,20),
     main = "18 out of 60 days the average number of steps\nis between 10k and 11k. Additional 8 days\nare a result of imputing missing values",
     xlab = "Average number of steps per day",
     ylab = "Frequency - Number of Days")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

```r
mean.noNA <- mean(steps.noNA.by.date$steps.per.day)
meadian.noNA <- median(steps.noNA.by.date$steps.per.day)
```
There are 2304 missing values in this data set. All missing values will be substituted with the closest integer value to the average number of steps for the given interval.   
With imputed missing values mean number if steps per day is 10749.77 and the meadian number of steps is 10641. 

## Are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)
steps.noNA$day <- factor(rep("weekday", nrow(steps)), levels = c("weekday", "weekend"))
for (i in seq(nrow(steps.noNA))){
    if (weekdays(steps.noNA[i,2]) %in% c("Saturday", "Sunday"))
        steps.noNA[i,4] <- as.factor("weekend")
}
steps.by.interval.by.day <- steps.noNA %>% 
                            group_by(interval, day) %>% 
                            summarise(steps.per.interval = mean(steps))
g <- ggplot(steps.by.interval.by.day, aes(x = interval, y = steps.per.interval))
g + geom_line() + facet_grid(day~.) + 
    labs(title = "Weekends have more evenly distributed step count",
         x = "Interval",
         y = "Steps per interval") +
    theme(plot.title = element_text(hjust = 0.5))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

The panel graph shows that there is a difference in activity between weekdays and weekends. On weekdays the subject is most active in the morning, probably going to work, and alos in the evening after working hours. On weekends the activity is more evenly distributed during the day, with an additional peek around 8pm. 

