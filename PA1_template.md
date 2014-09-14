---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---


# Peer Assignment 1

## Loading and preprocessing the data


```r
unzip("activity.zip")
activity <- read.csv("activity.csv")

# change classes:
activity$date <- as.Date(activity$date)
activity$interval <- as.factor(activity$interval)

# list variable classes:
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

## What is mean total number of steps taken per day?


```r
# create new dataframe for sums
dailysums <- aggregate(steps ~ date, data=activity, sum)
dailysums$date <- as.factor(dailysums$date)
```

### Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
qplot(date, steps, data=dailysums, geom="bar")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

### Calculate and report the mean and median total number of steps taken per day

```r
mean(dailysums$steps)
```

```
## [1] 10766
```

```r
median(dailysums$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
# create new dataframe for means
intervalmeans <- aggregate(steps ~ interval, data=activity, mean)
colNames <- names(intervalmeans)
colNames[2] <- "mean"
names(intervalmeans) <- colNames
```

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
qplot(interval, mean, data=intervalmeans, geom="bar", type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
as.character( intervalmeans[which.max(intervalmeans$mean), "interval"] )
```

```
## [1] "835"
```

## Imputing missing values

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(activity[is.na(activity$steps),])
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

> Let's impute with interval mean. 

### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
merged <- merge(activity, intervalmeans)
merged$steps <- ifelse( is.na(merged$steps), merged$mean, merged$steps )
merged$mean <- NULL
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# create new dataframe for sums
dailysums2 <- aggregate(steps ~ date, data=merged, sum)
dailysums2$date <- as.factor(dailysums2$date)

# plot
library(ggplot2)
qplot(date, steps, data=dailysums2, geom="bar")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
# Calculate and report the mean and median total number of steps taken per day
mean(dailysums2$steps)
```

```
## [1] 10766
```

```r
median(dailysums2$steps)
```

```
## [1] 10766
```

```r
# differences:
mean(dailysums2$steps) - mean(dailysums$steps)
```

```
## [1] 0
```

```r
median(dailysums2$steps) - median(dailysums$steps)
```

```
## [1] 1.189
```

## Are there differences in activity patterns between weekdays and weekends?


```r
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
merged$weekday <- weekdays( merged$date )
merged$day_type <- ifelse( (merged$weekday=="Saturday" | merged$weekday=="Sunday"), "weekend", "weekday")
merged$day_type <- as.factor(merged$day_type)
str(merged)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ steps   : num  1.72 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-01" "2012-11-23" ...
##  $ weekday : chr  "Monday" "Friday" "Sunday" "Tuesday" ...
##  $ day_type: Factor w/ 2 levels "weekday","weekend": 1 1 2 1 2 1 2 1 1 2 ...
```

```r
# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

weekend_data <- subset(merged, merged$day_type=="weekend")
weekday_data <- subset(merged, merged$day_type=="weekday")


weekend_intervalmeans <- aggregate(steps ~ interval, data=weekend_data, mean)
colNames <- names(weekend_intervalmeans)
colNames[2] <- "mean"
names(weekend_intervalmeans) <- colNames

weekday_intervalmeans <- aggregate(steps ~ interval, data=weekday_data, mean)
colNames <- names(weekday_intervalmeans)
colNames[2] <- "mean"
names(weekday_intervalmeans) <- colNames

par(mfrow=c(2,1))
plot(weekday_intervalmeans$interval, weekday_intervalmeans$mean, type="l", main="weekday")
plot(weekend_intervalmeans$interval, weekend_intervalmeans$mean, type="l", main="weekend")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
