---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
  #load libraries
  library(dplyr)
  library(ggplot2)
  library(lattice)
  #load data
  unzip("activity.zip")
  activity<-read.csv("activity.csv")
  #process data
  activity$date<-as.Date(activity$date)
```

## What is mean total number of steps taken per day?

```r
  #Calculate the total number of steps taken per day
  totals<-activity %>% group_by(date) %>% summarise(steps_total = sum(steps))
  #Make a histogram of the total number of steps taken each day
  totals_hist<-ggplot(totals, aes(steps_total)) + 
    geom_histogram(binwidth = 5000) + theme_classic() +
    ggtitle("Total Number of Steps Taken Each Day") + xlab("Steps") + ylab("Frequency")
  totals_hist
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
  #Calculate and report the mean and median of the total number of steps taken per day
  mean_daily_steps<-mean(totals$steps_total, na.rm = TRUE)
  median_daily_steps<-median(totals$steps_total, na.rm = TRUE)
```
The mean of the total number of steps taken per day is 1.0766189\times 10^{4}.

The median of the total number of steps taken per day is 10765.


## What is the average daily activity pattern?

```r
  #Calculate interval means
  interval_means<-activity %>% group_by(interval) %>% summarise(steps_mean = mean(steps, 
                                                                                na.rm = TRUE))
  #Generate time series plot
  plot(interval_means$interval, interval_means$steps_mean, type = "l", xlab = "5-minute interval",
     ylab = "Average Number of Steps", 
     main = "Time Series Plot of the Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
  #Find interval with the maximum number of steps
  max_interval_steps<-max(interval_means$steps_mean)
  max_interval_index<-which(interval_means$steps_mean==max_interval_steps)
  max_interval<-as.numeric(interval_means[max_interval_index,1])
```
The 5-minute interval that contains the maximum number of steps on average across all the days in the dataset is 835.  The maximum number of average steps is 206.1698113.

## Imputing missing values

```r
  #Calculate and report the total number of missing values in the dataset 
  missing_count<-sum(is.na(activity))
```
There are 2304 missing values.
Missing data is imputed by using the mean for that 5-minute interval.

```r
  #impute missing data
  activity_copy<-activity
  na_index<-which(is.na(activity),arr.ind=TRUE)
  for (i in 1:nrow(na_index)) {
    iinterval<-activity_copy[na_index[i,1],3]
    iMean<-interval_means$steps_mean[interval_means$interval==iinterval]
    activity_copy[na_index[i,1],na_index[i,2]]<-iMean
  }
  
  #Make a histogram of the total number of steps taken each day
  totalsNew<-activity_copy %>% group_by(date) %>% summarise(steps_total = sum(steps))
  totals_histNew<-ggplot(totalsNew, aes(steps_total)) + geom_histogram(binwidth = 5000) + 
    theme_classic() +
    ggtitle("Total Number of Steps Taken Each Day") + xlab("Steps") + ylab("Frequency")
  totals_histNew
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
  #Calculate and report the mean and median total number of steps taken per day
  mean_daily_stepsNew<-mean(totalsNew$steps_total, na.rm = TRUE)
  median_daily_stepsNew<-median(totalsNew$steps_total, na.rm = TRUE)
```
The mean of the total number of steps taken per day is 1.0766189\times 10^{4}.
The median of the total number of steps taken per day is 1.0766189\times 10^{4}.
Only the median barely differs from the previous estimates.

## Are there differences in activity patterns between weekdays and weekends?

```r
  #Create a new weekday factor variable
  weekdaysList <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
  activity_copy$weekday<-factor((weekdays(activity_copy$date) %in% weekdaysList), 
                              levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
  #Make a panel plot containing a time series plot
  interval_means_weekday<-activity_copy %>% group_by(interval,weekday) %>% 
    summarise(steps_mean = mean(steps, na.rm = TRUE))
  xyplot(interval_means_weekday$steps_mean~interval_means_weekday$interval|interval_means_weekday$weekday,
       type = "l",
       main="Time Series Plot of the Average Number of Steps Taken \nAcross Weekdays and Weekends",
       ylab="Number of Steps", xlab="Interval",
       layout = c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
