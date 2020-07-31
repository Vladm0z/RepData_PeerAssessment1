---
title: "Reproducible Research: Peer Assessment 1"
author: "Vladislav Mozgovoy"
date: "7/31/2020"
output: 
  html_document:
    keep_md: true
---


---

### Load libraries

```r
library(dplyr)
library(ggplot2)
library(scales)   
```


## Loading and preprocessing the data
Download and unzip file


```r
if (!file.exists(paste0(getwd(), '/data/activity.csv')) ){
  fileUrl <- "https://d396qusza40orc.clouStepsPerDayront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'))
  unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
}
StepsPerDay <- read.csv(paste0(getwd(), '/data/activity.csv'), stringsAsFactors = FALSE)
StepsPerDay$date <- as.Date(StepsPerDay$date)
str(StepsPerDay)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```



## Make histogram of the total number of steps taken each day


```r
stepsByDay <- StepsPerDay %>% group_by(date) %>% summarize(steps = sum(steps, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(stepsByDay)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <date>     <int>
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
hist(stepsByDay$steps, main="Steps by Day", xlab= "Steps", ylab="Number of Days", breaks=16, col="#E4E4E4")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Note that we ignored NA values with na.rm=TRUE

## What is mean total number of steps taken per day?

The mean number of steps per day is

```r
mean(stepsByDay$steps)
```

```
## [1] 9354.23
```

Add these value to the histogram 


```r
stepsByDay <- StepsPerDay %>% group_by(date) %>% summarize(steps = sum(steps, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(stepsByDay$steps, 
     main="Steps by Day", 
     xlab= "Steps", 
     ylab="Number of Days",
     breaks=16, 
     col="#E4E4E4")
abline(v=mean(stepsByDay$steps), lty=3, col="#FF4E32")
text(mean(stepsByDay$steps),6,labels="mean", pos=1, col="#FF3232") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



## What is the average daily activity pattern?

Summarize data by interval


```r
stepsByInterval <- StepsPerDay %>% group_by(interval) %>% summarize(steps = mean(steps, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(stepsByInterval)
```

```
## # A tibble: 6 x 2
##   interval  steps
##      <int>  <dbl>
## 1        0 1.72  
## 2        5 0.340 
## 3       10 0.132 
## 4       15 0.151 
## 5       20 0.0755
## 6       25 2.09
```

And visualize it in plot


```r
ggplot(data=stepsByInterval, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5min Interval") +
  ylab("Steps") + 
  scale_y_continuous(labels=comma) + 
  ggtitle("Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->



## Imputing missing values

#### Calculate number of missing values

Total number of rows with 1 or more NA

```r
sum(!complete.cases(StepsPerDay))
```

```
## [1] 2304
```

We will make a new variable 'StepsNew' that will fill in the missing values of 'steps' with the overall average for the interval

At first we will summarize the data by interval


```r
 stepsByInterval <- StepsPerDay %>% group_by(interval) %>% summarize(avgsteps = mean(steps, na.rm=TRUE)) %>% ungroup()
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
 head(stepsByInterval)
```

```
## # A tibble: 6 x 2
##   interval avgsteps
##      <int>    <dbl>
## 1        0   1.72  
## 2        5   0.340 
## 3       10   0.132 
## 4       15   0.151 
## 5       20   0.0755
## 6       25   2.09
```

Then merge it back to original dataframe


```r
StepsPerDay <- merge(StepsPerDay, stepsByInterval, by="interval", all.x = TRUE)
head(StepsPerDay)
```

```
##   interval steps       date avgsteps
## 1        0    NA 2012-10-01 1.716981
## 2        0     0 2012-11-23 1.716981
## 3        0     0 2012-10-28 1.716981
## 4        0     0 2012-11-06 1.716981
## 5        0     0 2012-11-24 1.716981
## 6        0     0 2012-11-15 1.716981
```

Then calculate 'StepsNew'


```r
 StepsPerDay <- StepsPerDay %>% mutate(StepsNew = ifelse(is.na(steps), avgsteps, steps))  
head(StepsPerDay)
```

```
##   interval steps       date avgsteps StepsNew
## 1        0    NA 2012-10-01 1.716981 1.716981
## 2        0     0 2012-11-23 1.716981 0.000000
## 3        0     0 2012-10-28 1.716981 0.000000
## 4        0     0 2012-11-06 1.716981 0.000000
## 5        0     0 2012-11-24 1.716981 0.000000
## 6        0     0 2012-11-15 1.716981 0.000000
```

And at the end we will create new dataset which also includes the new data


```r
StepsPerDay2 <- StepsPerDay %>% select(interval, date, StepsNew) %>% rename(steps = StepsNew)
head(StepsPerDay2)
```

```
##   interval       date    steps
## 1        0 2012-10-01 1.716981
## 2        0 2012-11-23 0.000000
## 3        0 2012-10-28 0.000000
## 4        0 2012-11-06 0.000000
## 5        0 2012-11-24 0.000000
## 6        0 2012-11-15 0.000000
```

Now we can make histogram of the total number of steps taken each day where missing values included


```r
 StepsNewByDay <- StepsPerDay %>% group_by(date) %>% summarize(StepsNew = sum(StepsNew, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
 hist(StepsNewByDay$StepsNew,  breaks=16, col="#E4E4E4", main="New Histogram of Steps by Day", xlab= "Steps By Day", ylab="Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
StepsPerDay <- StepsPerDay %>% mutate(day_type = ifelse(weekdays(StepsPerDay$date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
head(StepsPerDay)
```

```
##   interval steps       date avgsteps StepsNew day_type
## 1        0    NA 2012-10-01 1.716981 1.716981  Weekday
## 2        0     0 2012-11-23 1.716981 0.000000  Weekday
## 3        0     0 2012-10-28 1.716981 0.000000  Weekend
## 4        0     0 2012-11-06 1.716981 0.000000  Weekday
## 5        0     0 2012-11-24 1.716981 0.000000  Weekend
## 6        0     0 2012-11-15 1.716981 0.000000  Weekday
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
stepsByInterval <- StepsPerDay %>% group_by(day_type, interval) %>% summarize(steps = mean(steps, na.rm=TRUE)) %>% ungroup()
```

```
## `summarise()` regrouping output by 'day_type' (override with `.groups` argument)
```

```r
ggplot(data=stepsByInterval, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-Minute Interval") +
  ylab("Steps") + 
  scale_y_continuous(labels=comma) + 
  ggtitle("Average Steps") + facet_grid(day_type ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
