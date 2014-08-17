---
title: "Reproducible Research Assignment 1"
author: "Amr Hassan"
---

For this assignment I perform data analysis on the output of an activity
monitoring device of one person over the course of 61 days.

### The Source Raw Data
The data for these analyses are obtained from the URL: <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip> on August 16, 2014. The checksum of the file used for this report is `"61e0c1f8c4a736ff1ab0f8863344b301"`.

```r
# Downloading the source raw data and verifying its authenticity
library(tools)
if (!file.exists('activity.zip')) {
  download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip', 'activity.zip', method="curl")
  if (md5sum('activity.zip') != "61e0c1f8c4a736ff1ab0f8863344b301") {
    warning("The file downloaded differs from the original file on which the report's analysis were performed.")
  }
}
```

### Loading and Preprocessing the Raw Data
The data is a Zip-compressed CSV file in its raw format. I load it into
memory in order to perform my analysis first.

```r
data <- read.csv(unz('activity.zip', 'activity.csv'))
data$date <- as.Date(data$date)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### What is mean total number of steps taken per day?
The following is a histogram of the number of steps taken per day over
the monitoring period.

```r
library(reshape2)
library(ggplot2)
molten <- melt(data, id.vars=c('date'))
unmelted <- dcast(molten, date ~ variable, fun.aggregate=sum, na.rm=T)
mean.steps.per.day <- mean(unmelted$steps)
median.steps.per.day <- median(unmelted$steps)
ggplot(unmelted, aes(x=steps)) + 
  geom_histogram(binwidth=(max(unmelted$steps) - min(unmelted$steps))/30, 
                 fill="white", colour="black") +
  xlab('Total Number of Steps on a Single Day') + ylab('Frequency') + 
  geom_vline(aes(xintercept=mean.steps.per.day), linetype='dashed', colour='red') +
  geom_vline(aes(xintercept=median.steps.per.day), colour='green', linetype='dashed')
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

The **mean** and **median** statistics of the total number of steps on a single day are **9354.2295** and **10395** respectively.

### What is the average daily activity pattern?
In order to find the average daily activity pattern, I plot the average number of steps taken at each time of day against the times of the day.

```r
molten <- melt(data, id.vars=c('interval', 'date'))
average.steps.per.interval <- dcast(molten, interval ~ variable, fun.aggregate=mean, na.rm=T)[,c('interval', 'steps')]
qplot(interval, steps, data=average.steps.per.interval, geom="line", xlab="Time of day", ylab="Average Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


```r
maximum.activity.interval.i <- which.max(average.steps.per.interval$steps)
t <- average.steps.per.interval$interval[maximum.activity.interval.i];
t <- sprintf("%04d", t);
t <- gsub('([0-9]{2})([0-9]{2})', '\\1:\\2', t)
```
The maximum activity is at **08:35**, based on the average number of steps taken across the monitoring period.

### 