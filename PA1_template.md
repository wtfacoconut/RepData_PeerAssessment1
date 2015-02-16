# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
knitr::opts_chunk$set(cache=TRUE, echo=TRUE) # Setting global options
library(dplyr)
library(lubridate)
library(stringr)
library("lattice")

myDat <- read.csv("activity.csv")
myDat <- myDat[complete.cases(myDat),] # Removing incomplete cases
myDat$date <- as.factor(myDat$date) # Setting the Date in the dataset as a factor

# adding a column with a proper date-time stamp and another columns with just the time
myDat$interval <- str_pad(myDat$interval, 4, pad = "0")
myDat$time <- parse_date_time(paste(myDat$interval, "00", sep = ""), "%H%M%S")

myDat$dateTime <- paste(myDat$date, myDat$interval, sep = ":")
myDat$dateTime <- parse_date_time(myDat$dateTime, "%y%m%d:%H%M")

byDayGrouping <- group_by(myDat, date)
byIntervalGrouping <- group_by(myDat, interval)

totalSteps <- sum(myDat$steps)
totalStepsAvg <- mean(myDat$steps)
```


## What is mean total number of steps taken per day?

```r
totalStepsByDay <- summarise(byDayGrouping, totalSteps=sum(steps))
hist(totalStepsByDay$totalSteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
stepsByDayTotalAvg <- mean(totalStepsByDay$totalSteps)
print(paste("Mean total number of steps taken per day:", as.character(stepsByDayTotalAvg)))
```

```
## [1] "Mean total number of steps taken per day: 10766.1886792453"
```

```r
totalStepsMedian <- median(totalStepsByDay$totalSteps)
print(paste("Median number of steps taken per day:", as.character(totalStepsMedian)))
```

```
## [1] "Median number of steps taken per day: 10765"
```


## What is the average daily activity pattern?

```r
xyplot(steps ~ date, data = byDayGrouping[byDayGrouping$steps > 0,], type = "l", scales=list(x=list(rot=45)))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxSteps <- max(byDayGrouping[byDayGrouping$steps > 0,]$steps)
intervalWithMax <- byDayGrouping[byDayGrouping$steps == maxSteps,]$interval
print(paste("Interval(s): ", intervalWithMax, ", Had the greates number of steps of: ", maxSteps, sep = ""))
```

```
## [1] "Interval(s): 0615, Had the greates number of steps of: 806"
```


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
