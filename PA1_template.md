---
title: 'Reproducible research: Course Project 1'
author: "Aleksei Dudin"
date: "August 9, 2018"
output:
  html_document:
    keep_md: true
---

## Hi, lets start the peer-graded assignment!

### Load libraries


```r
library(ggplot2)
library(knitr)
```

### Read the data


```r
path <- 'C:/Users/Aleksei Dudin/Documents/data1/activity.csv'
data <- read.csv(path)
```

### Check the data


```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

### What is mean total number of steps taken per day?


```r
steps.per.day <- tapply(data$steps, data$date, FUN = sum, na.rm = T)

qplot(steps.per.day, binwidth = 1000,
      color = I('red'),
      fill = I('white'),
      xlab='Total number of steps taken per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
mean(steps.per.day, na.rm = T)
```

```
## [1] 9354.23
```


```r
median(steps.per.day, na.rm = T)
```

```
## [1] 10395
```


```r
sum (steps.per.day, na.rm = T)
```

```
## [1] 570608
```

### What is the average daily activity pattern?


```r
averages <- aggregate(x = list(steps = data$steps),
                      by = list(interval = data$interval),
                      FUN = mean, na.rm = T)

ggplot(data = averages, aes(x = interval, y = steps)) +
    geom_line(col = 'red') +
    xlab('5-minute intervals') +
    ylab('Average number of steps taken')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averages[which.max(averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

### Imputing missing values

Calculate and report the total number of missing values in the dataset.


```r
sum(is.na(data))
```

```
## [1] 2304
```

Use mean to fill in the `NA` values.


```r
fill.cells <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps)) filled <- c(steps) 
    else filled <- (averages[averages$interval == interval, 'steps'])
        return(filled)
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filled.data <- data
filled.data$steps <- mapply(fill.cells, filled.data$steps,
                            filled.data$interval)
```

Make a histogram of the total number of steps taken each day.


```r
filled.steps.per.day <- tapply(filled.data$steps, filled.data$date, FUN = sum)
qplot(filled.steps.per.day, binwidth = 1000, colour = I('red'),
      fill = I('white'), xlab = 'Total number of steps taken per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

 Calculate and report the mean and median total number of steps taken per day.
 

```r
mean(filled.steps.per.day)
```

```
## [1] 10766.19
```

```r
median(filled.steps.per.day)
```

```
## [1] 10766.19
```

```r
sum(filled.steps.per.day)
```

```
## [1] 656737.5
```

Remember the values without filled in `NA`.


```r
mean(steps.per.day, na.rm = T)
```

```
## [1] 9354.23
```

```r
median(steps.per.day, na.rm = T)
```

```
## [1] 10395
```

```r
sum (steps.per.day, na.rm = T)
```

```
## [1] 570608
```

Mean, median and total number steps values are higher after imputing missing data.

### Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday',
                   'Friday')) 
        return('weekday') else if (day %in% c('Saturday', 'Sunday')) 
            return('weekend') else stop('no date')
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN = weekday.or.weekend)
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
averages <- aggregate(steps ~ interval + day, data = filled.data, mean)
ggplot(averages, aes(interval, steps, color = day))+
    geom_line(show.legend=F) + facet_grid(day~.) + 
    xlab('5-minute intervals') + ylab('Number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

## Thank you for reading!
