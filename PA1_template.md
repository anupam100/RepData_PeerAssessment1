# Reproducible Research: Peer Assessment 1



## Load libraries

```r
library(ggplot2)
library(lubridate,warn.conflicts = FALSE)
```


## Loading and preprocessing the data


```r
# CLEAN OBJECTS FROM WORKSPACE
rm(list=ls())

# WORKING DIRECTORY
setwd("C:/RepData_PeerAssessment1/")

# LOAD DATA
unzip("activity.zip")
df_activity <- read.csv("activity.csv", sep=",", stringsAsFactors = FALSE, header = TRUE)

# TRANSFORM TO DATE
df_activity$date <- as.factor(as.Date(df_activity$date, format = "%Y-%m-%d"))
```



## What is mean total number of steps taken per day?


```r
## HISTOGRAM OF TOTAL NUMBER OF STEPS TAKEN EACH DAY 
total_steps <- with(df_activity, tapply(steps, date, FUN = sum, na.rm = TRUE))
qplot(total_steps, binwidth = 1000, ylab = "Frecuency", xlab="Number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## MEAN AND MEDIAM OF THE TOTAL NUMBER OF STEPS TAKEN PER DAY
mean(total_steps)
```

```
## [1] 9354.23
```

```r
median(total_steps) 
```

```
## [1] 10395
```



## What is the average daily activity pattern?


```r
## AVERAGE DAILY OF STEPS TAKEN EACH INTERVAL
daily_average <- aggregate(steps ~ interval, df_activity[!is.na(df_activity$steps),], mean)

ggplot(daily_average, aes(x=interval, y=steps)) + geom_line() +
    xlab("5-minute interval") + ylab("Average number of steps") + 
    ggtitle("Time Series Plot")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
max_interval <- daily_average[which.max(daily_average$step),]
```

The **5-minute interval that contains the maximum number of steps** is: 835 with 206.1698113 mean of steps



## Imputing missing values


```r
## TOTAL OF MISSING VALUES
summary(df_activity)
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

```r
table(is.na(df_activity$steps))
```

```
## 
## FALSE  TRUE 
## 15264  2304
```

```r
total_na <- sum(is.na(df_activity$steps))

## FILLING IN ALL OF THE MISSING VALUES
# - Strategy: reemplace NA by the mean for that 5 minute interval
df_activity_withoutNA <- df_activity
intervals_na <- unique(df_activity[is.na(df_activity$steps),"interval"])
for(i in intervals_na){
    df_activity_withoutNA[is.na(df_activity_withoutNA$steps) & df_activity_withoutNA$interval==i, ]$steps <- daily_average[daily_average$interval==i,]$steps
}

## HISTOGRAM OF TOTAL NUMBER OF STEPS TAKEN EACH DAY 
total_steps <- with(df_activity_withoutNA, tapply(steps, date, FUN = sum, na.rm = TRUE))
qplot(total_steps, binwidth = 1000, ylab = "Frecuency", xlab="Number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
## MEAN AND MEDIAM OF THE TOTAL NUMBER OF STEPS TAKEN PER DAY
mean(total_steps)
```

```
## [1] 10766.19
```

```r
median(total_steps) 
```

```
## [1] 10766.19
```



## Are there differences in activity patterns between weekdays and weekends?


```r
## DETERMINE IS WEEKDAY OR WEEKEND DAY
df_activity_withoutNA$day <- "weekday"
df_activity_withoutNA[wday(df_activity_withoutNA$date, label=TRUE) %in% c("Sun","Sat"), ]$day <- "weekend"
table(df_activity$day)
```

```
## < table of extent 0 >
```

```r
## AVERAGE DAILY OF STEPS TAKEN EACH INTERVAL
daily_sum <- aggregate(steps ~ interval+day, df_activity_withoutNA[!is.na(df_activity_withoutNA$steps),], mean)

ggplot(daily_sum, aes(x=interval, y=steps)) + 
    geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Average number of steps") + 
    ggtitle("Time Series Plot")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->