# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv("activity/activity.csv", colClasses = c("numeric", "character", "numeric"))
head(activity)
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
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```
**Converting to date from character**

```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
totalStepsPerDay <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```
**The plot for total steps by day is shown below**

```r
hist(totalStepsPerDay$steps, main = "Total steps by day", xlab = "Day", ylab = "Total Steps", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

**The mean for total teps per day is calculated below**

```r
mean(totalStepsPerDay$steps)
```

```
## [1] 10766.19
```
**The median of total steps per day is calculated below**

```r
median(totalStepsPerDay$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
**The average steps per time interval is calculated below**

```r
avgStepsPerInterval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
```
**The time series plot is shown below**

```r
plot(avgStepsPerInterval, type = "l", xlab = "5 minute intervals", 
    ylab = "Average across all Days", main = "Average number of steps taken", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

**The time interval with the maximum number of steps is**

```r
avgStepsPerInterval[which.max(avgStepsPerInterval$steps),1]
```

```
## [1] 835
```

## Imputing missing values
**The total number of missing values are**

```r
sum(is.na(activity))
```

```
## [1] 2304
```
**Creating anoter data frame called *completeActivityData* with the missing values filled in.** 
**The missng values are filled in using the mean steps per interval calculated earlier**

```r
completeActivityData <- merge(activity, avgStepsPerInterval, by.x = "interval", 
                              by.y = "interval", all.x=TRUE, all.y=FALSE)
completeActivityData <- within(completeActivityData, steps.x[is.na(steps.x)] <- steps.y[is.na(steps.x)])
completeActivityData$steps.y <- NULL
colnames(completeActivityData)[2] <- "steps"
completeActivityData <- completeActivityData[with(completeActivityData, order(date,interval)), ]
```
**Histogram of total steps per day using the filled in data set**

```r
ttspdComplete <- aggregate(steps ~ date, data = completeActivityData, sum, na.rm = TRUE)
hist(ttspdComplete$steps, main = "Total steps by day", xlab = "Day", ylab = "Total Steps", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

**Mean of total steps per day using complete data set**

```r
mean(ttspdComplete$steps)
```

```
## [1] 10766.19
```
**Median of total  steps per day using complete data set**

```r
median(ttspdComplete$steps)
```

```
## [1] 10766.19
```
**The mean remains the same even after filling in the missing values. The median, however, has changed and is now the same as the mean. This is the result of NAs beng replaced by the means for the respective intervals.**

## Are there differences in activity patterns between weekdays and weekends?
**Adding *day of week* and *weekday/weekend* columns to the complete data set.**

```r
completeActivityData$dayOfWeek <- weekdays(completeActivityData$date)
completeActivityData$dayType <- ifelse(completeActivityData$dayOfWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
completeActivityData$dayType <- as.factor(completeActivityData$dayType)
```
**The plot showing steps over intervals for Weekdays and Weekends:**

```r
stepsByDayType <- aggregate(steps ~ interval + dayType, data = completeActivityData, mean)
library(lattice)
xyplot(steps ~ interval | dayType, stepsByDayType, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 

