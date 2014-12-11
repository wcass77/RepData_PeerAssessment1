# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
####Here I load the data directly from the zip file in the repository

```r
data <- read.csv(unz("activity.zip", "activity.csv"), stringsAsFactors = FALSE)
```

####Convert date column to 'Date' class

```r
data$date <- as.Date(data$date, format = "%Y-%m-%d")
```
  
####Convert intervals into minutes since midnight (from time)

```r
data$interval <- (data$interval %% 100) + as.integer(data$interval / 100) * 60
```
  
## What is mean total number of steps taken per day?


```r
# Create new data frame with number of steps for each day
dates <- unique(data$date)
steps.by.date <- sapply(dates, FUN = function(x) {sum(data$steps[data$date == x], na.rm = TRUE)})

hist(steps.by.date, breaks = 10)
```

![](PA1_template_files/figure-html/by-date-1.png) 

```r
mean(steps.by.date)
```

```
## [1] 9354.23
```

```r
median(steps.by.date)
```

```
## [1] 10395
```
## What is the average daily activity pattern?


```r
intervals <- unique(data$interval)
mean.steps.by.interval <- sapply(intervals, FUN = function(x) {
  mean(data$steps[data$interval == x], na.rm = TRUE)})

plot(intervals, mean.steps.by.interval, type = 'l')
```

![](PA1_template_files/figure-html/by-interval-1.png) 

```r
intervals[which.max(mean.steps.by.interval)]
```

```
## [1] 515
```

## Imputing missing values


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
imputed.steps <- as.integer(apply(data, 1, function(row) {
  if(is.na(row["steps"]))
    row["steps"] <- as.integer(round(
      mean.steps.by.interval[intervals == as.numeric(row["interval"])]))
  return(row["steps"])}))
imputed.data <- data
imputed.data$steps <- imputed.steps

steps.by.date.imputed <- sapply(dates, FUN = function(x) {
  sum(imputed.data$steps[dates == x])})

hist(steps.by.date.imputed, breaks = 7)
```

![](PA1_template_files/figure-html/missing-data-1.png) 

```r
mean(steps.by.date.imputed)
```

```
## [1] 10765.64
```

```r
median(steps.by.date.imputed)
```

```
## [1] 10789
```

## Are there differences in activity patterns between weekdays and weekends?


```r
imputed.data$weekend <- as.factor(sapply(weekdays(imputed.data$date), function(x) {
  if (x == "Saturday" || x == "Sunday")
    return("weekend")
  return("weekday")
}))
mean.steps.weekend.by.interval <- sapply(intervals, FUN = function(x) {
  mean(imputed.data$steps[imputed.data$interval == x & imputed.data$weekend == "weekend"])
  })
mean.steps.weekday.by.interval <- sapply(intervals, FUN = function(x) {
  mean(imputed.data$steps[imputed.data$interval == x & imputed.data$weekend == "weekday"])
  })
par(mfcol = c(2,1), mar = c(4, 4, 2, 4))
plot(intervals,mean.steps.weekday.by.interval, type='l', ylab = "steps", main = "Weekday")
plot(intervals,mean.steps.weekend.by.interval, type='l', ylab = "steps", main = "Weekend")
```

![](PA1_template_files/figure-html/weekdays-1.png) 

