---
title: "PA1_template.Rmd"
author: "Luis Valladares"
date: "4 de junio de 2018"
output: html_document
keep_md: true 
---


```r
library(knitr)
library(ggplot2)
library(data.table)
```


```r
opts_chunk$set(echo = TRUE)
```

1. Code for reading in the dataset and/or processing the data

```r
dt <- fread('activity.csv', header = T)
dt$date <- as.Date(dt$date, '%Y-%m-%d')
```

2. Histogram of the total number of steps taken each day

```r
dt0 <- na.omit(dt)
g <- ggplot(dt0, aes(steps))
g1 <- g + geom_histogram(fill = 'yellow') + labs(x = 'Number of steps taken each day') + scale_y_continuous(breaks=seq(0, 12000, 3000))
g1
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk Histogram Steps](figure/Histogram Steps-1.png)


3. Mean and median number of steps taken each day

```r
x <- mean(dt$steps, na.rm = T)
x
```

```
## [1] 37.3826
```

```r
x1 <- median(dt$steps, na.rm = T)
x1
```

```
## [1] 0
```

4. Time series plot of the average number of steps taken

```r
dt1 <- dt0[, .(stepsAve = mean(steps)), by = date]
h <- ggplot(dt1, aes(date, stepsAve))
h1 <- h + geom_line(color = "red") + labs(y = 'Average number of steps') + labs(title = 'Time series plot')
h1
```

![plot of chunk Time series steps](figure/Time series steps-1.png)


5. The 5-minute interval that, on average, contains the maximum number of steps

```r
dt[which.max(steps), ]
```

```
##    steps       date interval
## 1:   806 2012-11-27      615
```


6. Code to describe and show a strategy for imputing missing data

```r
dt2 <- dt[, .(stepsN = ifelse(is.na(steps) == T, ifelse(is.na(mean(steps)) == T, 0, mean(steps)), as.double(steps)), steps, interval), by = date]
dt2
```

```
##              date stepsN steps interval
##     1: 2012-10-01      0    NA        0
##     2: 2012-10-01      0    NA        5
##     3: 2012-10-01      0    NA       10
##     4: 2012-10-01      0    NA       15
##     5: 2012-10-01      0    NA       20
##    ---                                 
## 17564: 2012-11-30      0    NA     2335
## 17565: 2012-11-30      0    NA     2340
## 17566: 2012-11-30      0    NA     2345
## 17567: 2012-11-30      0    NA     2350
## 17568: 2012-11-30      0    NA     2355
```

7. Histogram of the total number of steps taken each day after missing values are imputed

```r
k <- ggplot(dt2, aes(stepsN))
k1 <- k + geom_histogram(fill = 'green') + labs(x = 'Number of steps taken each day') + scale_y_continuous(breaks=seq(0, 15000, 3000))
k1
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk Histogram StepsN](figure/Histogram StepsN-1.png)

8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
dt3 <- dt2[,.(date, TypeDay = ifelse(weekdays(date) %in% c('sábado', 'domingo'), 'weekend', 'weekday'), stepsN, steps, interval)]
dt3$TypeDay <- as.factor(dt3$TypeDay) 

dt4 <- dt3[, .(stepsAveXInt = mean(stepsN)), by = .(interval, TypeDay)]

m <- ggplot(dt4, aes(interval, stepsAveXInt))
m1 <- m + geom_line(color = "blue") + facet_wrap(~TypeDay, nrow = 2, ncol = 1) + labs(y = 'Average number of steps')
m1
```

![plot of chunk Plot Interval vs Average number of steps](figure/Plot Interval vs Average number of steps-1.png)
