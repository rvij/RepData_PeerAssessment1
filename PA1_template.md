Reproducible Research Peer-Review Excercise 1 
========================================================

Excercise submitted by Rajeev Vij
Thanks for reviewing

## Step 1 Read data file and produce summary information


```r

activity <- read.csv("activity.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
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
summary(activity)
```

```
##      steps           date              interval   
##  Min.   :  0.0   Length:17568       Min.   :   0  
##  1st Qu.:  0.0   Class :character   1st Qu.: 589  
##  Median :  0.0   Mode  :character   Median :1178  
##  Mean   : 37.4                      Mean   :1178  
##  3rd Qu.: 12.0                      3rd Qu.:1766  
##  Max.   :806.0                      Max.   :2355  
##  NA's   :2304
```


## Step 1 Draw chart for total steps per day and avg steps per interval excluding missing data


```r
library(sqldf)
```

```
## Loading required package: gsubfn
## Loading required package: proto
## Loading required namespace: tcltk
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: RSQLite.extfuns
```

```r

ex1 <- sqldf("select date, sum(steps) as steps_per_day from activity where steps <> 'NA' group by date")
```

```
## Loading required package: tcltk
```

```r

library(ggplot2)

qplot(steps_per_day, data = ex1, geom = "histogram", binwidth = 1000)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-21.png) 

```r

ex2 <- sqldf("select interval, avg(steps) as avg_steps from activity where steps <> 'NA' group by interval")

maxval <- ex2[ex2$avg_steps == max(ex2$avg_steps), ]

ggplot(ex2, aes(ex2$interval, ex2$avg_steps)) + geom_line() + geom_vline(xintercept = maxval$interval) + 
    annotate("text", x = maxval$interval:maxval$interval, y = 0, label = as.character(maxval$interval))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-22.png) 


## Step 3 Impute missing data and draw total steps per interval and produce summary

```r
activity_nona <- activity
sum(is.na(activity_nona$steps))
```

```
## [1] 2304
```

```r

naindx <- which(is.na(activity_nona$steps))

for (i in naindx) {
    activity_nona$steps[i] <- ex2[ex2$interval == activity_nona$interval[i], 
        "avg_steps"]
}

ex11 <- sqldf("select date, sum(steps) as steps_per_day from activity_nona where steps <> 'NA' group by date")

qplot(steps_per_day, data = ex11, geom = "histogram", binwidth = 1000)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r

summary(activity_nona)
```

```
##      steps           date              interval   
##  Min.   :  0.0   Length:17568       Min.   :   0  
##  1st Qu.:  0.0   Class :character   1st Qu.: 589  
##  Median :  0.0   Mode  :character   Median :1178  
##  Mean   : 37.4                      Mean   :1178  
##  3rd Qu.: 27.0                      3rd Qu.:1766  
##  Max.   :806.0                      Max.   :2355
```


## Step 4: Add a factor variable daytype and produce facetd plot of avg_steps per interval 

```r
activity_nona$daytype <- as.factor(sapply(as.Date(activity_nona$date), function(d) {
    if (weekdays(d) %in% c("Saturday", "Sunday")) 
        "Weekend" else "Weekday"
}))

table(activity_nona$daytype)
```

```
## 
## Weekday Weekend 
##   12960    4608
```

```r

ex3 <- sqldf("select daytype, interval, avg(steps) as avg_steps from activity_nona group by daytype, interval")

p <- ggplot(ex3, aes(interval, avg_steps)) + geom_line()
p + facet_grid(daytype ~ .)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


