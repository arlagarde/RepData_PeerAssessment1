---
title: "Reproducible Research Peer assesment 1"
author: "A Lagarde"
date: "Saturday, January 17, 2015"
output: html_document
---

Loading and preprocessing the data
----------------------------------


```r
library(lattice)
data<-read.csv("activity.csv")
data$date<-as.Date(data$date)
dataNoNa<-na.omit(data)
```

What is mean total number of steps taken per day?
-------------------------------------------------


```r
dataDay<-aggregate(steps~date,data=dataNoNa,sum)
barplot(dataDay$steps,names.arg=dataDay$date,xlab="Date",ylab="Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

We calculate the median steps per day


```r
median(dataDay$steps)
```

```
## [1] 10765
```

and the mean per day


```r
mean(dataDay$steps)
```

```
## [1] 10766.19
```

What is the average daily activity pattern?
-------------------------------------------


```r
dataInt<-aggregate(steps~interval,data=dataNoNa,mean)
plot(dataInt,type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
dataInt$interval[which(dataInt$steps==max(dataInt$steps))]
```

```
## [1] 835
```

The 5-minute interval, which on average across all the days in the dataset, contains the maximum number of steps is 835, or 8h35.

Inputing missing values
-----------------------


```r
naRow<-which(is.na(data$steps))
length(naRow)
```

```
## [1] 2304
```

2304 values are missing
We will fill those missing values with the mean of the 5mn intervals, and create a new dataset called dataFill


```r
dataFill<-data
dataFill$steps[naRow]<-dataInt$steps[match(dataFill$interval[naRow],dataInt$interval)]
```

We used these dataset to do the same histogram and calculations (median and mean) as in the first question


```r
dataDay2<-aggregate(steps~date,data=dataFill,sum)
barplot(dataDay2$steps,names.arg=dataDay2$date,xlab="Date",ylab="Steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
median(dataDay2$steps)
```

```
## [1] 10766.19
```

```r
mean(dataDay2$steps)
```

```
## [1] 10766.19
```

There is little impact on the calcul of the median and none on the mean

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------


```r
dataFill$day<-as.numeric(format(dataFill$date,"%u"))
dataFill$we<-NA
dataFill$we[which(dataFill$day>5)]<-"weekend"
dataFill$we[which(dataFill$day<6)]<-"weekday"
dataFillInt<-aggregate(steps~interval+we,data=dataFill,mean)
xyplot(steps~interval|we,data=dataFillInt,type="l",scales=list(y=list(relation="free")),layout=c(1,2))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

We can conclude from the Graphs, that during the weekday most of the activity is during the morning, around the 8h35 peak, most likely when people go to work; during weekend the morning peak is a bit weaker, but activity is more equally distributed.
