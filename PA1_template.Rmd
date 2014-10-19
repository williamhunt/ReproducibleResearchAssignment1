---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

This is my document for the Peer Assessment 1. I address the questions below with code examples.

## Loading and preprocessing the data
I set the directory and read in the source file.

```{r}
setwd("./ReproducibleResearchAssignment1")
## Read in activity data
activity <- read.cs
## I aggregate the data to calculate the sums for the steps.
sumA <- aggregate(steps ~ date ,data = activity, sum)

```
## What is mean total number of steps taken per day?
1. I draw the histogram for the total steps.
```{r}
hist(sumA$steps)
```
2. The mean and median are calculated with the code below:
```{r}
meanA <- mean(sumA$steps)
medianA <- median(sumA$steps)
```
They are: mean: 10766.19 and median: 10765

## What is the average daily activity pattern?
1. the code for the time series plot is below. Again, I aggregated the steps this time with intervals and the function, mean.
```{r}
aveI <- aggregate(steps ~ interval ,data = activity, mean)
plot(aveI$interval,aveI$steps, type = "l")
```
2. The plot shows the interval with the maximum average steps. The code below is the calculation.
```{r}

xI <- aveI$interval[which.max(aveI$steps)]

```

The interval is 835.

## Inputing missing values
1. To calculate the number of missing values I subtracted the number of complete cases from the total number of step entries, rows, The code is below. 
```{r}
naT <- length(activity$steps) - sum(complete.cases(activity$steps))

```
There were 2304 missing step counts.

2. The strategy I used to fill in the missing data was to use the means of each interval. The code is:
```{r}
aveS <- aveI$steps
actS <- activity
```
3. The new dataset was created by replacing the NA steps with the average values as shown in the code below.
```{r}
actS$steps[is.na(actS$steps)] <- aveS

```
4. The resulting histogram was created by the code below.
```{r}
sumC <- aggregate(steps ~ date ,data = actS, sum)
hist(sumC$steps)

```
The mean and median are calculated by the code below.
```{r}
meanC <- mean(sumC$steps)
medianC <- median(sumC$steps)

```
They are virtually the same as the values with the NAs ignored. 
They are: mean: 10766.19 and median: 10766.19. I tested other missing value replacements and got differences. It was using a good choice that kept them close. They was not impact when using good replacement choices.

## Are there differences in activity patterns between weekdays and weekends?
1. The new weekday/weekend factor and dataframe are shown in the code below.
```{r}
dateAsDate <- as.Date(activity$date, format = "%Y-%m-%d")
daylist <- weekdays(dateAsDate,abbreviate = TRUE)
daylist[daylist %in% c("Sat","Sun")] <- "weekend"
daylist[daylist != "weekend"] <- "weekday"
activeWeek <- cbind(actS,daylist)

```
2. The code to create the average steps is below.
```{r}
stepsAveInt <- aggregate(activeWeek, by=list(activeWeek$interval,activeWeek$daylist),FUN="mean")

```
The panel plot used lattice and the code is shown below.
```{r}
names(stepsAveInt)[1] <- "interval"
names(stepsAveInt)[2] <- "dayType"
xyplot(steps ~ interval | dayType, data = stepsAveInt, layout = c(1,2), type = "l")

```






