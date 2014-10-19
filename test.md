---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    
---


## Loading and preprocessing the data



```r
1*1
```

```
## [1] 1
```

```r
setwd("./ReproducibleResearchAssignment1")
```

```
## Error in setwd("./ReproducibleResearchAssignment1"): cannot change working directory
```

```r
## Read in activity data
activity <- read.csv("activity.csv")
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

## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
