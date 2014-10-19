## I move all the programming and output to its own directory Code elsewhere checks and sets up the new directory
setwd("./ReproducibleResearchAssignment1")
## Read in activity data
activity <- read.csv("activity.csv")
## 
# install.packages("reshape2")
# library("reshape2")
## wide <- dcast(activity, interval ~ date, value.var = "steps")
sumA <- aggregate(steps ~ date ,data = activity, sum)
hist(sumA$steps)
dev.copy(png, file= "histogramTotalStepsIgnoredNAs.png")
dev.off()
meanA <- mean(sumA$steps)
medianA <- median(sumA$steps)

aveI <- aggregate(steps ~ interval ,data = activity, mean)
plot(aveI$interval,aveI$steps, type = "l")
dev.copy(png, file= "averageStepsPerInterval.png")
dev.off()
maxI <- aveI$interval[which.max(aveI$steps)]

naT <- length(activity$steps) - sum(complete.cases(activity$steps))
# Replace all NAs with interval average
aveS <- aveI$steps
actS <- activity
actS$steps[is.na(actS$steps)] <- aveS

sumC <- aggregate(steps ~ date ,data = actS, sum)
hist(sumC$steps)
dev.copy(png, file= "histogramTotalStepsFilledNAs.png")
dev.off()
meanC <- mean(sumC$steps)
medianC <- median(sumC$steps)
## testing for weekday/weekend factor creation
dateAsDate <- as.Date(activity$date, format = "%Y-%m-%d")
daylist <- weekdays(dateAsDate,abbreviate = TRUE)
daylist[daylist %in% c("Sat","Sun")] <- "weekend"
daylist[daylist != "weekend"] <- "weekday"
activeWeek <- cbind(actS,daylist)
## calculate averages for weekday and weekend intervals
stepsAveInt <- aggregate(activeWeek, by=list(activeWeek$interval,activeWeek$daylist),FUN="mean")

install.packages("lattice")
library("lattice")

stepsAveInt <- aggregate(activeWeek, by=list(activeWeek$interval,activeWeek$daylist),FUN="mean")
names(stepsAveInt)[1] <- "interval"
names(stepsAveInt)[2] <- "dayType"
xyplot(steps ~ interval | dayType, data = stepsAveInt, layout = c(1,2), type = "l")
dev.copy(png, file= "AverageStepsbyDayType.png")
dev.off()



## Set the working directory back to the original
## setwd("..")
