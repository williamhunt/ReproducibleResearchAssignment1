## Move all the programming and output to its own directory. Check and set up directories for raw data, working directory containing the code, and figures.

## Check and if necessary create a working directory for the assignment
if (!file.exists("./ReproducibleResearchAssignment1"))
{
        dir.create("./ReproducibleResearchAssignment1")
}
## Check and if necessary create a figure directory for assignment
if (!file.exists("./ReproducibleResearchAssignment1/figure"))
{
        dir.create("./ReproducibleResearchAssignment1/figure")
}
## Check to see if a data directory and a zip file exist. Create the directory and download the zipfile as necessary.
if (!file.exists("./data"))
{
        dir.create("./data")
}
if (!file.exists("./data/RRAssignment1activity.zip"))
{
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileURL,"./data/RRAssignment1activity.zip", mode = "wb")
        ## save download date
        dateDownloaded <- date()
        sink("./ReproducibleResearchAssignment1/dateDataDownloaded.txt")
        cat("Date data downloaded: ")
        cat(dateDownloaded)
        sink()
}
## Read in activity data
activity <- read.csv(unz("./data/RRAssignment1activity.zip","activity.csv"))
## Set the working directory
setwd("./ReproducibleResearchAssignment1")

## Aggregate the data to calculate the sums for the steps.
sumA <- aggregate(steps ~ date ,data = activity, sum)
meanA <- mean(sumA$steps)
medianA <- median(sumA$steps)
hist(sumA$steps, main="Histogram of Total Daily Steps", xlab = "Total Steps")
# Add the mean and median to the histogram
abline(v = meanA, col = "red")
points(meanA, 1, col = "red", pch = 3)
meantext <- paste('mean    =', as.character(trunc(meanA)))
text(meanA,1, meantext, pos = 4, col = "red")
points(medianA, 3, col = "red", pch = 4)
mediantext <- paste('median =', as.character(trunc(medianA)))
text(medianA,3, mediantext, pos = 4, col = "red")
dev.copy(png, file= "./figure/histogramTotalStepsIgnoredNAs.png")
dev.off()
# Create a time series plot for average steps in each interval.
aveI <- aggregate(steps ~ interval ,data = activity, mean)
maxI <- aveI$interval[which.max(aveI$steps)]
plot(aveI$interval,aveI$steps, type = "l", xlab = "HourMinute Time Interval", ylab = "Average # of Steps")
# added the max steps interval to the plot
abline(v = maxI, col = "red")
text(maxI, 0, as.character(maxI), col = "red")
# The interval format is in hours and minutes, HHHMM. To get the step index, we need the formula converting intervals to row numbers.
rowNum <- 12*trunc(maxI/100) + (maxI %% 100)/5 + 1
text(maxI, aveI$steps[rowNum], as.character(trunc(aveI$step[rowNum])), pos = 2, col = "red", srt = 90)
dev.copy(png, file= "./figure/averageStepsPerInterval.png")
dev.off()
## calculate the missing data, NAs = total # steps - # complete cases
naT <- length(activity$steps) - sum(complete.cases(activity$steps))
# Replace all NAs with interval average
aveS <- aveI$steps
actS <- activity
actS$steps[is.na(actS$steps)] <- aveS
## I aggregate the filled-in data to calculate the sums for the steps.
sumC <- aggregate(steps ~ date ,data = actS, sum)
meanC <- mean(sumC$steps)
medianC <- median(sumC$steps)
hist(sumC$steps, main="Histogram of Total Daily Steps Replacing NAs with Interval Means", xlab = "Total Steps")
# added the mean and median to the filled-in histogram
abline(v = meanC, col = "red")
points(meanC, 1, col = "red", pch = 3)
meantext <- paste('mean    =', as.character(trunc(meanC)))
text(meanC,1, meantext, pos = 4, col = "red")
points(medianC, 3, col = "red", pch = 4)
mediantext <- paste('median =', as.character(trunc(medianC)))
text(medianC,3, mediantext, pos = 4, col = "red")
dev.copy(png, file= "./figure/histogramTotalStepsFilledNAs.png")
dev.off()
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
names(stepsAveInt)[1] <- "Interval"
names(stepsAveInt)[2] <- "dayType"
xyplot(steps ~ interval | dayType, data = stepsAveInt, layout = c(1,2), type = "l", xlab = "Interval", ylab = "Number of Steps")
dev.copy(png, file= "./figure/AverageStepsbyDayType.png")
dev.off()
## Set the working directory back to the original
## setwd("..")
