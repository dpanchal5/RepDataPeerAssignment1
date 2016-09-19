Reproducible Research–Project1
Divya Panchal

September 14, 2016

Loading and preprocessing the data
Set the working directory

setwd("D:/DivyaDataScience/ReproducibleResearch/Project/repdata_data_activity")
Read the file

activitydata<-read.csv("activity.csv")
head(activitydata)
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
What is mean total number of steps taken per day?
Calculate the number of steps taken

total<-aggregate(steps ~ date, data=activitydata, FUN = sum, na.rm = TRUE)
head(total)
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
Plotting histogram

hist(total$steps, main = "Total steps per day", xlab = "Steps", col ="blue")


Calculating the mean and median of the total number of steps taken per day

meanvalue<-mean(total$steps)
meanvalue
## [1] 10766.19
medianvalue<-median(total$steps)
medianvalue
## [1] 10765
What is the average daily activity pattern?
timeseries<-aggregate(steps ~ interval, data = activitydata, FUN = mean, na.rm = TRUE)
names(timeseries)<-c("Interval", "Mean")
head(timeseries)
##   Interval      Mean
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
Plotting

plot(timeseries$Interval, timeseries$Mean, type = "l", xlab = "5-min Interval", ylab = "Average for all days", main = "Time Series Plot", col="red")


maxposition<-which.max(timeseries$Mean)
maxpositionrow<-timeseries[maxposition,]
maxpositionrow$Interval
## [1] 835
Imputing missing values
Calculate and report the number of missing values

totalmissingvalue<-sum(is.na(activitydata))
totalmissingvalue
## [1] 2304
Creating a copy of dataset and assign 50 to NA

copyactivitydata<-activitydata
copyactivitydata[is.na(copyactivitydata)]<-50
head(copyactivitydata)
##   steps       date interval
## 1    50 2012-10-01        0
## 2    50 2012-10-01        5
## 3    50 2012-10-01       10
## 4    50 2012-10-01       15
## 5    50 2012-10-01       20
## 6    50 2012-10-01       25
newtotal<-aggregate(steps ~ copyactivitydata$date, data=copyactivitydata, FUN = sum)
hist(newtotal$steps, main = "Total steps per day with NA replaced with 50", xlab = "Steps", col ="blue")


newmeanvalue<-mean(newtotal$steps)
newmeanvalue
## [1] 11242.75
newmedianvalue<-median(newtotal$steps)
newmedianvalue
## [1] 11458
summary(newtotal$steps)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   11460   11240   14400   21190
Are there differences in activity patterns between weekdays and weekends?
day <- weekdays(as.Date(activitydata$date))
daylevel <- vector()
for (i in 1:nrow(activitydata)) {
  if (day[i] == "Saturday") {
    daylevel[i] <- "Weekend"
  } else if (day[i] == "Sunday") {
    daylevel[i] <- "Weekend"
  } else {
    daylevel[i] <- "Weekday"
  }
}
activitydata$daylevel <- daylevel
activitydata$daylevel <- factor(activitydata$daylevel)
stepsByDay <- aggregate(steps ~ interval + daylevel, data = activitydata, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
library(lattice)
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
