##check for csv file
if(!file.exists("activity.csv"))
{
    ##extract file from zip.  suppress warnings for files we dont need to overwrite
    suppressWarnings(unzip("activity.zip", overwrite = FALSE))
}

activities<-read.csv("activity.csv")

##tranform date field into date data type
activities$date <- as.Date(activities$date, format="%Y-%m-%d")

#how many steps per day
omittedData<-na.omit(activities)
stepsPerDay<-aggregate(omittedData$steps, list(Date=omittedData$date), sum, na.rm=TRUE)
hist(stepsPerDay$x, xlab="Number of Steps", main="Histogram of Steps per Day", breaks=length(stepsPerDay$Date))
meanStepsPerDay<-mean(stepsPerDay$x, na.rm=TRUE)
medianStepsPerDay<-median(stepsPerDay$x, na.rm=TRUE)


avgStepsPerInterval<-tapply(activities$steps, activities$interval, mean, na.rm=TRUE)
plot(x=dimnames(avgStepsPerInterval)[[1]], avgStepsPerInterval, type="l", xlab="Interval", ylab="Average steps", main="Average Steps per Interval")
maxAvgIndex<-which.max(avgStepsPerInterval)
maxAvgValue<-avgStepsPerInterval[maxAvgIndex]

library("Hmisc")
sum(is.na(activities$steps))
meanForInterval<-function(interval) {
    return (avgStepsPerInterval[interval][[1]])
}
imputedSteps<-activities
for (i in 1:nrow(imputedSteps)) {
    if (is.na(imputedSteps$steps[i])) {
        imputedSteps$steps[i] <- meanForInterval(as.character(imputedSteps$interval[i]))
    }
}
nonOmittedStepsPerDay<-aggregate(imputedSteps$steps, list(Date=imputedSteps$date), sum)
hist(nonOmittedStepsPerDay$x, xlab="Number of Steps", main="Histogram of Steps per Day (with Filled in Data)", breaks=length(nonOmittedStepsPerDay$Date))
nonOmittedMeanStepsPerDay<-mean(nonOmittedStepsPerDay$x)
nonOmittedMedianStepsPerDay<-median(nonOmittedStepsPerDay$x)

imputedSteps$weekdays<-factor(weekdays(imputedSteps$date))
levels(imputedSteps$weekdays)<-list(weekday=c("Monday", "Tuesday","Wednesday","Thursday", "Friday"), weekend=c("Saturday", "Sunday"))
weekdayAvgSteps<-aggregate(imputedSteps$steps, list(Interval=imputedSteps$interval, WeekDay=imputedSteps$weekdays), mean)
library(lattice)
weekdayPlot<-xyplot(weekdayAvgSteps$x~weekdayAvgSteps$Interval|weekdayAvgSteps$WeekDay, type="l", xlab="Interval", ylab="Average steps", layout=c(1,2))
print(weekdayPlot)


