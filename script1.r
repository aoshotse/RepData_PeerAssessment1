## Install and load ggplot2 WARNING: Skip install if package is already installed!
## Before running this code, ensure that the data file "activity.csv"
## is already IN YOUR WORKING DIRECTORY

install.packages("ggplot2")
library(ggplot2)



########################################
## Loading and preprocessing the data ##
########################################

activity <- read.csv("activity.csv")



#######################################################
## What is mean total number of steps taken per day? ##
#######################################################

## Aggregate steps taken by date:
sumsteps <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
colnames(sumsteps) <- c("date", "Sum.Steps")

## Histogram of steps
attach(sumsteps)
hist(Sum.Steps)

## Mean and median of steps (although evident from histogram):
mean(Sum.Steps)
median(Sum.Steps)



#################################################
## What is the average daily activity pattern? ##
#################################################

## Average of steps plotted against time interval:
avsteps <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
colnames(avsteps) <- c("interval", "Av.Steps")
attach(avsteps)
plot(interval, Av.Steps, type="l")

## Interval with max number of steps:
k <- which.max(Av.Steps)
interval[k]



#############################
## Imputing missing values ##
#############################

## Number of na's in data set:
sum.na <- sum(is.na(activity$steps))
sum.na

## Input missing step values as mean for intervals (as "stepfull"):
cnt <- length(activity$steps)
for (i in 1:cnt) {
	if (is.na(activity$steps[i])>0) {
	     g <- avsteps$Av.Steps[avsteps$interval==activity$interval[i]]
		 activity$stepfull[i] <- g
	}
	else {
	     activity$stepfull[i] <- activity$steps[i]
	}

}

## Histogram total steps (missing values filled as "stepfull")
sumstepfull <- aggregate(activity$stepfull, by=list(activity$date), FUN=sum, na.rm=TRUE)
colnames(sumstepfull) <- c("date", "Sum.Stepfull")
## Histogram of stepfull
attach(sumstepfull)
hist(Sum.Stepfull)
## Mean and median of stepfull:
mean(Sum.Stepfull)
median(Sum.Stepfull)



###############################################################################
## Are there differences in activity patterns between weekdays and weekends? ##
###############################################################################

## Create column of factor variables (2 levels) : "weekend" and "weekday"
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
wkds <- weekdays(activity$date) ###
activity <- cbind(wkds, activity)
cnt1 <- length(activity$wkds)
for (i in 1:cnt1) {
	if ((activity$wkds[i] == "Saturday" | activity$wkds[i] == "Sunday")>0) {
		 activity$FactDay[i] <- "weekend"
	}
	else {
	     activity$FactDay[i] <- "weekday"
	}

}
activity$FactDay <- as.factor(activity$FactDay) 

## Aggregate weekend data by average step/interval and ascribe to data.frame "avwkend"
activitywkend <- subset(activity, FactDay=="weekend")
avwkend <- aggregate(activitywkend$stepfull, by=list(activitywkend$interval), FUN=mean, na.rm=TRUE)
colnames(avwkend) <- c("interval", "Av.Steps")
ll <- length(avwkend$interval)
avwkend$FactDay <- rep("Weekend",ll)

## Aggregate weekday data by average step/interval and ascribe to data.frame "avwkday"
activitywkday <- subset(activity, FactDay=="weekday")
avwkday <- aggregate(activitywkday$stepfull, by=list(activitywkday$interval), FUN=mean, na.rm=TRUE)
colnames(avwkday) <- c("interval", "Av.Steps")
ll <- length(avwkday$interval)
avwkday$FactDay <- rep("Weekday",ll)

## Combine avwkend and avwkday
actvtagg <- rbind(avwkend, avwkday)

## Plot of 5-minute interval (x-axis) vs. avg. steps taken: weekday/weekend(y-axis)
qplot(interval, Av.Steps, data = actvtagg, facets = FactDay~., type = "l", ylab ="Number of steps", geom = "line")