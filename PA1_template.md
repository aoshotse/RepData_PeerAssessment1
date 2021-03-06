---
#### title: "PA Project 1"
###### author: "aoshotse"
###### date: "Thursday, August 14, 2014"
###### output: html_document
__________________________________________________________________________________
  <br />
  
#### Peer Assesment Project 1:
Installing/loading packages:


```r
## Install and load ggplot2 WARNING: Skip install if package is already installed!
## Before running this code, ensure that the data file "activity.csv"
## is already IN YOUR WORKING DIRECTORY
## install.packages("ggplot2")
 library(lattice)
```
  <br />
  <br />
  
#### Loading and preprocessing the data

Show any code that is needed to 

1.) Load the data (i.e. read.csv())

2.) Process/transform the data (if necessary) into a format suitable for your analysis


```r
########################################
## Loading and preprocessing the data ##
########################################

activity <- read.csv("activity.csv")
```
  <br />
  <br />
  
#### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1.) Make a histogram of the total number of steps taken each day

2.) Calculate and report the mean and median total number of steps taken per day


```r
#######################################################
## What is mean total number of steps taken per day? ##
#######################################################

## Aggregate steps taken by date:
sumsteps <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
colnames(sumsteps) <- c("date", "Sum.Steps")

## Histogram of steps
attach(sumsteps)
hist(Sum.Steps)
```

![plot of chunk Histogram and Mean/Median Steps](figure/Histogram and Mean/Median Steps.png) 

```r
## Mean and median of steps (although evident from histogram):
mean(Sum.Steps)
```

```
## [1] 9354
```

```r
median(Sum.Steps)
```

```
## [1] 10395
```
  <br />
  <br />
  
#### What is the average daily activity pattern?

1.) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2.) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#################################################
## What is the average daily activity pattern? ##
#################################################

## Average of steps plotted against time interval:
avsteps <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
colnames(avsteps) <- c("interval", "Av.Steps")
attach(avsteps)
plot(interval, Av.Steps, type="l")
```

![plot of chunk Average Daily Activity Pattern](figure/Average Daily Activity Pattern.png) 

```r
## Interval with max number of steps:
k <- which.max(Av.Steps)
interval[k]
```

```
## [1] 835
```
  <br />
  <br />
  
#### Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2.) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3.) Create a new dataset that is equal to the original dataset but with the missing data filled in.

4.) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#############################
## Imputing missing values ##
#############################

## Number of na's in data set:
sum.na <- sum(is.na(activity$steps))
sum.na
```

```
## [1] 2304
```

```r
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
```

```
## The following object is masked from sumsteps:
## 
##     date
```

```r
hist(Sum.Stepfull)
```

![plot of chunk Inputing Missing Values](figure/Inputing Missing Values.png) 

```r
## Mean and median of stepfull:
mean(Sum.Stepfull)
```

```
## [1] 10766
```

```r
median(Sum.Stepfull)
```

```
## [1] 10766
```
Removing the missing values causes the mean value to dramatically rise and causes the mean and median to become equal.
  <br />
  <br />
  <br />
  
#### Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.) Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2.) Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
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
xyplot(Av.Steps ~ interval | FactDay, data = actvtagg, type = "l", ylab ="Number of steps", layout=c(1,2))
```

![plot of chunk Differences in Activity Patterns Weekdays vs. Weekends](figure/Differences in Activity Patterns Weekdays vs. Weekends.png) 

