# Reproducible Research: Project Work 1

Created Truong Dang on 6th April 2016, 
Melbourne Australia


#### Setup - Required Packages
The following packages are required to be loaded before commencing. The packages were previously installed

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
library(scales)
library(lattice)
```



#### Loading and preprocessing the data
To save time and processing power, a condition was added to ensure the dataset was not repeatedly downloaded

```r
#Download File if it does not exist in the current directory
if (!file.exists("Activity_Dataset.zip")){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, destfile = "Activity_Dataset.zip", method="curl")
}

#Unzip the file
unzip(zipfile="Activity_Dataset.zip")

#Read the data and assign it to variable
steps_data <- read.csv("activity.csv")
```

#### What is the mean total number of steps taken per day?
To calculate the total number of steps taken per day

```r
#Default of aggregate function is to ignore NA values
steps_day <- aggregate(steps~date, steps_data,FUN=sum)
```

Make a histogram of the total number of steps taken per day

```r
#Convert date from factor to the date format
steps_day$date <- as.Date(as.character(steps_day$date), "%Y-%m-%d")

ggplot(steps_day, aes(x=date, y=steps)) + geom_bar(stat="identity", fill="navy") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Calculate and report the mean and median of the total number of steps taken per day

```r
#To calculate mean
steps_day_mean <- mean(steps_day$steps)

#To calculate median
steps_day_med <- median(steps_day$steps)
```

Results

```r
steps_day_mean
```

```
## [1] 10766.19
```

```r
steps_day_med
```

```
## [1] 10765
```

#### What is the avereage daily activity pattern?
Make a times series plot of the 5 minute interval (x-axis) and the average number of steps taken, averaged across all days(y-axis)

```r
#Aggregate the new required dataset
steps_int <- aggregate(steps~interval, steps_data,FUN=mean)

ggplot(steps_int, aes(x=interval, y=steps)) + geom_line(color="navy") + labs(title = "Total Number of Steps Taken Each Interval", x = "Interval", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

Which 5-minute interval, on average across all days in the dataset, contains the maximum number of steps?

```r
steps_int[which.max(steps_int$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

#### Imputing missing values
Calculate and report the total number of missing values in the dataset

```r
sum(is.na(steps_data))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
steps_data2 <- steps_data
for (i in 1:nrow(steps_data2)) {
  if (is.na(steps_data2$steps[i])) {
    steps_data2$steps[i] <- steps_int[which(steps_data2$interval[i] == steps_int$interval), ]$steps
  }
}
```

Make a histogram of the total number of steps taken each day

```r
steps_day2 <- aggregate(steps~date, steps_data2,FUN=sum)

steps_day2$date <- as.Date(as.character(steps_day2$date), "%Y-%m-%d")

ggplot(steps_day2, aes(x=date, y=steps)) + geom_bar(stat="identity", fill="navy") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

Calculate and report the mean and median total number of steps taken per day. 

```r
#Calculate Mean
new_steps_mean <- mean(steps_day2$steps)

#Calculate Median
new_steps_median <- median(steps_day2$steps)
```


Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_day_mean
```

```
## [1] 10766.19
```

```r
steps_day_med
```

```
## [1] 10765
```

```r
new_steps_mean
```

```
## [1] 10766.19
```

```r
new_steps_median
```

```
## [1] 10766.19
```

While the mean of both the datasets are exactly identical, the median of the new dataset (no missing values) is slightly higher. 

#### Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
steps_data2$date <- as.Date(as.character(steps_data2$date), "%Y-%m-%d")

steps_data3 <- steps_data2
#Add an empty column in the new dataset
steps_data3$day <- c("")

#Add Weekend or Weekday to the new column of the dataset depending on the output of the weekdays function for each row
for (i in 1:nrow(steps_data3)) {
  if (weekdays(steps_data3$date[i])=="Saturday" | weekdays(steps_data3$date[i])=="Sunday") {
    steps_data3$day[i] <- "Weekend"
  }
  else {
    steps_data3$day[i] <- "Weekday"
  }
}
```

Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
final_data <- aggregate(steps~interval+day,steps_data3, FUN=mean)

xyplot(final_data$steps ~ final_data$interval | final_data$day, layout = c(1, 2), type = "l", col="navy",
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

