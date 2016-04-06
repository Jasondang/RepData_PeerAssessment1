library(ggplot2)
library(scales)
library(lattice)

if (!file.exists("Activity_Dataset.zip")){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, destfile = "Activity_Dataset.zip", method="curl")
}

unzip(zipfile="Activity_Dataset.zip")

steps_data <- read.csv("activity.csv")

#Default of aggregate function is to ignore NA values
steps_day <- aggregate(steps~date, steps_data,FUN=sum)
steps_day$date <- as.Date(as.character(steps_day$date), "%Y-%m-%d")

ggplot(steps_day, aes(x=date, y=steps)) + geom_bar(stat="identity", fill="navy") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")

steps_day_mean <- mean(steps_day$steps)

steps_day_med <- median(steps_day$steps)

steps_int <- aggregate(steps~interval, steps_data,FUN=mean)

ggplot(steps_int, aes(x=interval, y=steps)) + geom_line(color="navy") + labs(title = "Total Number of Steps Taken Each Interval", x = "Interval", y = "Total number of steps")

steps_int[which.max(steps_int$steps),]


sum(is.na(steps_data))

steps_data2 <- steps_data
for (i in 1:nrow(steps_data2)) {
  if (is.na(steps_data2$steps[i])) {
    steps_data2$steps[i] <- steps_int[which(steps_data2$interval[i] == steps_int$interval), ]$steps
  }
}

steps_day2 <- aggregate(steps~date, steps_data2,FUN=sum)

steps_day2$date <- as.Date(as.character(steps_day2$date), "%Y-%m-%d")

ggplot(steps_day2, aes(x=date, y=steps)) + geom_bar(stat="identity", fill="navy") + labs(title = "Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")

mean(steps_day2$steps)

median(steps_day2$steps)

steps_data2$date <- as.Date(as.character(steps_data2$date), "%Y-%m-%d")

steps_data3 <- steps_data2
steps_data3$day <- c("")

for (i in 1:nrow(steps_data3)) {
  if (weekdays(steps_data3$date[i])=="Saturday" | weekdays(steps_data3$date[i])=="Sunday") {
    steps_data3$day[i] <- "Weekend"
  }
  else {
    steps_data3$day[i] <- "Weekday"
  }
}

final_data <- aggregate(steps~interval+day,steps_data3, FUN=mean)

xyplot(final_data$steps ~ final_data$interval | final_data$day, layout = c(1, 2), type = "l", col="navy",
       xlab = "Interval", ylab = "Number of steps")
  