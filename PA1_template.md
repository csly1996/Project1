---
title: "PA1_template"
author: "Siliangyu Cheng"
date: "June 16, 2016"
output: html_document
---
##Loading and preprocessing the data

Show any code that is needed to

Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis

##What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
Calculate and report the mean and median of the total number of steps taken per day

```r
# Load the data 
activity <- read.csv("C:/Users/apple/Desktop/activity.csv", as.is = TRUE)

# Remove the NA values and store in a separate structure for future use
act_na <- activity[complete.cases(activity), ]

# Calculate the total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, act_na , sum)
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
# Create a histogram of no of steps per day
hist(steps_per_day$steps, main = "Histogram of Daily Steps", xlab = "Steps per day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
# mean & median
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# Mean of total number of steps per day is 10766.19, median is 10765.

# Calculate average steps per interval for all days 
avg_steps_per_interval <- aggregate(steps ~ interval, act_na , mean)

# Plot the time series with appropriate labels and heading
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type='l', 
     main="Average number of steps by Interval ", xlab="Time Interval", 
     ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
# Identify the interval index which has the highest average steps
max_steps_row <- which.max(avg_steps_per_interval$steps)
avg_steps_per_interval [max_steps_row, ]
```

```
##     interval    steps
## 104      835 206.1698
```

```r
# The interval 835 has the maximum average value of steps (206.1698).


# Calculate the number of rows with missing values
sum(is.na(activity))
```

```
## [1] 2304
```
##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# Loop thru all the rows of activity
for (i in 1:nrow(activity)) {
    if(is.na(activity$steps[i])) {
        val <- avg_steps_per_interval$steps[which(avg_steps_per_interval$interval == activity$interval[i])]
        activity$steps[i] <- val 
    }
}

# Aggregate the steps per day with the imputed values
steps_per_day_impute <- aggregate(steps ~ date, activity, sum)

# Draw a histogram of the value 
hist(steps_per_day_impute$steps, main="Histogram of total number of steps per day (imputed)",xlab="steps per day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png)

```r
# Compute the mean and median of the imputed value
# Calculate the mean and median of the total number of steps taken per day
mean(steps_per_day_impute$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day_impute$steps)
```

```
## [1] 10766.19
```

```r
mean(steps_per_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps)
```

```
## [1] 10765
```

```r
# mean and the median doesn't change 
```

##Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r

library(ggplot2)

#creat a function
week_day <- function(date_val) {
    wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
    } else {
        x <- 'Weekend'
    }
    x
}

# Apply the week_day function and add a new column to activity dataset
activity$day_type <- as.factor(sapply(activity$date, week_day))

# Create the aggregated data frame by intervals and day_type
steps_per_day_impute <- aggregate(steps ~ interval+day_type, activity, mean)

# plot
plt <- ggplot(steps_per_day_impute, aes(interval, steps)) +
    geom_line(stat = "identity", aes(color = day_type)) +
    facet_grid(day_type ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y="Number of steps") +
    theme_bw() +
    ggtitle("Number of steps per interval by day type")

print(plt)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png)

```r
#library(markdown)
#library(knitr)

#transform the .Rmd to a markdown (.md) file.
#knit('C:/Users/apple/Desktop/PA1_template.Rmd')
```
