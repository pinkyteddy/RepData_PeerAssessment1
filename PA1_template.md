#Reproducible Research - Personal activity monitoring


##Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Data
The data for this assignment can be downloaded from the course web site: <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

The variables included in this dataset are:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* date: The date on which the measurement was taken in YYYY-MM-DD format
* interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##Loading and preprocessing the data

####1. Load the data


```r
# Check and unzip if file doesn't exist
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
# Load data
activityData <- read.csv('activity.csv')

# print out first 10 rows
head(activityData,10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

####2. Process/transform the data (if necessary) 

```r
# load all packages used in this exploratory analysis
library('dplyr')
```
##What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

```r
myData <- activityData[ with (activityData, { !(is.na(steps)) } ), ]

# print out first 10 rows
head(myData)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```
####1. Calculate the total number of steps taken per day

```r
#Calculate total per day
day_data <- group_by(myData, date)
steps_per_day <- summarise(day_data, total = sum(steps))

#Display data
steps_per_day
```

```
## Source: local data frame [53 x 2]
## 
##          date total
##        (fctr) (int)
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```


####2. Make a histogram of the total number of steps taken each day


```r
hist(steps_per_day$total, main="Histogram of the total number of steps taken each day", 
     xlab="Total number of steps each day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

####3. Calculate and report the mean and median of the total number of steps taken per day

```r
summary(steps_per_day)
```

```
##          date        total      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

* Mean of total number of steps taken day is **10766**
* Median of total number of steps taken day is **10765**

##What is the average daily activity pattern?

####1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
# preprocessing data
plot_data <- tapply(myData$steps, myData$interval, mean, na.rm = TRUE)

# create plot 
plot(row.names(plot_data), plot_data, type = "l", xlab = "5-min interval", 
    ylab = "Average across all Days", main = "Average number of steps taken", 
    col = "blue")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

####2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval <- which.max(plot_data)

names(max_interval)
```

```
## [1] "835"
```

##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
data_NA <- sum(is.na(activityData))

data_NA
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset.
The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
stepAverage <- aggregate(steps ~ interval, data = activityData, FUN = mean)
fillNAData <- numeric()
for (i in 1:nrow(activityData)) {
    temp <- activityData[i, ]
    if (is.na(temp$steps)) {
        steps <- subset(stepAverage, interval == temp$interval)$steps
    } else {
        steps <- temp$steps
    }
    fillNAData <- c(fillNAData, steps)
}
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
new_data <- activityData
new_data$steps <- fillNAData
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#Calcuate new data

myNewTotal <- aggregate(steps ~ date, data = new_data, sum, na.rm = TRUE)
#Draw new histogram
hist(myNewTotal$steps, main = "Total steps by day", xlab = "day", col = "red")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 




