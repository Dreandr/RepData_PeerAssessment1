Reproducible Research: Course project 1 (Coursera Data Science Spec.)
=====================================================================

This document was created as part of a course project with the
Reproducible Research course at Coursera.com. In this project, the
student is expected to study a dataset covering the activities of a
volunteer tracking their physical activity, answer a range of questions
regarding the dataset and provide an overview of the activity pattern it
suggests.

Full description of the project and the tasks it involves is available
in the forked repository.

Step 0: Download the Data
-------------------------

As a first step in our analysis, we download the dataset, unzip it and
read it into R.

    zipUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    zipFile <- "repdata_data_activity.zip"

    if (!file.exists("activity.csv")) {
      download.file(zipUrl, zipFile)
      unzip(zipFile)
    }

    rm(zipFile, zipUrl)

    data <- read.csv("activity.csv")

Step 1: Total number of steps per day, mean and median
------------------------------------------------------

Now that the data have been downloaded and read, we can start to dig in
for some actual analysis.

First, we will take a look at the total number of steps per day,
plotting it as a histogram and, for now, ignoring any NAs the data
happen to have.

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    dataGr <- group_by(data, date) 
    sumSteps <- summarize(dataGr, steps = sum(steps, na.rm = T))
    hist(sumSteps$steps, main = "Histogram: Total number of steps per day", xlab = "Total number of steps per day", col = "red")

![](PA1_template_files/figure-markdown_strict/steps%20total%20hist-1.png)

As we can see, the subject would most often make between 10000 and 15000
steps per day.

Now, let us take a look at the mean and median values for the steps per
day total:

    mean(sumSteps$steps)

    ## [1] 9354.23

    median(sumSteps$steps)

    ## [1] 10395

Step 2: Daily Activity Pattern
------------------------------

In this section, we will take a look at the subject's daily activity
pattern. However, as per the course project's description, first, we
have to impute the data. Here, I desided to base the imputation on the
mean for the target time interval.

### Step 2.1: Imputing the Data

Before imputing the data, let us take a look at how many NAs we are
dealing with, and what percent of the evaluation is missing.

    sum(is.na(data$steps))

    ## [1] 2304

    (sum(is.na(data$steps))/length(data$steps))*100 

    ## [1] 13.11475

Now, we will create a new dataset, identical to the first one, and
replace the missing values for the steps variable in line with the means
for their respective intervals, rounding them downwards.

    data2 <- data
    means <- numeric()
    for (i in 1:sum(is.na(data2$steps))) {
      means <- c(means, floor(mean(data$steps[data$interval == data2$interval[is.na(data2$steps)][i]], na.rm = T)))
    }
    data2$steps[is.na(data2$steps)] <- means

### Step 2.2: Comparing the imputed dataset against the original one

With a new dataset in our hands, we will now see how it is different
from the one we started with. First, let us take a look at the new
histogram for total number of steps per day:

    dataGr2 <- group_by(data2, date) 
    sumSteps2 <- summarize(dataGr2, steps = sum(steps, na.rm = T))
    hist(sumSteps2$steps, main = "Histogram: Total number of steps per day (imputed dataset)", xlab = "Total number of steps per day", col = "yellow")

![](PA1_template_files/figure-markdown_strict/new%20hist-1.png)

As we can see, even more data is now clustered in the centre. We can
also take a look at the new mean and median for the set:

    mean(sumSteps2$steps)

    ## [1] 10749.77

    median(sumSteps2$steps)

    ## [1] 10641

We can note that both the mean and the median have moved up.

### Step 2.3: Plotting the Daily Activity Pattern

Now we can use our new dataset to calculate and plot the daily activity
pattern of the subject and identiify the interval of peak activity:

    data2Gr <- group_by(data2, interval)
    avgSum <- summarize(data2Gr, steps = mean(steps))
    plot(avgSum$interval, avgSum$steps, type = "l", lwd = 1.5, col = "red", main = "Daily activity pattern", xlab = "Time (in 5-minute intervals)", ylab = "Average total of steps")

![](PA1_template_files/figure-markdown_strict/daily%20act-1.png)

    avgSum$interval[which.max(avgSum$steps)]

    ## [1] 835

Step 3: Weekend vs. Weekday Activity Pattern
--------------------------------------------

Finally, let's see if there's a difference in the activity patterns
dispplayed by the subject on weekends and weekdays. First, let's add a
new Weekday factor variable to our imputed dataset:

    wdays <- weekdays(as.Date(data2$date, "%Y-%m-%d"))
    wdays[wdays == "суббота" | wdays == "воскресенье"] <- "weekend"
    wdays[wdays != "weekend"] <- "weekday"
    data2 <- mutate(data2, weekday = as.factor(wdays))
    rm(wdays)

Now, let's calculate the patterns for weekends and weekdays and plot
them:

    data2Gr <- group_by(data2, weekday, interval)
    wkdaySum <- summarize(data2Gr, steps = mean(steps))
    rm(data2Gr)

    library(ggplot2)
    ggplot(wkdaySum, aes(interval, steps, color = weekday)) + facet_grid(weekday ~ .) + geom_line() + labs(title = "Average activity pattern, weekday vs. weekend", x = "Time (in 5-minute intervals)", y = "Average total of steps", color = "Legend")

![](PA1_template_files/figure-markdown_strict/plot%20wdays-1.png)

    rm(data2, wkdaySum)

As we can see, while the weekday pattern is quite consistent with the
one we plotted on the previous step, the weekend pattern reveals a
significantly more even distridution of activity.
