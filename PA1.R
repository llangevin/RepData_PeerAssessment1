#Assignment
setwd("C:/Users/Luc/Documents/coursera/Reproducible Research/RepData_PeerAssessment1")

#Loading and preprocessing the data
unzip("activity.zip")
activity <- read.csv("./activity.csv")


#What is mean total number of steps taken per day?
#For this part of the assignment, you can ignore the missing values in the dataset.
library(dplyr)
sum_steps<-activity %>% filter(!is.na(steps)) %>%group_by(date) %>% summarise(sum_steps = sum(steps))
hist(sum_steps$sum_steps, main='Total number of steps taken each day',xlab='Number of steps',breaks=10)
mean(sum_steps$sum_steps)
median(sum_steps$sum_steps)

#What is the average daily activity pattern?
intervals<-activity %>% filter(!is.na(steps)) %>%group_by(interval) %>% summarise(mean_steps = mean(steps))
plot(intervals$interval,intervals$mean_steps,type = "l",main='Average number of steps taken vs 5-minute interval',
     xlab='5-minute interval',ylab='average number of steps')
intervals[intervals$mean_steps==max(intervals$mean_steps),]

#Imputing missing values
summary(activity$steps)
mean_steps<-activity %>% filter(!is.na(steps)) %>%group_by(interval) %>% summarise(mean_int_steps = round(mean(steps)))
activity2<-left_join(activity, mean_steps, by = "interval")
activity2$steps_imputed<-ifelse(is.na(activity2$steps), activity2$mean_int_steps, activity2$steps)
sum_steps_imputed<-activity2 %>%group_by(date) %>% summarise(sum_steps_imputed = sum(steps_imputed))
hist(sum_steps_imputed$sum_steps_imputed, main='Total number of imputed steps taken each day',xlab='Number of steps',breaks=10)
mean(sum_steps_imputed$sum_steps_imputed)
median(sum_steps_imputed$sum_steps_imputed)

#Are there differences in activity patterns between weekdays and weekends?
activity$date <- as.POSIXct(activity$date)
Sys.setlocale("LC_TIME", "English")
activity2$day<-ifelse(weekdays(activity$date)=='Saturday' | weekdays(activity$date)=='Sunday', 'weekend', 'weekday')
intervals_day<-activity2 %>%group_by(day,interval) %>% summarise(mean_steps_imputed = mean(steps_imputed))
library(ggplot2)
ggplot(intervals_day, aes(x = interval, y = mean_steps_imputed)) + geom_line() +
  xlab("interval") + ylab("average imputed steps") + facet_grid(day ~ .) +
  ggtitle("Average number of steps taken, weekday vs weekend")
  facet_grid(day ~ .)