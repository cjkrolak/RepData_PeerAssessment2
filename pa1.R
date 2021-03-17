propDamageFiltered <- function(val, exp) {
    returnVal <- val  # default
    if (exp=="K") {
        returnVal <- val * 1000
    }
    else if (exp =="M" | exp =="m") {
        returnVal <- val * 1000000
    }
    else if (exp == "B") {
        returnVal <- val * 1000000000
    }
    returnVal
}
# 1. Code for reading in the dataset and/or processing the data
library(utils)
# read in data
# 10. Does the analysis start from the raw data file (i.e. the original .csv.bz2 file)?
print("reading in raw data...")
#df <- read.csv(".\\repdata_data_StormData.csv.bz2", na.strings = NA)
print("done reading in raw data.")

# transform data
df$InjuriesPlusFatalities <- df$FATALITIES + df$INJURIES
df$logInjuriesPlusFatalities <- log(df$InjuriesPlusFatalities + 1)
df$logFatalities <- log(df$FATALITIES + 1)
df$logInjuries <- log(df$INJURIES + 1)
df$propDamageFiltered <- propDamageFiltered(df$PROPDMG, df$PROPDMGEXP)
df$logPropDamageFiltered <- log(df$propDamageFiltered)

# transform date field and add field for weekDay
# df$date <- strptime(df$date,"%Y-%m-%d")
# df$weekDay <- weekdays(df$date)
# df$group <- factor(ifelse(df$weekDay %in% c("Saturday", "Sunday"),
#                           "weekend", "weekday"))
# summary(df)

# aggregate data into summary tables
print("building summary tables...")
totalFatalities <- aggregate(df$FATALITIES, list(factor(df$EVTYPE)), sum)
names(totalFatalities) <- c("EVTYPE", "FATALITIES")
totalFatalities <- totalFatalities[totalFatalities$FATALITIES >0,]
totalFatalities$logFatalities <- log(totalFatalities$FATALITIES + 1)

totalInjuries <- aggregate(df$INJURIES, list(factor(df$EVTYPE)), sum)
names(totalInjuries) <- c("EVTYPE", "INJURIES")
totalInjuries <- totalInjuries[totalInjuries$INJURIES>0,]
totalInjuries$logInjuries <- log(totalInjuries$INJURIES + 1)

totalInjuriesPlusFatalities <- aggregate(df$InjuriesPlusFatalities, list(factor(df$EVTYPE)), sum)
names(totalInjuriesPlusFatalities) <- c("EVTYPE", "InjuriesPlusFatalities")
totalInjuriesPlusFatalities <- totalInjuriesPlusFatalities[totalInjuriesPlusFatalities$InjuriesPlusFatalities>0,]
totalInjuriesPlusFatalities$logInjuriesPlusFatalities <- log(totalInjuriesPlusFatalities$InjuriesPlusFatalities + 1)

totalPropertyDamage <- aggregate(df$propDamageFiltered,
                                 list(factor(df$EVTYPE)), sum)
names(totalPropertyDamage) <- c("EVTYPE", "totalPropDamage")
totalPropertyDamage <- totalPropertyDamage[totalPropertyDamage$totalPropDamage>0,]
totalPropertyDamage$logTotalPropDamage <- log(totalPropertyDamage$totalPropDamage + 1)
print("done building summary tables.")

# 11. Does the analysis address the question of which types of events are most
#     harmful to population health?
par(mfrow=c(1,2), mar=c(4,4,2,1))
#hist(df$FATALITIES)


# 12. Does the analysis address the question of which types of events have the
#     greatest economic consequences?
#hist(df$INJURIES)

#ggplot(df, aes(x = "propertyDamageFiltered", propertyDamageFiltered)) + geom_boxplot(fill = "royalblue") + 
#       labs(x = "EVTYPE", y = "property damage")

# pareto of total fatalities by EVTYPE
par(mfrow=c(1,1), mar=c(4,10,2,1), cex.axis=0.6, las=2)
paretoFatalities <- totalFatalities[totalFatalities$FATALITIES>200,]
pareto.chart(paretoFatalities$FATALITIES, ylim=c(0,6000),
             xlab="EVTYPE", ylab="total fatalities",
             main="total fatalities by event type",
             names.arg=paretoFatalities$EVTYPE)
readline(prompt="pareto chart of fatalities, Press [enter] to continue")

# pareto of total injuries by EVTYPE
par(mfrow=c(1,1), mar=c(4,10,2,1), cex.axis=0.6, las=2)
paretoInjuries <- totalInjuries[totalInjuries$INJURIES>1000,]
pareto.chart(paretoInjuries$INJURIES, ylim=c(0,100000),
             xlab="EVTYPE", ylab="total injuries",
             main="total injuries by event type",
             names.arg=paretoInjuries$EVTYPE)
readline(prompt="pareto chart of injuries, Press [enter] to continue")

# pareto of total fatalities+injuries by EVTYPE
par(mfrow=c(1,1), mar=c(4,10,2,1), cex.axis=0.6, las=2)
paretoTotal <- totalInjuriesPlusFatalities[totalInjuriesPlusFatalities$InjuriesPlusFatalities>1000,]
pareto.chart(paretoTotal$InjuriesPlusFatalities, ylim=c(0,100000),
             xlab="EVTYPE", ylab="total injuries+fatalities",
             main="total injuries+fatalities by event type",
             names.arg=paretoTotal$EVTYPE)
readline(prompt="pareto chart of total injuries+fatalities, Press [enter] to continue")

# filter out minor events to compare fatalities + injuries per event
majorEvents <- df[df$InjuriesPlusFatalities>200,]
boxplot(InjuriesPlusFatalities ~ EVTYPE, data=majorEvents,
        horizontal=TRUE, main="Harm caused per event",
        ylab="event type", xlab="total harm (injuries+fatalities)")
readline(prompt="boxplot of fatalities + injuries per event, Press [enter] to continue")

# property damage vs. events
majorPropertyEvents <- df[df$propDamageFiltered>20000,]
boxplot(logPropDamageFiltered ~ EVTYPE, data=majorPropertyEvents,
        horizontal=TRUE, main="property damage caused per event",
        ylab="event type", xlab="log of property damage (dollars)")
readline(prompt="boxplot of log property damage per event, Press [enter] to continue")

#library(ggplot2)
#ggplot(totalPropertyDamage, aes(x = "totalPropDamage", totalPropDamage)) + geom_boxplot(fill = "royalblue") + 
#    labs(x = "EVTYPE", y = "property damage")

# boxplot(logFatalities ~ EVTYPE, df, horizontal=TRUE, main="fatalities by evtype",
#         ylab="event type", xlab="log of fatalities")
# readline(prompt="Press [enter] to continue")
# 
# par(mfrow=c(1,1), mar=c(4,10,2,1), cex.axis=0.6, las=2)
# boxplot(FATALITIES ~ EVTYPE, totalFatalities, horizontal=TRUE, main="fatalities by evtype",
#         ylab="event type", xlab="total fatalities")
# readline(prompt="Press [enter] to continue")
# 
par(mfrow=c(1,1), mar=c(4,10,2,1), cex.axis=0.6, las=2)
#boxplot(logFatalities ~ EVTYPE, totalFatalities, horizontal=TRUE, main="log(fatalities) by evtype",
#        ylab="event type", xlab="log(total fatalities)")
#readline(prompt="plot of log fatalities, Press [enter] to continue")
# 
# par(mfrow=c(1,1), mar=c(4,10,2,1), cex.axis=0.6, las=2)
# boxplot(INJURIES ~ EVTYPE, totalInjuries, horizontal=TRUE, main="injuries by evtype",
#         ylab="event type", xlab="total injuries")
# readline(prompt="Press [enter] to continue")
# 
# par(mfrow=c(1,1), mar=c(4,10,2,1), cex.axis=0.6, las=2)
#boxplot(logInjuries ~ EVTYPE, totalInjuries, horizontal=TRUE, main="log(injuries) by evtype",
#        ylab="event type", xlab="log(total injuries)")
#readline(prompt="plot of log injuries, Press [enter] to continue")
# 
# par(mfrow=c(1,1), mar=c(4,10,2,1), cex.axis=0.6, las=2)
# boxplot(logInjuriesPlusFatalities ~ EVTYPE, df, horizontal=TRUE, main="log(injuries+fatalities) by evtype",
#         ylab="event type", xlab="log(total injuries + fatalities)")
# readline(prompt="Press [enter] to continue")
# 
par(mfrow=c(1,1), mar=c(4,10,2,1), cex.axis=0.6, las=2)
#boxplot(totalPropDamage ~ EVTYPE, totalPropertyDamage, horizontal=TRUE, main="property damage by evtype",
#         ylab="event type", xlab="log property damage")
#readline(prompt="chart of log(prop damage) vs. EVTYPE, Press [enter] to continue")

# # calculate total steps per day
# stepsPerDay <- aggregate(df$steps, list(factor(df$date)), sum)
# names(stepsPerDay) <- c("date", "totalSteps")
# avgStepsPerDay <- aggregate(df$steps, list(factor(df$date)), mean)
# stepsPerDay$avgSteps <- avgStepsPerDay$x
# stepsPerDay[is.na(stepsPerDay$totalSteps),2] <- 0  # replace NA with 0
# stepsPerDay[is.na(stepsPerDay$totalSteps),3] <- 0  # replace NA with 0
# mean(stepsPerDay$totalSteps)
# readline(prompt="Press [enter] to continue")
# 
# # 2. Histogram of the total number of steps taken each day
# par(mfrow=c(1,1))
# hist(stepsPerDay$totalSteps)
# 
# # 3. Mean and median number of steps taken each day
# summary(stepsPerDay$totalSteps)
# readline(prompt="Press [enter] to continue")
# 
# # 4. Time series plot of the average number of steps taken
# intervalData <- data.frame(index= seq.int(unique(df$interval)), interval=unique(df$interval))
# intervalData$meanSteps <- tapply(df$steps, df$interval, mean, na.rm=TRUE)
# intervalData$medianSteps <- tapply(df$steps, df$interval, median, na.rm=TRUE)
# names(intervalData) = c("index", "interval", "meanSteps", "medianSteps")
# par(mfrow=c(1,1))
# plot(intervalData$interval, intervalData$meanSteps, type="l", main="steps per 5-minute interval",
#      xlab="5 minute interval", ylab="average steps")
# readline(prompt="Press [enter] to continue")
# 
# # 5. The 5-minute interval that, on average, contains the maximum number of steps
# intervalData[which.max(intervalData$meanSteps), 2]
# 
# # 6. Code to describe and show a strategy for imputing missing data
# # use mean value for each 5 minute interval to impute missing data
# dfi <- df
# dfi$steps[is.na(dfi$steps)] <- intervalData$meanSteps[match(dfi$interval, intervalData$interval)][which(is.na(dfi$steps))]
# 
# # 7. Histogram of the total number of steps taken each day after missing values are imputed
# hist(dfi$steps)
# summary(dfi$steps)
# readline(prompt="Press [enter] to continue")
# 
# # 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# library(lattice)
# xyplot(steps ~ interval|group, data=dfi, layout=c(1,2), type="l")
# readline(prompt="Press [enter] to continue")
# 
# 
# # 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
# 
# 
# 
# ## Are there differences in activity patterns between weekdays and weekends?