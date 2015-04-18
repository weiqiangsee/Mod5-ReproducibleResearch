#uploading the dataset into R
```{r}
activity <- read.csv(file="C:/Users/User/Documents/GitHub/Mod5-ReproducibleResearch/Project1/activity.csv")
```

#format date field
```{r}
activity$date <- as.Date(activity$date)
``` 
#Load "reshape2" package 
```{r}
library(reshape2)
``` 

##Mean total number of steps taken per day

#plot histogram
```{r}
meltActivity <- melt(activity, id=c("date"), na.rm=TRUE, measure.vars="steps")
castActivity <- dcast(meltActivity, date ~ variable, sum)
hist(castActivity$steps)
```


#get mean and median
```{r}
actMean <- format(round(mean(castActivity$steps), 2), nsmall = 2)
actMedian <- median(castActivity$steps)
actMean
actMedian
```

##Average daily activity pattern

#plot graph
```{r}
ADAmeltInterval <- melt(activity, id=c("interval"), na.rm=TRUE, measure.vars="steps")
ADAcastInterval <- dcast(ADAmeltInterval, interval ~ variable, mean)
plot( ADAcastInterval$interval, ADAcastInterval$steps, type="l")
```


#finding which interval has the maximum no. of steps and how much
```{r}
maxRow <- ADAcastInterval[ADAcastInterval$steps==max(ADAcastInterval$steps),]
maxRow
```

##Inputing missing values

#calculate & report total no. of missing values
```{r}
sum(is.na(activity$steps))
```

#plotting histogram
```{r}
activityNa <- is.na(activity$steps)
MVcastIntervalAdj <- cbind(ADAcastInterval, as.integer(round(ADAcastInterval$steps)))
nonNaActivity <- activity[!activityNa,]
NaActivity <- activity[activityNa,]
NaResolved <- merge(NaActivity, MVcastIntervalAdj, by.x = "interval", by.y = "interval", all=FALSE )
NaResolved$steps.x <- NULL
NaResolved$steps.y <- NULL
names(NaResolved)[3] <- paste("steps")
NaResolvedActivity <- rbind(NaResolved, nonNaActivity)
MVmeltActivity <- melt(NaResolvedActivity, id=c("date"), na.rm=TRUE, measure.vars="steps")
MVcastActivity <- dcast(MVmeltActivity, date ~ variable, sum)
hist(MVcastActivity$steps)
```


#get mean and median
```{r}
MVMean <- format(round(mean(MVcastActivity$steps), 2), nsmall = 2)
MVMedian <- median(MVcastActivity$steps)
MVMean
MVMedian
```

##Difference in activity pattern Weekday VS weekends
```{r}
wd <- !(weekdays(as.Date(NaResolvedActivity$date)) %in% c('Saturday','Sunday'))
wdwe <- c("", "")
for (i in 1:length(wd)) {
  if (wd[i]) {wdwe[i] <- "Weekday"} else {wdwe[i] <- "Weekend"}
}
NaResolvedActivity[, "dayType"] <- factor(wdwe)
library(ggplot2)
diff <- ggplot(NaResolvedActivity, aes(x=interval, y=steps)) + geom_line()
melt <- melt(NaResolvedActivity, id=c("interval", "dayType"), na.rm=TRUE, measure.vars="steps")
cast <- dcast(melt, interval + dayType ~ variable, mean)
diff <- ggplot(cast, aes(x=interval, y=steps)) + geom_line() + ylab("Number of Steps")
diff + facet_wrap(~ dayType, ncol=1)
```

