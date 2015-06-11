library(dplyr)
pamd <- read.csv("activity.csv")

##clear environment except NEI and SCC dataframes
rm(list=setdiff(ls(), c("pamd")))

##total steps per day, including NA
stepsDay <- as.data.frame(pamd %>% group_by(date) %>% summarise(sum(steps)))
dayMedian <- apply(stepsDay,2, FUN = median, na.rm = TRUE)
colnames(stepsDay)[2] <- "totalSteps" ##rename column
dayMedian <- as.numeric(dayMedian)
dayMean <- mean(stepsDay$totalSteps, na.rm = TRUE)
x11()
hist(stepsDay$totalSteps,breaks=8,xlab="Total Steps per Day")
abline(v=dayMedian,col=3,lwd=3) ## green line at the median
abline(v=dayMean,col=4,lty=2)##blue line at the mean
dev.off()

pamd2 <- pamd[complete.cases(pamd),]
stepsInterval <- as.data.frame(pamd2 %>% group_by(interval) %>% summarise(total = sum(steps)/61))

highestAvg <- max(stepsInterval$total, na.rm=TRUE) #get highest avg
tmp = subset(stepsInterval, total == highestAvg)

x11()
plot(x=stepsInterval$interval,y=stepsInterval$total,type='l')
points(tmp$interval,tmp$total,type="p")
abline(v=tmp$interval,col=4,lty=2)

und <- sum(is.na(pamd$steps)) #total number of na

##populate all NA values with the mean for that interval
alldata <- full_join(pamd,stepsInterval, by="interval")
alldata$steps[is.na(alldata$steps)] <- alldata$total[is.na(alldata$steps)]

stepsDay2 <- as.data.frame(alldata %>% group_by(date) %>% summarise(sum(steps)))
dayMedian2 <- apply(stepsDay2,2, FUN = median, na.rm = TRUE)
dayMedian2 <- as.numeric(dayMedian2)
colnames(stepsDay2)[2] <- "totalSteps" ##rename column
stepsDay2$totalSteps <- as.integer(stepsDay2$totalSteps)
dayMean2 <- mean(stepsDay2$totalSteps)

x11()
hist(stepsDay2$totalSteps,breaks=8,xlab="Total Steps per Day")
abline(v=dayMedian2,col=3,lwd=3) ## green line at the median
abline(v=dayMean2,col=2,lty=2)

alldata$date <- as.character(alldata$date)
alldata$date <- as.Date(alldata$date)

weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
alldata$wDay <-  factor((weekdays(alldata$date) %in% weekdays1)+1L,
                    levels=1:2, labels=c('weekend', 'weekday'))
weekdays <- subset(alldata, wDay=="weekday")
weekends <- subset(alldata, wDay=="weekend")

numdays <- length(unique(weekdays$date))
dayavg <- as.data.frame(weekdays %>% group_by(interval) %>% summarise(total = sum(steps)/numdays))
numdays <- length(unique(weekends$date))
endavg <- as.data.frame(weekends %>% group_by(interval) %>% summarise(total = sum(steps)/numdays))
x11()
par(mfrow=c(2,1)) #lays out a grid for the charts, 2 rows and 2 columns
#upper left chart
plot(dayavg$interval,dayavg$total,type="l")
#upper right chart
plot(endavg$interval,endavg$total,type="l")
