###Load the Data

1.) Load and preprocess the data
```{r}
activity<-read.delim("activity.csv",sep=",",head=T,skip=0,as.is=T)
summary(activity)
head(activity)
```

2.) Process/transform the data (if necessary) into a format suitable for your analysis

```{r}
activity$date <- as.Date(activity$date, "%Y-%m-%d")
daily_steps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```
###What is mean total number of steps taken per day?

1.) Calculate the total number of steps taken per day
```{r}
sum(daily_steps$steps,na.rm=T)
```

sum 570608

2.) Make a histogram of the total number of steps taken each day

```{r}
hist(daily_steps$steps,xlab="day",col="light blue",main="Total Steps by Day",ylim=c(0,45))
```

![alt tag](https://github.com/fsimmz23/ReproduceResearch/blob/master/total_day_byhist_01.png)

3.) Calculate and report the mean and median of the total number of steps taken per day

```{r}
round(mean(daily_steps$steps,na.rm=T),2)
round(median(daily_steps$steps,na.rm=T),2)
```

mean 10766.19

median 10765

###What is the average daily activity pattern?

1.) Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the 
average number of steps taken, averaged across all days (y-axis)
```{r}
time_series<-tapply(activity$steps, activity$interval, mean, na.rm = TRUE)

plot(row.names(time_series),time_series,type="l",col="blue",ylab="average across all days"
,xlab = "5-min interval",main="Average Number of Steps Taken")
```

![alt tag](https://github.com/fsimmz23/ReproduceResearch/blob/master/time_series.png)

2.) Which 5-minute interval, on average across all the days in the dataset, contains 
the maximum number of steps?
```{r}
max(time_series)
which(time_series==max(time_series))
```

max 206.1698

series number 835

###Imputing missing values

1.) Calculate and report the total number of missing values in the dataset (i.e. the total 
number of rows with NAs)
```{r}
length(which(is.na(activity$steps)==TRUE))
```

length 2304

2.) Devise a strategy for filling in all of the missing values in the dataset. The strategy 
does not need to be sophisticated. For example, you could use the mean/median for that day, 
or the mean for that 5-minute interval, etc.

```{r}
NA_time_series<-activity$interval[which(is.na(activity$steps)==TRUE)]
head(row.names(time_series))

mat1<-matrix(NA, ncol=1, nrow=nrow(activity))

for(i in 1: length(NA_time_series))
	{
	index<-which(row.names(time_series)==NA_time_series[i])
	mat1[which(is.na(activity$steps)==TRUE)[i]]<-time_series[index]
	}	
	
 head(activity)
 
 head(mat1[1:nrow(activity)])
 
 new_act<-cbind(mat1,activity)
 
 
 for(i in 1: nrow(new_act))
	{
	if(is.na(new_act[i,2])==TRUE){
	new_act[i,2]<-new_act[i,1]
	} else{
		  new_act[i,2]<-new_act[i,2]
		  }
	}

head(new_act)
 
```
3.) Create a new dataset that is equal to the original dataset but with the missing data 
filled in.
```{r}
new_d.set<-new_act[,2:4]
```
4.) Make a histogram of the total number of steps taken each day and Calculate and report 
the mean and median total number of steps taken per day. Do these values differ from the 
estimates from the first part of the assignment? What is the impact of imputing missing 
data on the estimates of the total daily number of steps?
```{r}
new_d.steps<-aggregate(steps ~ date, data = new_d.set, sum, na.rm = TRUE)
hist(new_d.steps$steps,xlab="Number of Steps",col="red",main="Total Steps by Day",ylim=c(0,45))
hist(daily_steps$steps,xlab="Number of Steps",col="blue",main="Total Steps by Day",ylim=c(0,45), add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
```

![alt tag](https://github.com/fsimmz23/ReproduceResearch/blob/master/total_steps02.png)

The histogram has a higher peak height. The frequency now maxes out at 35 instead of 30.

```{r}
round(mean(new_d.steps$steps),2)
median(new_d.steps$steps)
```

mean 10766.19

median 10766.19

After replacing the values the mean is the same but the median is slightly different.

###Are there differences in activity patterns between weekdays and weekends?


For this part the weekdays() function may be of some help here. Use the dataset 
with the filled-in missing values for this part.


col.names
```{r}
new_d.set[,4]<-weekdays(new_d.set[,2])



head(data.frame(new_d.set,stringsAsFactors=FALSE))


day_type<-matrix(NA, ncol=1, nrow=length(weekdays(new_d.set[,2])))

for(i in 1:length(weekdays(new_d.set[,2])))
{

if(new_d.set[,4][i]=="Saturday")
	{
	day_type[i,1]<-"weekend"
	} else if(new_d.set[,4][i]=="Sunday")
		  {
		  day_type[i,1]<-"weekend"
		  } else{
		        day_type[i,1]<-"weekday"
		        }
}
```


Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
indicating whether a given date is a weekday or weekend day.
```{r}
new_d.set<-cbind(new_d.set,day_type)
colnames(new_d.set)<-c("steps","date","interval","day","day_type")
```

separate weekday and weekend by which function.

```{r}
new_d.set_we<- new_d.set[which((new_d.set)[,5]=="weekend"),]
new_d.set_wd<- new_d.set[which((new_d.set)[,5]=="weekday"),]

new_d.set2_we<-tapply(new_d.set_we$steps, new_d.set_we$interval, mean, na.rm = TRUE)
new_d.set2_wd<-tapply(new_d.set_wd$steps, new_d.set_wd$interval, mean, na.rm = TRUE)

```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval 
(x-axis) and the average number of steps taken, averaged across all weekday days or weekend days 
(y-axis). See the README file in the GitHub repository to see an example of what this plot should 
look like using simulated data.

```{r}

plot(row.names(new_d.set2_we),new_d.set2_we,type="l",col="blue",ylab="number of steps"
,xlab = "5-min interval",main="weekend")

plot(row.names(new_d.set2_wd),new_d.set2_wd,type="l",col="blue",ylab="number of steps"
,xlab = "5-min interval",main="weekday")
```

![alt tag](https://github.com/fsimmz23/ReproduceResearch/blob/master/weekend.png)

![alt tag](https://github.com/fsimmz23/ReproduceResearch/blob/master/weekday.png)


There are differences in activity.  The weekday has more activity in the morning and the weekend 
has more activity in the afternoon.