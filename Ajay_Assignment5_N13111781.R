#Question 1 :
#Ans:Code:
#Below I have installed the necessary packages and called the libraries 
install.packages("reshape")
install.packages("chron")
library(reshape)
library(chron)
hourly_data = read.csv(file.choose())
daily_data = read.csv(file.choose())
Vire(hourly_data)
View(daily_data)

#Plotting the count data with respect to hours and days

hours <- c(1:10197)
days <- c(1:426)
plot(hours, hourly_data$Steps..count., pch = 20, col = 'darkblue', type='l')
plot(days, daily_data$Steps..count., pch = 20, col = 'darkblue', type='l')
data1 <- data.frame(hourly_data$Steps..count.)


#Plotting the sum of hourly step count for the entire year

hourly_data1 = transform(hourly_data, Start = colsplit(Start, split = " ", names = c('a', 'b')))
hourly_data2 = transform(hourly_data1, Finish = colsplit(Finish, split = " ", names = c('a', 'b')))
View(hourly_data1)

x <- c(0:424)
check <- 424*24 +1
sum19hr <- sum(hourly_data2$Steps..count.[24*x + 1])
x <- c(0:424)
check <- 424*24 +2
sum20hr <- sum(hourly_data2$Steps..count.[24*x + 2])
x <- c(0:424)
check <- 424*24 +3
sum21hr <- sum(hourly_data2$Steps..count.[24*x + 3])
x <- c(0:424)
check <- 424*24 +4
sum22hr <- sum(hourly_data2$Steps..count.[24*x + 4])
x <- c(0:424)
check <- 424*24 +5
sum23hr <- sum(hourly_data2$Steps..count.[24*x + 5])
x <- c(0:424)
check <- 424*24 +6
sum0hr <- sum(hourly_data2$Steps..count.[24*x + 6])
x <- c(0:424)
check <- 424*24 +7
sum1hr <- sum(hourly_data2$Steps..count.[24*x + 7])
x <- c(0:424)
check <- 424*24 +8
sum2hr <- sum(hourly_data2$Steps..count.[24*x + 8])
x <- c(0:424)
check <- 424*24 +9
sum3hr <- sum(hourly_data2$Steps..count.[24*x + 9])
x <- c(0:424)
check <- 424*24 +10
sum4hr <- sum(hourly_data2$Steps..count.[24*x + 10])
x <- c(0:424)
check <- 424*24 +11
sum5hr <- sum(hourly_data2$Steps..count.[24*x + 11])
x <- c(0:424)
check <- 424*24 +12
sum6hr <- sum(hourly_data2$Steps..count.[24*x + 12])
x <- c(0:424)
check <- 424*24 +13
sum7hr <- sum(hourly_data2$Steps..count.[24*x + 13])
x <- c(0:424)
check <- 424*24 +14
sum8hr <- sum(hourly_data2$Steps..count.[24*x + 14])
x <- c(0:424)
check <- 424*24 +15
sum9hr <- sum(hourly_data2$Steps..count.[24*x + 15])
x <- c(0:424)
check <- 424*24 +16
sum10hr <- sum(hourly_data2$Steps..count.[24*x + 16])
x <- c(0:424)
check <- 424*24 +17
sum11hr <- sum(hourly_data2$Steps..count.[24*x + 17])
x <- c(0:424)
check <- 424*24 +18
sum12hr <- sum(hourly_data2$Steps..count.[24*x + 18])
x <- c(0:424)
check <- 424*24 +19
sum13hr <- sum(hourly_data2$Steps..count.[24*x + 19])
x <- c(0:424)
check <- 424*24 +20
sum14hr <- sum(hourly_data2$Steps..count.[24*x +20])
x <- c(0:424)
check <- 424*24 +21
sum15hr <- sum(hourly_data2$Steps..count.[24*x +21])
x <- c(0:423)
check <- 423*24 +22
sum16hr <- sum(hourly_data2$Steps..count.[24*x + 22])
x <- c(0:423)
check <- 423*24 +23
sum17hr <- sum(hourly_data2$Steps..count.[24*x +23])
x <- c(0:423)
check <- 423*24 +24
sum18hr <- sum(hourly_data2$Steps..count.[24*x +24])

hourly_sum <- c(sum19hr, sum20hr, sum21hr, sum22hr, sum23hr, sum0hr, sum1hr, sum2hr,
                sum3hr, sum4hr, sum5hr, sum6hr, sum7hr, sum8hr, sum9hr, sum10hr, sum11hr,
                sum12hr, sum13hr, sum14hr, sum15hr, sum16hr, sum17hr, sum18hr)
hour_values <- c(19:23, 0:18)
View(hour_values)
plot(hour_values, hourly_sum, pch =20, col='darkblue')
barplot(hourly_sum, main="Sum of Counts for Each Hourly data",
        xlab="Each Hour of a Day from 19Hrs to 18Hrs", 
        ylab="Total Count",  col="darkblue")

#Plotting the sum of weekly count for the entire year

daily_data1 <- as.data.frame(sapply(strsplit(as.character(daily_data$Start),' '), "[", 1))

View(daily_data1)
thetimes1 <- data.frame(chron(dates=as.character(daily_data1[,1]),format=c(dates="m/d/y")))
thetimes1<-as.data.frame(thetimes1)
thetimes1$day <- weekdays(as.Date(thetimes1[,1]))
dtparts <- cbind(daily_data1[,1],as.data.frame(thetimes1$day))
daily_data1 <- data.frame(thetimes1$day, daily_data$Steps..count.)
View(daily_data1)

x <- c(0:60)
check <- 60*7 +1
Mondaysum <- sum(daily_data$Steps..count.[7*x + 1])
x <- c(0:60)
check <- 60*7 +2
Tuesdaysum <- sum(daily_data$Steps..count.[7*x + 2])
x <- c(0:60)
check <- 60*7 +3
Wednesdaysum <- sum(daily_data$Steps..count.[7*x + 3])
x <- c(0:60)
check <- 60*7 +4
Thursdaysum <- sum(daily_data$Steps..count.[7*x + 4])
x <- c(0:60)
check <- 60*7 +5
Fridaysum <- sum(daily_data$Steps..count.[7*x + 5])
x <- c(0:60)
check <- 60*7 +6
Saturdaysum <- sum(daily_data$Steps..count.[7*x + 6])
x <- c(0:59)
check <- 59*7 +7
Sundaysum <- sum(daily_data$Steps..count.[7*x + 7])

daily_sum <- c(Mondaysum, Tuesdaysum, Wednesdaysum, Thursdaysum, Fridaysum,
               Saturdaysum, Sundaysum)
View(daily_string)
daily_string <- c('Monday', 'Tuesday', 'Wednesday', 'Thurday', 'Friday',
                  'Saturday', 'Sunday')

plot(daily_string, daily_string)

xxx <- barplot(daily_sum, main="Sum of Counts for Each Weekday",
        xlab="Day of the Week Monday to Sunday", ylab="Total Count",
        col="darkblue")
install.packages("grid")
library(grid)
grid.text(names(daily_string), x = unit(xxx, "native"), y= unit(-1, "lines"),
          just = "right")
#Question 2: 
#(a) If you have a classification model that outputs predicted probabilities, how could you
#convert those probabilities to class predictions? 
#Ans:Set a threshold, and classify everything above the threshold as a 1 and everything below the #threshold as a 0.
#(b)Why are predicted probabilities (rather than just class predictions) required to 
#generate an ROC curve?
#Ans:Because an ROC curve is measuring the performance of a classifier at all possible thresholds, and #thresholds only make sense in the context of predicted probabilities
#(c)Could you use an ROC curve for a regression problem? Why or why not?
#Ans:No, because ROC is a plot of TPR vs FPR, and those concepts have no meaning in a regression problem.
#(d)What's another term for True Positive Rate? (
#Ans:Sensitivity or recall.
#(e)If I wanted to increase specificity, how would I change the classification threshold?
#Ans:By increasing the threshold we can increase the specificity.
#(f)Is it possible to adjust your classification threshold such that both sensitivity and
#specificity increase simultaneously? Why or why not?
#Ans:No, because increasing either of those requires moving the threshold in opposite directions.
#(g)What are the primary benefits of ROC curves over classification accuracy?
#Ans:It doesn't require setting a classification threshold,allows you to visualize the performance of 
#your classifier ,works well for unbalanced classes.
#(h)What should you do if you have a low AUC value like 0.15?
#Ans:Reverse your predictions so that your AUC is 0.85.
#(i):What's a real-world scenario in which you would prefer high specificity (rather than
#high sensitivity) for your classifier?
#Ans:Speed cameras issuing speeding tickets.


