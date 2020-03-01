rm(list=ls())
library(rio)
library(car)
library(rio)
library(moments)
library(corrplot)

## Loading required package: carData
#PreProcessing

#1 Reading in CSV Data

cab_trips=read.csv(file="6304 Regression Project Data.csv", header=TRUE, sep=",")

colnames(cab_trips)=tolower(make.names(colnames(cab_trips)))
attach(cab_trips)

#2

set.seed(27683820)

my.cabInfo =cab_trips[sample(1:nrow(cab_trips),100,replace=FALSE),]

summary(my.cabInfo)

attach(my.cabInfo)


#3
#There are a few records wherein trip_seconds is 0 and trip_fair is some positive value.
#This is a case of INCORRECT DATA or missing value, since if the trip_seconds are 0, 
#ideally there shouldn't be any fare. So cleaning those values

my.cabInfo = subset(my.cabInfo, my.cabInfo$trip_seconds > 0 ,select = c("taxi_id","trip_seconds","trip_miles","fare","tips","extras","trip_total","payment_type"))

#Also there are few instances wherein the trip_miles is 0 but trip fair is again some positive value.
#This is also a case of missing or Incorrect Data, since if the miles covered is 0,
#then the fare should also be 0

sample_cabdata = subset(my.cabInfo, my.cabInfo$trip_miles > 0,select = c("taxi_id","trip_seconds","trip_miles","fare","tips","extras","trip_total","payment_type"))

#Finally after removing incorrect entries, we are left with 73 observations

summary(sample_cabdata)


#ANALYSIS

#1

d1 <- density(sample_cabdata$trip_seconds)
plot(d1, main="Total Time of Trip in Seconds", xlab="Time in Seconds")
polygon(d1, col="green", border="red")

#From the density plot we can say that most of the records lie between 60-2000 seconds
#and some lie in the vicinity of 4800 seconds

boxplot(sample_cabdata$trip_seconds,col="red",main="Boxplot for trip_seconds",xlab = "Time in Seconds")

min(sample_cabdata$trip_seconds)
max(sample_cabdata$trip_seconds)
median(sample_cabdata$trip_seconds)

skewness(sample_cabdata$trip_seconds)

summary(sample_cabdata$trip_seconds)

#As we can see in the box plot for seconds
#the minimum is at 60 seconds and the maximum value(which is an outlier) is 4680 seconds
#the median is 540 seconds and we have 4 outliers
#From the skewness we can see that the plot is right skewed with high skewness

########################

d2 <- density(sample_cabdata$trip_miles)
plot(d2, main="Boxplot for Total Trip in Miles", xlab = 'Total Trip Miles')
polygon(d2, col="green", border="red",xlab = "Miles Travelled")

#From the density plot we can see that most of the values are between 0.1 and 9.889
#and a few values are scattered between 0.100 to somewhere around 10

boxplot(sample_cabdata$trip_miles,col="red",main="Boxplot for trip_miles",xlab = "Total Miles Travelled")

min(sample_cabdata$trip_miles)
max(sample_cabdata$trip_miles)
median(sample_cabdata$trip_miles)
skewness(sample_cabdata$trip_miles)

summary(sample_cabdata$trip_miles)
#For the trip_miles boxplot,
#the minimum value is 0.1 mile and the maxmium value(which is an outlier) is 450 miles
#and the median is 1.6 miles. As shown in the boxplot, we have multiple outliers
#around 6-7
#The skewness for trip_miles is 8.25 which indicates that it is highly skewed to the right

###########################################
d3 <- density(sample_cabdata$fare)
plot(d3, main="Total Fare of the Trip",xlab = "Total Fare")
polygon(d3, col="green", border="red")

boxplot(sample_cabdata$fare,col="red",main="Boxplot for Fare",xlab = "Total Fare")

#From the density plot we can see that most values are between 3.75 to 20 and some are scattered from 20 to 60

min(sample_cabdata$fare)
max(sample_cabdata$fare)
median(sample_cabdata$fare)
skewness(sample_cabdata$fare)

summary(sample_cabdata$fare)

#For the fare  boxplot,
#the minimum value is 3.75 and the maxmium value(which is an outlier) is 56
#and the median is 8.75. As shown in the boxplot, we have multiple outliers
#around 10
#The skewness for fare is 1.9303 which indicated that it is skewed to the right

###########################################

d4 <- density(sample_cabdata$tips)
plot(d4, main="Tip for the Trip",xlab = "Tips")
polygon(d4, col="green", border="red")
#From the density plot we can see that most of the values are between 0 to 5 and a few are between 5 to 10

boxplot(sample_cabdata$tips,col="red",main="Boxplot for Tips", xlab = "Tips")
min(sample_cabdata$tips)
max(sample_cabdata$tips)
median(sample_cabdata$tips)
skewness(sample_cabdata$tips)

summary(sample_cabdata$tips)

#For the tips boxplot,
#the minimum value(and the 1st quartile which overlap) is 0 and the maxmium value(which is an outlier) is 10.15
#and the median is 1. As shown in the boxplot, we have multiple outliers
#around 7
#The skewness for tips is 1.75484, which is high and from the plot we can say that it is right skewed
###########################################

d5 <- density(sample_cabdata$extras)
plot(d5, main="Extra Incidentals of Trip",xlab = "Extras")
polygon(d5, col="green", border="red")

#From the plot we can see that most of the values are between 0 to 1 and rest are scattered beyond 2 till 6

boxplot(sample_cabdata$extras,col="red",main="Boxplot for Extras",xlab = "Extras")

min(sample_cabdata$extras)
max(sample_cabdata$extras)
median(sample_cabdata$extras)
skewness(sample_cabdata$extras)

summary(sample_cabdata$extras)

#For the extras boxplot,
#the minimum value(and the 1st quartile and also the median which overlap) is 0 and the maxmium 
#value(which is an outlier) is 5 #and the median is 0. As shown in the boxplot, we have multiple outliers
#around 3
#The skewness for extras is 1.912984, and from the plot we can say that it is right skewed

###########################################

d6 <- density(sample_cabdata$trip_total)
plot(d6, main="Final Amount of Trip",xlab = "Trip Total Amount")
polygon(d6, col="green", border="red")

#From the density plot, we can say that most of the values are between 4.45 to 20 while few other points
#are from 20 to 60.90

boxplot(sample_cabdata$trip_total,col="red",main="Boxplot for Total Amount",xlab = "Trip Total Amount ")


min(sample_cabdata$trip_total)
max(sample_cabdata$trip_total)
median(sample_cabdata$trip_total)
skewness(sample_cabdata$trip_total)

summary(sample_cabdata$trip_total)

#For the trip_total boxplot,
#the minimum value is 4.45 and the maxmium value(which is an outlier) is 60.9
#and the median is 9.75. As shown in the boxplot, we have multiple outliers
#around 9
#The skewness for trips_total is 1.894067 and from the plot we can see that the plot is right skewed

###########################################

#2

table(sample_cabdata$payment_type)
#The table method when used on payment_type, returns the total count of "CASH",
#"CREDIT CARD" and "OTHER" payments 


#3

sample_data_correlation = cor(sample_cabdata[sapply(sample_cabdata, is.numeric)])
sample_data_correlation

#The sample_data_correlation matrix shows the matrix of correlation.
#It measure the linear relation and strength between two variables
#For example the cor between trip_seconds and trip_miles is 0.0997. It is a positive number
#it mean that if trip_seconds increases, trip_miles also increases.
#Another example as per the cor, the relation between tips and trip_miles is negative,
#it means as the trip_miles increases the tips decreases or vice versa
#The best correlation is between fare and trip_miles

#In order to aid in better understanding of correlation, I have also shown a visual plot
corrplot(sample_data_correlation, method = "number",title = "visualization of a correlation matrix")


#4

lm_sample_cab = lm(formula = fare ~ trip_seconds + trip_miles + payment_type, data = sample_cabdata)
confint(lm_sample_cab, level = 0.95)
summary(lm_sample_cab)

plot(sample_cabdata$fare,lm_sample_cab$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#This linear model was built using fare as the dependent variable and trip_seconds, trip_miles, and payment_type
#as dependent variables. Keeping the confience interval at 95%, we get the values, the confidence interval values
#for (Intercept)  -2.10607443 to 2.50346298, 
#for trip_seconds  0.01325227 to  0.01718661,
#for trip_miles   -0.02324120 to  0.02733108 and 
#for payment_type   0.49333928 to  5.81493280
#The equation for fare would be
#fare = 0.198694 + 0.0152194 * trip_seconds + 0.0020449 * trip_miles + 3.154136 * payment_type
#for every change in trip_seconds, the fare changes by 0.0152194, and its p value is significantly small (<2e-16), 
#hence we reject the null hypothesis
#for every change in trip_miles, the fare changes by 0.0020449, but its p value is significantly large(0.8723), 
#hence we fail to reject the null hypothesis since there is not enough evidence
#for every change in payment, the fare changes by 3.154136, and its p value is significantly small (0.0209), 
#hence we reject the null hypothesis 
#for the intercept, if the miles and seconds travelled is 0, and payment is also 0, then the total fare
#should be 0.198694, which doesnt make any sense. Also its pvalue is significantly lare (0.8640),
#hence we fail to reject the null hypothesis since there is not enough evidence
#Also the value for R-sq is 0.794 and Adj R-sq is 0.7851 which tells us the percentage that the three independent
#variables describe the dependent one.

#By looking at the plot we can see that the points are clustered at the start, with few outliers
#
#################################################


#5

#Creating squared variables to use for generating linear model
sample_cabdata$miles_sq = 0

sample_cabdata$miles_sq = sample_cabdata$trip_miles^2

sample_cabdata$trip_seconds_sq = 0

sample_cabdata$trip_seconds_sq = sample_cabdata$trip_seconds^2

############
lm_sample_cab = lm(formula = fare ~ trip_seconds + trip_miles + payment_type, data = sample_cabdata)
summary(lm_sample_cab)

plot(sample_cabdata$fare,lm_sample_cab$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#This is the standard linear model considering all the dependent variables(as explained in question number 4),
#the equation for line here is
#fare = 0.198694 + 0.0152194 * trip_seconds + 0.0020449 * trip_miles + 3.154136 * payment_type

#----------------------------------------

#Removing Payment

lm_sample_cab_no_payment = lm(formula = fare ~ trip_seconds + trip_miles, data = sample_cabdata)
summary(lm_sample_cab_no_payment)

plot(sample_cabdata$fare,lm_sample_cab_no_payment$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#Here we remove the payment record and build a linear model using only trip_seconds and trip_miles
#the equation of line is 1.616472 + 0.015615 * trip_seconds - 0.001569 * miles
#For intercept the value is 1.616472. Its p value is 0.117 which is significantly large. It means if we change
#the trip_seconds and trip_miles to 0, the fare would be 1.61647 which isn't correct. Hence judging for the p value,
#we fail to reject the null hypothesis for the intercept
#In case of trip_seconds, if the value for trip_seconds changes, the fare also changes by 0.015615. Its p value
#is 2e-16 which is significanlty small and hence we reject the null hypothesis
#In case of trip_miles, if the value for trip_miles changes, the fare also changes by 0.001569. Its p value
#is 0.904 which is significanlty large and hence we fail to reject the null hypothesis, due to lack of evidence
#The value of R-sq is 0.7774 and for adj R-sq is 0.771 which is a good indicator for how the independent
#variables describe the dependent variable

##################################################

lm_sample_cab_no_miles = lm(formula = fare ~ trip_seconds + payment_type, data = sample_cabdata)
summary(lm_sample_cab_no_miles)
plot(sample_cabdata$fare,lm_sample_cab_no_miles$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#If we do not consider the trip_miles, the equation for line that is obtained is
#fare = 0.2184070 + 0.0152382 * trip_seconds + 3.1281896 * payment_type
# If trip_seconds changes, the value for fare changes by 0.0152382. Its pvalue is less than 2e-16, which is
#significantly small. Hence we reject null hypothesis
#If changes payment_type, the value for fare changes by 3.1281896. Its p value is 0.0201, which is significantly
#small. Hence we rejet null hypothesis
#For intercept, if both trip_seconds and payment_type is 0, the fare would be 0.2184070, which is not correct.
#Its p value is 0.8487, which is significantly large, hence we fail to reject the null hypothesis.
#The value for R-sq is 0.794 and for adj R is 0.7881, it tells how the independent variables describle the dependent one
##########################################

#NOT A GOOD FIT
lm_sample_cab_no_seconds = lm(formula = fare ~ trip_miles + payment_type, data = sample_cabdata)
summary(lm_sample_cab_no_seconds)

plot(sample_cabdata$fare,lm_sample_cab_no_seconds$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#This is not a good fit since R-sq = 0.083 and Adj R-sq = 0.0568

##########################################

lm_sample_cab_only_seconds = lm(formula = fare ~ trip_seconds , data = sample_cabdata)
summary(lm_sample_cab_only_seconds)

plot(sample_cabdata$fare,lm_sample_cab_only_seconds$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#Here we are only considering trip_second for the Linear Model. The equation for fare becomes
# Fare = 1.6102035 + trip_seconds * 0.0156034. It mean when the trip_seconds changes, the fare changes by
#0.0156034. The p value for fair is,less than 2e-16 which is significantly small, hence we reject the null hypothesis.
#The intercepts value is 1.6102035, and its pvalue is 0.116 which is significantly large, hence we fail
#to reject Null Hypothesis since we dont have enough evidence.

#########################################
#NOT GOOD
lm_sample_cab_only_binpayment = lm(formula = fare ~ payment_type, data = sample_cabdata)
summary(lm_sample_cab_only_binpayment)
plot(sample_cabdata$fare,lm_sample_cab_only_binpayment$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#SAME VALUE FOR BOTH

#NOT GOOD

lm_sample_cab_only_payment = lm(formula = fare ~ payment_type, data = sample_cabdata)
summary(lm_sample_cab_only_payment)
plot(sample_cabdata$fare,lm_sample_cab_only_payment$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)


#All the values are the same if we consider, bin_payment or payment, hence we consider payment only
#Also the R-sq = 0.07109 and the Adj. R-sq = 0.05801, hence this model is not a good fit

#------------------------------------------------

#BAD FIT
lm_sample_cab_only_miles = lm(formula = fare ~  trip_miles , data = sample_cabdata)
summary(lm_sample_cab_only_miles)

plot(sample_cabdata$fare,lm_sample_cab_only_miles$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#This is a bad fit
#Also the R-sq = 0.006587 and Adj. R-sq = -0.007404

################################################

lm_sample_cab_only_miles_sq = lm(formula = fare ~  trip_miles + miles_sq , data = sample_cabdata)
summary(lm_sample_cab_only_miles_sq)

plot(sample_cabdata$fare,lm_sample_cab_only_miles_sq$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#Here we are considering only the trip_miles and its squared value stored in miles_sq. 
#The equation for fare would be: fare = 4.7551053 + 2.3833269 * trip_miles - 0.0052553 * miles_sq
#If trip_miles changes, the value for fare changes by 2.3833269. The p value for trip_miles is <2e-16, which
#is significantly less, hence we can reject the Null Hypothesis. 
#If miles_sq changes, the value for fare changes by -0.0052553. The p value for miles_sq is < 2e-16, which
#is significantly less, hence we can reject the Null Hypothesis. 
#For Intercept, the pvalue is 1.41e-09, hence we reject the Null Hypothesis
#The  R-sq = 0.861 and Adj R-sq = 0.8571
##########################################

#NOT GOOD AFER SQUARE
lm_sample_cab_only_seconds_sq =lm(formula = fare ~  trip_seconds + trip_seconds_sq , data = sample_cabdata)
summary(lm_sample_cab_only_seconds_sq)

plot(sample_cabdata$fare,lm_sample_cab_only_seconds_sq$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#Here we are considering only trip_seconds and trip_seconds_sq for building the linear model.
#The equation for fare would be
# fare = -2.227e+00 + 2.367e-02 * trip_seconds + -2.249e-06 * trip_seconds_sq

#The p value for trip_seconds, trip_seconds_sq and Intercept is < 2e-16, 4.88e-05 and 0.083, which are 
#significantly small, hence we reject the Null Hypothesis
#The  R-sq = 0.8243 and Adj R-sq = 0..8193
#######################################################################  

lm_sample_cab_no_payment_time_sq = lm(fare ~ trip_seconds+trip_seconds_sq + trip_miles, data = sample_cabdata)
summary(lm_sample_cab_no_payment_time_sq)

plot(sample_cabdata$fare,lm_sample_cab_no_payment_time_sq$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#Here we consider trip_seconds, trip_seconds_sq, trip_miles for building the linear model.
#The equation for fare becomes :
# fare = -2.249e+00 + 2.382e-02 * trip_seconds - 2.276e-06 *  trip_seconds_sq - 6.444e-03 * trip_miles
#If trip_seconds, trip_seconds_sq or trip_miles change, the fare changes by 2.382e-02 and 2.276e-06 and 
#6.444e-03 respictively
#The p value of intercept is 0.0818, which is a bit high and hence we fail to reject the Null Hypothesis due to lack of evidence 
#for trips_seconds is <2e-16,hence we reject the null hypothesis,
#for trip_seconds_sq is 4.75e-05 hence we reject the null hypothesis,and for trip_miles is 0.5819,
#which is significantly high,
#hence we fail to reject the null hypothesis
#The  R-sq = 0.8251 and Adj R-sq = 0.8175
###############################################################################

#FINAL BEST MODEL
lm_sample_cab_no_payment_miles_sq = lm(fare ~ trip_seconds + trip_miles + miles_sq, data = sample_cabdata)
summary(lm_sample_cab_no_payment_miles_sq)

plot(sample_cabdata$fare,lm_sample_cab_no_payment_miles_sq$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#Here we consider trip_seconds, trip_miles and miles_sq for building the linear model.
#The equation for fare:
#fare = 2.6668736 + 0.0064833 * trip_seconds + 1.6087559 * trip_miles - 0.0035525 * miles_sq
#Here for all the variables, we get a pvalue such that we can reject the Null Hypothesis in every case.
#For Intercept, the pvalue is 0.000215, for trip_seconds, the pvalue is 4.46e-07, for trip_miles, the pvalue is 3.18e-14
#and for miles_sq, the pvalue is  2.91e-14 which are all less than 0.05. We can reject the Null Hypothesis
#The  R-sq = 0.9042 and Adj R-sq = 0.9
##########################################################################################

#GOOD MODEL

lm_sample_cab_sum_sq = lm(fare ~ trip_seconds + trip_seconds_sq + trip_miles + miles_sq, data = sample_cabdata)
summary(lm_sample_cab_sum_sq)

plot(sample_cabdata$fare,lm_sample_cab_sum_sq$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#Here we are using trip_seconds, trip_seconds_sq, trip_miles and miles_sq for building the linear model.
#The equation for fare would be
#fare = 1.264e+00 + 1.010e-02 * trip_seconds - 7.686e-07 * trip_seconds_sq + 1.458e+00 * trip_miles - 3.223e-03 * miles_sq
#the p value for Intercept is significantly large 0.2234, hence we fail to reject Null Hypothesis, due to
#lack of evidence
#and p value for trip_seconds_sq is 0.0765, 
#Hence we reject the Null Hypothesis in this case due to lack of evidence
#The R-sq value is 0.9085 and Adj R-sq is 0.9032

########################################################################################################

lm_sample_cab_sum_sq_payment = lm(fare ~ trip_seconds + trip_seconds_sq + trip_miles + miles_sq + payment_type, data = sample_cabdata)
summary(lm_sample_cab_sum_sq_payment)

plot(sample_cabdata$fare,lm_sample_cab_sum_sq_payment$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#Here we are considering trip_seconds, trip_seconds_sq, trip_miles, miles_sq and payment_type for building the
#linear model. The equation for fare would be:
#fare = 1.018e+00 + 9.634e-03 * trip_seconds -6.481e-07 * trip_seconds_sq + 1.443e+00 * trip_miles - 3.188e-03 * miles_sq + 9.817e-01 * payment_type
#the p value for intercept, trip_seconds_sq and payment_type is significantly high 0.33869, 0.14786 and 0.30505, hence we reject the
#Null Hypothesis.
#Also the R-sq value is 0.91 and Adj R-sq value is 0.9033
#The p value of trip_seconds,trip_seconds_sq and payment_type is significantly large
#Hence we reject the Null Hypothesis due to lack of evidence
#For the rest, the p value is significanlty small

#For interactions we do the following

lm_sample_cab_intercations = lm(formula = fare ~ trip_seconds + trip_miles + payment_type + trip_seconds:trip_miles, data = sample_cabdata)
summary(lm_sample_cab_intercations)

plot(sample_cabdata$fare,lm_sample_cab_intercations$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#Here we consider the interaction of trip_seconds and trip_miles,
#the equation for fare becomes
#fare = -0.6399 + trip_seconds * 0.016 + trip_miles * 0.1225 + payment_type 2.899 - trip_seconds * 0.0001
# the fare changes


lm_sample_cab_intercations_sq = lm(formula = fare ~ trip_seconds + trip_miles + payment_type + trip_seconds:trip_miles + trip_seconds_sq,data = sample_cabdata)
summary(lm_sample_cab_intercations_sq)

plot(sample_cabdata$fare,lm_sample_cab_intercations_sq$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#Here we also consider trip_seconds_sq bu no miles_sq.
#The equation for fare would be
#Fare = -6.009e-02 + 2.157e-02 * trip_seconds - 1.121e+00  * trip_miles + 1.001e+00 * payment_type + 1.243e-03 * trip_seconds:trip_miles -5.868e-06 * trip_seconds_sq
#The p value of Intercept and payment_type is significanlty large 0.961 and 0.375 respictively.
#Hence we fail to reject the Null Hypothesis. For rest the pvalue is well below the acceptable limit of 0.05
#The value for R-sq is 0.8753 and Adj R-sq is 0.866


#6

#LINE ASSUMPTIOMS

#Linearity

plot(sample_cabdata$fare,lm_sample_cab_no_payment_miles_sq$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#The "Actual v/s Fitted values" plot shows the points clustered near the lower tail
#There are few outliers in the plot as well. Hence the plot is linear


#Normality

qqnorm(lm_sample_cab_no_payment_miles_sq$residuals,pch=19,main="P&O Normality Plot")
qqline(lm_sample_cab_no_payment_miles_sq$residuals,col="green",lwd=3)

#The "P&O Normality Plot" plot shows that the points are normal in nature except some points
#which are towards the upper tail i.e. increased residuals
#There are some outliers in the plot as well but the plot shows the points are normal in nature.

#Equality of Variance
plot(lm_sample_cab_no_payment_miles_sq$fitted.values,lm_sample_cab_no_payment_miles_sq$residuals,pch=19,main="P&O Residuals",
     xlab = "Fitted Values",ylab="Residuals")
abline(0,0,col="orange",lwd=3)

#The "P&O Residuals" plot shows that the residuals are not equivalent to variance .
#There are many outliers in the plot as well.
#Hence we can say that the outliers do not affect the accuracy of the model

#7

#Since there are a lot of outliers in the plot we check for leverage.
#We expell the points with high leverage 

leverage=hat(model.matrix(lm_sample_cab_no_payment_miles_sq))
plot(leverage,pch=19)
abline(3*mean(leverage),0,col="green",lwd=3)

#from the plot we can see there are around 4 - 5 points which can be considered as outliers.
#by using Which function, returns the indices of the logical object when it is TRUE.

leverage_points = which(leverage>3*mean(leverage))

leverage_points
#There are 5 outliers, the points are '2, 19, 20, 24, 36' are out of the leverage level
#here are the details of the points which are inappropriate for the model.

sample_cabdata[2,]

sample_cabdata[19,]

sample_cabdata[20,]

sample_cabdata[24,]

sample_cabdata[36,]

#Here we remove the inappropriate points and build a new model using the new dataset

final_regression = sample_cabdata[-c(leverage_points),]
summary(final_regression)

#Rerunning the models using the new data afer removing the outliers

reg_without_outliers = lm(fare ~ trip_seconds + trip_miles + miles_sq , data = sample_cabdata)

summary(reg_without_outliers)

plot(sample_cabdata$fare,reg_without_outliers$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="red",lwd=3)

#As we can see we get the same values after removing the outliers even for R-sq = 0.9042 and Adj R-sq = 0.9, 
#i.e. both the models are quite similar

#The equation for fare would be
# fare = 2.6668736 + trip_seconds * 0.0064833 + trip_miles * 1.6087559 - miles_sq * 0.0035525 

#All the p values are significantly low hence we reject the null hypothesis

#8

#Adding +5 to my U number 27683820 to set the new seed
set.seed(27683825)

my.testCabInfo = cab_trips[sample(1:nrow(cab_trips),100, replace = FALSE),]
#There are some records wherein the trip seconds and trip_miles are zero, but the fare is still some
#positive number. Hence cleaning up such records

test_cabModel = subset(my.testCabInfo, trip_seconds > 0 & trip_miles > 0, -c(tolls,taxi_id))

sum(is.na(test_cabModel))

test_cabModel$trip_miles_sq = test_cabModel$trip_miles ^ 2
attach(test_cabModel)

##########################################

test_cabModel_lm = lm(fare ~  trip_seconds + trip_miles + trip_miles_sq, data = test_cabModel)
summary(test_cabModel_lm)

plot(test_cabModel$fare,test_cabModel_lm$fitted.values,pch=19,main="Actual v/s Fitted values",
     xlab = "fare", ylab = "Fitted Values")
abline(0,1,col="cyan",lwd=3)

#Using the best model from question 6 on the new dataset, we get a R-sq = 0.9539 and Adj R-sq = 0.952.
#The value for Intercept is 3.1029464 and its p value is 2.37e-06
#The value for trip_seconds is 0.0080489 and its p value is 7.98e-16
#The value for trip_miles is 1.1125162 and its p value is 7.22e-08
#The value for trip_miles_sq is 0.0173281 and its p value is 0.00291 
#Hence in all the cases, we Reject the Null Hypothesis.
#The equation for fare would be
# fare = 3.1029464 + 0.0080489 * trip_seconds + 1.1125162 * trip_miles + 0.0173281 * trip_miles_sq

