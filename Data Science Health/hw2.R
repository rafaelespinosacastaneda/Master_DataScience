################################
### Biostatistical Methods ##
################################


################################
### Always need the survival package ##
################################

install.packages("survival")
library(survival)

################################
### Data from class ##
################################

mydata <- read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/hw2_braincancer-1.csv", header=TRUE,sep =",")


mydata


#compute and plot Kaplan Meier estimator
#Note that Surv(x, delta) is how you tell R that this is a survival variable

km = survfit(Surv(mydata$time, mydata$status) ~mydata$stereo)


summary(km)




summary(km)
#heavy S(10) = 0.805. SRT
#light S(10) = 0.952. SRS

#Test the null hypothesis that S(10) is the same between the two groups. Do you reject the null hypothesis?

#s_heavy - s_light = 0.449-0.389
s.difference = 0.952-0.805
var.difference = 0.0465 ^2 + 0.0507^2
conf.interval = c(s.difference - 1.96*sqrt(var.difference), s.difference + 1.96*sqrt(var.difference))
conf.interval
#0 is in this interval so we fail to reject.

#if you want an explicit p-value, you calculate the Z test-statistic
z.test.statistic = s.difference/sqrt(var.difference)
z.test.statistic

#now you look at the area under the Normal(0,1) curve, and multiply by 2 for 2-sided test
p.value = 2*(1-pnorm(z.test.statistic))
p.value


survdiff(Surv(mydata$time, mydata$status) ~mydata$stereo)

