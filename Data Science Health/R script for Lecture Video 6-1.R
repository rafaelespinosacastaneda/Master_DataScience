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

mydata = data.frame(id = 1:6, xi = c(381,226,43,238,111,149), deltai = c(0,0,1,1,0,1))

#plot ecdf
ss = sort(mydata$xi)
nn = length(mydata$xi)

plot(stepfun(c(ss), c(1,1-(1:nn)/nn)), xlim = c(0,300), verticals=T, pch=20, col.vert = "light grey", xlab = "Time, t", ylab = "Survival Probability", main = "Empirical Survival")

#compute and plot Kaplan Meier estimator
#Note that Surv(x, delta) is how you tell R that this is a survival variable

km = survfit(Surv(mydata$xi, mydata$deltai) ~ 1)
summary(km)

#default gives confidence intervals
plot(km, main = "Kaplan Meier", xlab = "Time, t", ylab = "Survival Probability", lwd = 2)

#ask to mark censoring time, and ask for no CI
plot(km, main = "Kaplan Meier", xlab = "Time, t", ylab = "Survival Probability", mark.time=TRUE, conf.int = FALSE)

#see that default is log transformation
km = survfit(Surv(mydata$xi, mydata$deltai) ~ 1, conf.type = "log")
summary(km)

#ask for no transformation
km = survfit(Surv(mydata$xi, mydata$deltai) ~ 1, conf.type = "plain")
summary(km)

#ask for Fleming-Harrington survival
km = survfit(Surv(mydata$xi, mydata$deltai) ~ 1, type = "fh")
summary(km)

#all matches our calculations from class

################################
### Parametric estimation ##
################################

#Let's generate some exponential survival data
n=100
t = rexp(n,rate = 3)
#censoring does not have to be exponential, it can be uniform or you can even make it = 1 for everyone, as long as it does NOT depend on t

c = rexp(n, rate = 1)
x = pmin(t,c)
delta = 1*(t<c)

#remember that we derived the estimate of lambda, the parameter for the exponential distribution (R calls it the "rate") in class as the sum(delta_i)/sum(x_i) so we can calculate that here as our estimate for lambda
our.estimate = sum(delta)/sum(x)
our.estimate

#you will get slightly different estimate because our seeds are not the same

#now let's estimate the same thing using the survreg function in 
surv.exp = survreg(Surv(x, delta) ~ 1, dist = "exponential")
summary(surv.exp)
#survreg is incredibly confusing. The \lambda here is actually exp(-Intercept)
exp(-surv.exp$coef)
#see that it is an exact match to what we calculated above.

#let's look at KM for this data
km = survfit(Surv(x, delta) ~ 1)
summary(km)
plot(km, main = "Kaplan Meier", xlab = "Time, t", ylab = "Survival Probability", conf.int=TRUE)

#now let's add the estimates we get using our exponential distribution
#making a time variable so I can calculate S(t) at each of these t's
mm = max(x)
time.scale = seq(0,mm, length=100)

#remember that when something T is exponential(lambda), then S(t) = exp(-lambda*t) so below I am calculating exactly that

surv.est.exp = exp(-our.estimate*time.scale)

#now I am adding it to the plot
points(time.scale, surv.est.exp, typ = "l", col = 3)

#the survival estimates using the kaplan-meier vs. the exponential are very close, which makes sense because the data ARE exponential

################################
### Parametric estimation when the model is wrong ##
################################

#Let's generate some log normal survival data
n=1000
t = exp(rnorm(n,mean = 1, sd = .5))

#again c can be anything
c = rexp(n, rate = 0.1)
x = pmin(t,c)
delta = 1*(t<c)

#let's look at KM for this data
km = survfit(Surv(x, delta) ~ 1)
summary(km)
plot(km, main = "Kaplan Meier", xlab = "Time, t", ylab = "Survival Probability", conf.int=FALSE, lwd = 3)

#But lets pretend we think it is exponential so we use an exponential model to calculate lambda
our.estimate = sum(delta)/sum(x)
our.estimate

#and then use that lambda to estimate the survival just like we did above
mm = max(x)
time.scale = seq(0,mm, length=100)
surv.est.exp = exp(-our.estimate*time.scale)

#add it to the plot
points(time.scale, surv.est.exp, typ = "l", col = 3, lwd = 3)

#since we generated the data from a log normal with mean = 1 and sd = 0.5, we actually know exactly the truth. Let's add the TRUE survival here. 
#plnorm with lower.tail = FALSE gives you exactly the P(T>t) if T is log normal with mean = meanlog and sd=sdlog

surv.true = plnorm(time.scale, meanlog = 1, sdlog = 0.5, lower.tail = FALSE)

#add it to the plot
points(time.scale, surv.true, typ = "l", col = 4, lwd = 3)

legend("topright", c("KM","Exponential", "Truth"), lty = 1, col = c(1,3,4), lwd = 3)

# see that we have a lot of bias; the survival estimates using the exponential are wrong; things go wrong when you assume a model that is not true


################################
### Real data ##
################################

#This is a Phase II (single sample) clinical trial of Xeloda and oxaliplatin (XELOX) chemotherapy given before surgery to 48 advanced gastric cancer patients with para- aortic lymph node metastasis (Wang et al. 2014). An important survival outcome of interest is progression-free survival, which is the time from entry into the clinical trial until progression or death, whichever comes first. The data, which have been extracted from the paper, are in the data set “gastricXelox” in the “asaur” package
#Reference: Wang, Y., Yu, Y., Li, W., Feng, Y., Hou, J., Ji, Y., Sun, Y., Shen, K., Shen, Z., Qin, X., Liu, T.: A phase II trial of xeloda and oxaliplatin (XELOX) neo-adjuvant chemotherapy followed by surgery for advanced gastric cancer patients with para-aortic lymph node metastasis. Cancer Chemother. Pharmacol. 73(6), 1155–1161 (2014)

install.packages("asaur")
library(asaur)
gastricXelox

#The x variable is gastricXelox$timeWeeks, the delta variable is gastricXelox$delta

#compute and plot the empirical CDF

#compute and plot the Kaplan Meier estimator

#What is S(40) = P(T>40)?

#Consider an exponential distribution, what is the estimated lambda?

#What is S(40) using the exponential model?

#Plot the exponential survival curve on top of the KM curve. How close are they?


#compute and plot the empirical CDF

ss = sort(gastricXelox$timeWeeks)
nn = length(gastricXelox$timeWeeks)

plot(stepfun(c(ss), c(1,1-(1:nn)/nn)), xlim = c(0,300), verticals=T, pch=20, col.vert = "light grey", xlab = "Time, t", ylab = "Survival Probability", main = "Empirical Survival")


#compute and plot the Kaplan Meier estimator
km = survfit(Surv(gastricXelox$timeWeeks, gastricXelox$delta) ~ 1)
summary(km)

plot(km, main = "Kaplan Meier plot for Gastric Cancer data", xlab = "Time in weeks", ylab = "Survival Probability", mark.time=TRUE)

#Notice that you have reproduced Figure 1 from this paper: https://link.springer.com/article/10.1007/s00280-014-2449-1

#What is S(40) = P(T>40)?

#You can look at the summary(km) output and see that S(40)=0.542 (because remember KM is a step function, if t is between two event times you get the survival for the most recent event time before t)
#S(40) = 0.542

#or you can use the approx function which does the same thing; "constant" tells it that it is a step function

approx(x=km$time, y=km$surv, xout = 40, method = "constant")

#Consider an exponential distribution, what is the estimated lambda?
#let's use the survreg function

surv.exp = survreg(Surv(gastricXelox$timeWeeks, gastricXelox$delta) ~ 1, dist = "exponential")
summary(surv.exp)
lambda = exp(-surv.exp$coef)

#What is S(40) using the exponential model?
surv.est.exp = exp(-lambda*40)
surv.est.exp
# 0.640


#Plot the exponential survive curve on top of the KM curve. How close are they?
#and plot both
plot(km, main = "Kaplan Meier plot for Gastric Cancer data", xlab = "Time in weeks", ylab = "Survival Probability", mark.time=TRUE, conf.int=FALSE, lwd = 3)
mm = max(gastricXelox$timeWeeks)
time.scale = seq(0,mm, length=100)
surv.est.exp = exp(-lambda*time.scale)
points(time.scale, surv.est.exp, typ = "l", col = 3, lwd = 3)

#not so close
######################################

# Let's look at exponential

#if something is exponential then the cumulative hazard should be linear, let's plot the cumulative hazard ==> 
# Remember for an exponential the cumulative hazard Lambda(t) = lambda*t, this means if you plot Lambda(t) vs. t, you should see a line.
# also Remember that S(t) = exp(-\Lambda(t)) for any survival variable. Doing some algebra this means Lambda(t) = -log(S(t)). So I take my Kaplan-meier estimate and transform it to Lambda(t), the cumulative hazard

cum.haz = -log(km$surv)

#now I plot it as a function of t
plot(km$time, cum.haz, typ = "l")

#does it look linear? Hard to tell, let't determine the best fit line and add it to the plot
results.lm = lm(cum.haz~ km$time)
abline(results.lm, lty = 2) 

#does it look linear? No. Exponential is not a good distribution to use for these data.

#we will come back to model checking when we do regression


################################
### Real data: Smoking  ##
################################


#As an example, let us consider the data set “pharmacoSmoking” in the “asaur” package, where the primary goal is to compare the time to relapse (defined in this study as return to smoking) between two treatment groups. The purpose of this study was to evaluate extended duration of a triple-medication combination versus therapy with the nicotine patch alone in smokers with medical illnesses. Patients with a history of smoking were randomly assigned to the triple-combination or patch therapy and followed for up to six months. The primary outcome variable was time from randomization until relapse (return to smoking); individuals who remained non-smokers for six months were censored.

head(pharmacoSmoking)

#here we care about comparing the two groups, patch only vs combination

plot(survfit(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$grp), main = "Kaplan Meier by group", xlab = "Time, t", ylab = "Survival Probability", conf.int=FALSE, col=c("blue", "red"))
legend("topright", legend=c("Combination", "Patch only"), col=c("blue","red") , lwd=2)

#the legend is always a little tricky for KM, survfit orders alphabetically
#This is to motivate our next section which is comparing survival between two groups.

