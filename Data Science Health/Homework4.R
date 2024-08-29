
library(tdROC)
library(pROC)
library(medicaldata)
library(survival)
library(asaur)
library(dplyr)
install.packages("timeROC")

library(timeROC)


mydata <- read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/hw4_lung-1.csv", header=TRUE,sep =",")
mydata





mydata<-mutate(mydata, status2 = case_when(status >1 ~ 1, status <2 ~ 0))






my.model = coxph(Surv(time, status2) ~ sex + age +ph.ecog, data = mydata)


base.haz.estimate = basehaz(my.model,centered=FALSE)

t= 400
base.haz.estimate.30 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=t, method = "constant")$y

#calculate beta*Z
beta.z = predict(my.model, newdata = mydata, type = "lp", reference = "zero")

#calculate survival
pred.surv = exp(-base.haz.estimate.30*exp(beta.z))
pred.surv

pred.event = 1-pred.surv






#roc/auc
roc.object = timeROC(mydata$time, mydata$status2, pred.event, cause = 1, weighting = "marginal", times = 400,ROC=TRUE)
roc.object
plotAUCcurve(roc.object)

roc.object$AUC

theta<-0.6
SeSpPPVNPV(theta, mydata$time, mydata$status2, pred.event, cause = 1, weighting = "marginal", times = 400)

71.92-50.73



mydata2 <- read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/hw4_BMT-1.csv", header=TRUE,sep =",")
mydata2









#first I make a variable splitting my sample
groupb  =  mydata2[mydata2$T1 >= 90 ,]
groupb$acute_GVHD_90 = 1*(groupb$deltaA == 1 & groupb$TA < 90)
groupb



km = survfit(Surv(T1, delta1) ~ acute_GVHD_90+Z1+Z3+Z5, data=groupb)
plot(km, col = c(1,2), lwd = 2, ylab = "Conditional survival probability", xlab = "Time")
legend("topright", lty = 1, col = c(1,2), lwd = 2, c("No transplant before 30 days", "Transplant before 30 days"))


summary(km)


survdiff(Surv(T1, delta1) ~ acute_GVHD_90+Z1+Z3+Z5, data=groupb)
#no difference

#can do a cox model with group b
#note that surgery is something we have at baseline, it is an indicator of prior bypass surgery
my.model = coxph(Surv(T1, delta1) ~ acute_GVHD_90+Z1+Z3+Z5, data=groupb)
summary(my.model)



new_data <- data.frame(1,55,1,0) 
colnames(new_data) <- c('acute_GVHD_90','Z1','Z3','Z5') 
new_data



base.haz.estimate = basehaz(my.model,centered=FALSE)

t= 500
base.haz.estimate.30 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=t, method = "constant")$y

#calculate beta*Z
beta.z = predict(my.model, newdata = new_data, type = "lp", reference = "zero")

#calculate survival
pred.surv = exp(-base.haz.estimate.30*exp(beta.z))
pred.surv

pred.event = 1-pred.surv





