################################
### Biostatistical Methods ##
################################


library(tdROC)
library(pROC)
library(medicaldata)
library(survival)

#######################Time-dependent versions


library(ISLR2)
head(BrainCancer)

BrainCancer = BrainCancer[!is.na(BrainCancer$diagnosis),]

#let's now do a single split
total.n = length(BrainCancer$time)


#randomly split into training and test
set.seed(100)
training = sample(total.n, round(0.5*total.n), replace = F)
test = setdiff(1:total.n, training)
training.data = BrainCancer[training,]
test.data = BrainCancer[test,]

#fit the Cox model in training data
my.model = coxph(Surv(time, status) ~ sex + diagnosis + loc + ki + gtv + stereo, data = training.data)
summary(my.model)

#now calculate predictions in test data for survival to t=30
#estimate the baseline hazard
base.haz.estimate = basehaz(my.model,centered=FALSE)

t= 30
base.haz.estimate.30 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=t, method = "constant")$y

#calculate beta*Z
beta.z = predict(my.model, newdata = test.data, type = "lp", reference = "zero")

#calculate survival
pred.surv = exp(-base.haz.estimate.30*exp(beta.z))
pred.surv

pred.event = 1-pred.surv

#let's calculate sensitivity using a threshold 
theta = 0.50

#let's first calculate the Kaplan Meier estimate of survival
test.data$censor = 1-test.data$status
km = survfit(Surv(test.data$time, test.data$censor) ~ 1)
G.t = approx(x=km$time, y=km$surv, xout = t, method = "constant")$y
G.Xi = approx(x=km$time, y=km$surv, xout = test.data$time, method = "constant")$y

#create the weights
test.data$Wi = rep(0,length(test.data$time))
test.data$Wi[test.data$time>t] = 1/G.t 
test.data$Wi[test.data$time <= t & test.data$status == 1] = 1/G.Xi[test.data$time <= t & test.data$status == 1]

#sensitivity
sens = sum(test.data$Wi*(pred.event >= theta)*(test.data$time <= t))/sum(test.data$Wi*(test.data$time <= t))
sens

#specificity
spec = sum(test.data$Wi*(pred.event < theta)*(test.data$time > t))/sum(test.data$Wi*(test.data$time > t))
spec

#see https://cran.r-project.org/web/packages/timeROC/timeROC.pdf

install.packages("timeROC")
library(timeROC)
SeSpPPVNPV(theta, test.data$time, test.data$status, pred.event, cause = 1, weighting = "marginal", times = 30)

#can give multiple times
SeSpPPVNPV(theta, test.data$time, test.data$status, pred.event, cause = 1, weighting = "marginal", times = c(10,30,50))

#roc/auc
roc.object = timeROC(test.data$time, test.data$status, pred.event, cause = 1, weighting = "marginal", times = 30,ROC=TRUE)
roc.object

#plot roc
plot(roc.object, time = 30)

#multiple times
roc.object = timeROC(test.data$time, test.data$status, pred.event, cause = 1, weighting = "marginal", times = c(10,30,50),ROC=TRUE)
roc.object

plotAUCcurve(roc.object)


########################################
#class activity 

# Use the Paquid dataset in the timeROC package
#Redefine the status varianble which is currently - 
#status : the status indicator : 0 = censored, 1 = dementia onset and 2 = death without dementia.

data(Paquid)
head(Paquid)
Paquid$status.use = 1*(Paquid$status == 1)

#1. Fit a Cox model using the 2 covariates and calculate the training error rate in terms of the time-dependent AUC. Use t = 7 years.

#2. Now use cross-validation to estimate the test error rate

#3. What is the incremental value of the Digit Symbol Substitution Score Test?


#1. Fit a Cox model using the 2 covariates and calculate the training error rate in terms of the time-dependent AUC. Use t = 7 years.
my.model = coxph(Surv(time, status.use) ~ DSST + MMSE, data = Paquid)
summary(my.model)

base.haz.estimate = basehaz(my.model,centered=FALSE)

t= 7
base.haz.estimate.7 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=t, method = "constant")$y

#calculate beta*Z
beta.z = predict(my.model, type = "lp", reference = "zero")

#calculate survival
pred.surv = exp(-base.haz.estimate.7*exp(beta.z))
pred.surv

pred.event = 1-pred.surv

roc.object = timeROC(Paquid$time, Paquid$status.use, pred.event, cause = 1, weighting = "marginal", times = t,ROC=TRUE)
roc.object

#2. Now use cross-validation to estimate the test error rate
#I will use generalized cross-validation

M=100
h=0.9
test.error.vector = vector(length = M)
total.n = length(Paquid$time)
set.seed(50)

for(i in 1:M){
	training = sample(total.n, round(h*total.n), replace = F)
	test = setdiff(1:total.n, training)
	training.data = Paquid[training,]
	test.data = Paquid[test,]

	#fit the model with the training data
	my.model = coxph(Surv(time, status.use) ~ DSST + MMSE, data = training.data)

	base.haz.estimate = basehaz(my.model,centered=FALSE)

	t= 7
	base.haz.estimate.7 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=t, method = "constant")$y
	
	#predict in test data
	#calculate beta*Z
	beta.z = predict(my.model, newdata = test.data, type = "lp", reference = "zero")

	#calculate survival
	pred.surv = exp(-base.haz.estimate.7*exp(beta.z))
	pred.surv

	pred.event = 1-pred.surv

	roc.object = timeROC(test.data$time, test.data$status.use, pred.event, cause = 1, weighting = "marginal", times = t,ROC=TRUE)

	test.error.vector[i] = roc.object$AUC[2]
}
mean(test.error.vector)



#3. What is the incremental value of the Digit Symbol Substitution Score Test?

M=100
h=0.9
test.error.vector.both = vector(length = M)
test.error.vector.nodsst = vector(length = M)
total.n = length(Paquid$time)
set.seed(50)

for(i in 1:M){
	training = sample(total.n, round(h*total.n), replace = F)
	test = setdiff(1:total.n, training)
	training.data = Paquid[training,]
	test.data = Paquid[test,]

	#fit the model with the training data
	my.model = coxph(Surv(time, status.use) ~ DSST + MMSE, data = training.data)

	base.haz.estimate = basehaz(my.model,centered=FALSE)

	t= 7
	base.haz.estimate.7 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=t, method = "constant")$y
	
	#predict in test data
	#calculate beta*Z
	beta.z = predict(my.model, newdata = test.data, type = "lp", reference = "zero")

	#calculate survival
	pred.surv = exp(-base.haz.estimate.7*exp(beta.z))
	pred.surv

	pred.event = 1-pred.surv

	roc.object = timeROC(test.data$time, test.data$status.use, pred.event, cause = 1, weighting = "marginal", times = t,ROC=TRUE)

	test.error.vector.both[i] = roc.object$AUC[2]
	
	###drop DSST
	#fit the model with the training data
	my.model = coxph(Surv(time, status.use) ~ MMSE, data = training.data)

	base.haz.estimate = basehaz(my.model,centered=FALSE)

	t= 7
	base.haz.estimate.7 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=t, method = "constant")$y
	
	#predict in test data
	#calculate beta*Z
	beta.z = predict(my.model, newdata = test.data, type = "lp", reference = "zero")

	#calculate survival
	pred.surv = exp(-base.haz.estimate.7*exp(beta.z))
	pred.surv

	pred.event = 1-pred.surv

	roc.object = timeROC(test.data$time, test.data$status.use, pred.event, cause = 1, weighting = "marginal", times = t,ROC=TRUE)

	test.error.vector.nodsst[i] = roc.object$AUC[2]
}
mean(test.error.vector.both)
mean(test.error.vector.nodsst)

mean(test.error.vector.both)-mean(test.error.vector.nodsst)


