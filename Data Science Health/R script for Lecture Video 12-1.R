################################
### Biostatistical Methods ##
################################


################################
### Always need the survival package ##
################################

library(survival)
library(asaur)

################################
### Real data: Smoking  ##
################################


head(pharmacoSmoking)

#fit a cox proportional hazards model with just age

my.model = coxph(Surv(ttr, relapse) ~ age, data = pharmacoSmoking)
summary(my.model)

#let's estimate S(t) = P(T>t) for these different ages
#estimate the baseline hazard, very important to say FALSE for centered
base.haz.estimate = basehaz(my.model,centered=FALSE)

#we are going to estimate S(t) at t=2, get baseline hazard at this time point
time = 2
base.haz.estimate.2 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=time, method = "constant")$y

#calculate beta*Z
beta.z = my.model$coef*pharmacoSmoking$age

#calculate survival
pred.surv = exp(-base.haz.estimate.2*exp(beta.z))
pred.surv
predictions = cbind(pharmacoSmoking$age, pred.surv)

#let's make an informative plot
plot(predictions[,1], predictions[,2], pch = 20, xlab = "Age", ylab = "Predicted probability of relapse > 2 days")

################################
### let's fit another model adding in gender and treatment group
###############################

my.model.2 = coxph(Surv(ttr, relapse) ~ age + gender + grp, data = pharmacoSmoking)
summary(my.model.2)

#let's say you want the reference group to be patch only instead, this is how you do it
pharmacoSmoking$grp = relevel(pharmacoSmoking$grp, ref = "patchOnly")
my.model.2 = coxph(Surv(ttr, relapse) ~ age + gender + grp, data = pharmacoSmoking)
summary(my.model.2)

#let's again estimate S(t) at 2 days, but say we want this estimate for someone who is 45 years old, a female, and in the combination group
time = 2
base.haz.estimate = basehaz(my.model.2,centered=FALSE)
base.haz.estimate.2 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=time, method = "constant")$y

#calculate beta*Z
beta.z = t(my.model.2$coef)%*%c(45, 0, 1)
beta.z

#you can also use the predict function, but be careful: you need to ask for the linear predictor "lp" and tell it that you are NOT giving centered values i.e., reference = "zero"

#if you do not give newdata, it just gives you the B'Z for your dataset; this way is easier once you have multiple covariates and/or have categorical variables

predict(my.model.2, type = "lp", newdata = data.frame(age = 45, gender = "Female", grp = "combination"), reference = "zero")

#calculate survival
pred.surv = exp(-base.haz.estimate.2*exp(beta.z))
pred.surv

#now let's estimate S(t) for individuals age 30 to 60, assuming female, with one set of estimates assuming patch only group and one set assuming combination group, still at 2 days

ages = c(30:60)
beta.z.patch = cbind(ages, rep(0,length(ages)), rep(0, length(ages)))%*% my.model.2$coef
pred.surv.patch = exp(-base.haz.estimate.2*exp(beta.z.patch))

#how do we do this with predict?
people = data.frame(age = ages, gender = rep("Female",length(ages)), grp = rep("patchOnly", length(ages)))
beta.z.patch.v2 = predict(my.model.2, type = "lp", newdata = people, reference = "zero")
cbind(beta.z.patch, beta.z.patch.v2)
#this matches

beta.z.combination = cbind(ages, rep(0,length(ages)), rep(1, length(ages)))%*% my.model.2$coef
pred.surv.combination = exp(-base.haz.estimate.2*exp(beta.z.combination))

plot(ages, pred.surv.patch, pch = 20, xlab = "Age", ylab = "Predicted probability of relapse > 2 days", ylim = c(0.5,1), main = "Predicted probability of relapse > 2 days, Females, by group")
points(ages, pred.surv.combination, pch = 20, col = 3)
legend("bottomright", pch = 20, col = c(1,3), c("Patch only", "Combination"))


#let's say you want the probability of relapse before 2 days

plot(ages, 1-pred.surv.patch, pch = 20, xlab = "Age", ylab = "Predicted probability of relapse < 2 days", ylim = c(0,1), main = "Predicted probability of relapse < 2 days, Females, by group")
points(ages, 1-pred.surv.combination, pch = 20, col = 3)
legend("bottomright", pch = 20, col = c(1,3), c("Patch only", "Combination"))

###################################
## class activity###
#Our investigation is motivated by the BrainCancer dataset, which contains the survival times for patients with primary brain tumors undergoing treatment with stereotactic radiation methods. The predictors are gtv (gross tumor volume, in cubic centimeters); sex (male or female); diagnosis (meningioma, LG glioma, HG glioma, or other); loc (the tumor location: either infratentorial or supratentorial); ki (Karnofsky index); and stereo (stereotactic method: either stereotactic radiosurgery or fraction- ated stereotactic radiotherapy, abbreviated as SRS and SRT, respectively). Selingerova ́ et al. (2016) Survival of patients with primary brain tumors: Comparison of two statistical approaches. PLoS One, 11(2):e0148733.

install.packages("ISLR2")
library(ISLR2)
data(BrainCancer)
head(BrainCancer)

#Fit a Cox model with the 6 covariates. The time variable is in months.

#Write a paragraph describing your results.

#For each patient, estimate the probability of surviving past 10 months. What is the lowest probability? What is the highest probability?

# Estimate the probability of surviving past 10 months for each gross tumor volume value. Make a plot comparing gross tumor volume vs. the predicted probability. You decide how to set all of the other covariates e.g., at specific values.

#Fit a Cox model with the 6 covariates. The time variable is in months.

my.model = coxph(Surv(time, status) ~ sex + diagnosis + loc + ki + gtv + stereo, data = BrainCancer)
summary(my.model)

#Write a paragraph describing your results.

#We investigated the association between survival time and gross tumor volume, sex (male or female), diagnosis (meningioma, LG glioma, HG glioma, or other), the tumor location (either infratentorial or supratentorial), Karnofsky index, and stereotactic method (either stereotactic radiosurgery or fractionated stereotactic radiotherapy, abbreviated as SRS and SRT, respectively) using a Cox proportional hazards model among patients with primary brain tumors undergoing treatment with stereotactic radiation methods. Results showed that diagnosis (hazard ratio [HR; reference is meningioma] for HG glioma diagnosis = 8.62, p<0.001) and Karnofsky index (HR  = 0.95, p<0.01) were significantly associated with survival time. Holding all other variables constant, the estimated hazard of death among those with an HG glioma diagnosis was 8.62 times the hazard of death among those with a meningioma diagnosis. Holding all other variables constant, a higher Karnofsky index was associated with a lower hazard of death. Gross tumor volume, sex, tumor location, and stereotactic method were not associated with time to death.

#For each patient, estimate the probability of surviving past 10 months. What is the lowest probability? What is the highest probability?

#estimate the baseline hazard
base.haz.estimate = basehaz(my.model,centered=FALSE)
time = 10
base.haz.estimate.10 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=time, method = "constant")$y

#calculate beta*Z
beta.z = predict(my.model, type = "lp", reference = "zero")


#calculate survival
pred.surv = exp(-base.haz.estimate.10*exp(beta.z))
pred.surv

min(pred.surv)
#  0.0005814475

max(pred.surv)
#0.9915879

# Estimate the probability of surviving past 10 months for each gtv value. Make a plot comparing gtv vs. the predicted probability. You decide how to set all of the other covariates e.g., at specific values.

unique.gtv = unique(BrainCancer$gtv)
nn = length(unique.gtv)
mynewdata = data.frame(sex  = rep("Female",nn), diagnosis = rep("Meningioma",nn), loc = rep("Infratentorial", nn), ki = rep(80, nn), gtv =unique.gtv, stereo = rep("SRS", nn))

#calculate beta*Z
beta.z = predict(my.model, type = "lp", newdata = mynewdata, reference = "zero")

#calculate survival
pred.surv = exp(-base.haz.estimate.10*exp(beta.z))
pred.surv

plot(unique.gtv, pred.surv, pch = 20, xlab = "GTV", ylab = "Predicted probability of death > 10 months", main = "Predicted probability of death > 10 months by GTV")



