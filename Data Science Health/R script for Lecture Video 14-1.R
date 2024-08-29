################################
### Biostatistical Methods ##
################################


################################
### Always need the survival package ##
################################

library(survival)
library(asaur)

#####################################
## Residuals
#####################################

my.model = coxph(Surv(ttr, relapse) ~ age + grp + longestNoSmoke, data = pharmacoSmoking)
summary(my.model)

#martingale residuals
mm = residuals(my.model)

#plot age vs. residuals
plot(mm~pharmacoSmoking$age, main = "Martingale residuals versus age", pch=20)

#add a smoothed curve
smoothSEcurve <- function(yy, xx) {
	# use after a call to "plot"
	# fit a lowess curve and 95% confidence interval curve
	# make list of x values
	xx.list <- min(xx) + ((0:100)/100)*(max(xx) - min(xx))
	# Then fit loess function through the points (xx, yy) # at the listed values
	yy.xx <- predict(loess(yy ~ xx), se=T,
     	newdata=data.frame(xx=xx.list))
	lines(yy.xx$fit ~ xx.list, lwd=2)
  	lines(yy.xx$fit -qt(0.975, yy.xx$df)*yy.xx$se.fit ~ xx.list, lty=2)
  	lines(yy.xx$fit + qt(0.975, yy.xx$df)*yy.xx$se.fit ~ xx.list, lty=2) 
}

smoothSEcurve(mm, pharmacoSmoking$age)

#plot prior attempts vs. residuals
plot(mm~pharmacoSmoking$longestNoSmoke, main = "Martingale residuals versus longestNoSmoke")
smoothSEcurve(mm, pharmacoSmoking$longestNoSmoke)

#try with log transformation
my.model = coxph(Surv(ttr, relapse) ~ age + grp + log(longestNoSmoke+1), data = pharmacoSmoking)
mm = residuals(my.model)
plot(mm~log(pharmacoSmoking$longestNoSmoke+1), main = "Martingale residuals versus longestNoSmoke")
smoothSEcurve(mm, log(pharmacoSmoking$priorAttempts+1))

plot(mm~pharmacoSmoking$grp)

###Deviance residuals
dd = residuals(my.model, type = "deviance")
plot(dd~pharmacoSmoking$age, main = "Deviance residuals versus age")
smoothSEcurve(dd, pharmacoSmoking$age)



###########################################

# checking proportional hazards assumption, plot

surv.p = survfit(Surv(ttr, relapse) ~ 1, subset = (grp == "patchOnly"), data = pharmacoSmoking)
surv.c = survfit(Surv(ttr, relapse) ~ 1, subset = (grp == "combination"), data = pharmacoSmoking)
time.p = surv.p$time
time.c = surv.c$time

#calculate the complementary log log of survival
cll.surv.p=log(-log(surv.p$surv))
cll.surv.c=log(-log(surv.c$surv))

plot(log(time.p),cll.surv.p,typ="l",lwd=3)
points(log(time.c),cll.surv.c,typ="l",lwd=3, col = 2)

#####################################

#testing proportional hazards using shchoenfeld residuals

#let's go back to the brain cancer dataset

library(ISLR2)
my.model = coxph(Surv(time, status) ~ sex + diagnosis + loc + ki + gtv + stereo, data = BrainCancer)
model.test = cox.zph(my.model)
plot(model.test, var = "gtv")
abline(h=0, col=2)

plot(model.test, var = "sex")
abline(h=0, col=2)

model.test

#let's say you wanted to startify by sex i.e. allow the baseline hazard to be difference in males and females
my.model = coxph(Surv(time, status) ~ diagnosis + loc + ki + gtv + stereo + strata(sex), data = BrainCancer)
summary(my.model)



