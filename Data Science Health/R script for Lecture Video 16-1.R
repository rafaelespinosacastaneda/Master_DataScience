################################
### Biostatistical Methods ##
################################


################################
### Always need the survival package ##
################################

library(survival)
library(asaur)


##################################
## modeling decisions 
##################################


#likelihood ratio test
model.a = coxph(Surv(ttr, relapse) ~ age + grp + employment, data = pharmacoSmoking)
summary(model.a)

model.b = coxph(Surv(ttr, relapse) ~ age + grp, data = pharmacoSmoking)
summary(model.b)

logLik(model.a)
logLik(model.b)
lrt = 2*(-375.1393 + 379.2411)
pchisq(lrt, df = 2, lower = F)

#same as 
anova(model.a, model.b)


##################################

#AIC
model.c = coxph(Surv(ttr, relapse) ~ age + grp + gender + longestNoSmoke + race, data = pharmacoSmoking)

AIC(model.a)
AIC(model.c)


#BIC 
BIC(model.a)
BIC(model.c)
