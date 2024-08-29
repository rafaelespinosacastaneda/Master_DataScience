############################################

### Observational Studies ###

############################################

############################################

### Double Robust estimation ###

############################################


############################################
### lindner dataset ###
############################################


#We will use the lindner dataset
#The lindner data contain data on 996 patients treated at the Lindner Center, Christ Hospital, Cincinnati in 1997. Patients received a Percutaneous Coronary Intervention (PCI). 
#The outcome is binary: sixMonthSurvive, whether patients survived to six months post treatment [denoted by TRUE] or did not survive to six months [FALSE]
#The treatment variable is abcix, where 0 indicates PCI treatment and 1 indicates standard PCI treatment and additional treatment in some form with abciximab (a blood thinner). 
#Covariates include acutemi, 1 indicating a recent acute myocardial infarction and 0 not; ejecfrac for the left ventricle ejection fraction, a percentage from 0 to 90; ves1proc giving the number of vessels (0 to 5) involved in the initial PCI; stent with 1 indicating coronary stent inserted, 0 not; diabetic where 1 indicates that the patient has been diagnosed with diabetes, 0 not; height in centimeters and female coding the sex of the patent, 1 for female, 0 for male.

library(twang)
data(lindner)
?lindner
head(lindner)

#outcome model

model = glm(sixMonthSurvive~abcix +stent + height + female + diabetic + acutemi + ejecfrac + ves1proc, data = lindner, family = "binomial")
summary(model)

#IPW estimator where we get weights using logistic regression

ps.logit = glm(abcix ~ stent + height + female + diabetic + acutemi + ejecfrac + ves1proc, data=lindner, family = "binomial")
my.preds = predict(ps.logit, type = "response")
lindner$w.logit = rep(1,nrow(lindner))
lindner$w.logit[lindner$abcix==1] = 1/my.preds[lindner$abcix==1]
lindner$w.logit[lindner$abcix==0] = 1/(1-my.preds[lindner$abcix==0])

#dx.wts() from the twang package diagnoses the balance for an arbitrary set of weights producing a balance table. This function requires the user to specify the estimand argument in order to perform the appropriate calculations relative to the target group on which we are drawing inferences. 

bal.logit <- dx.wts(x = lindner$w.logit, data=lindner,vars=c("stent", "height", "female" , "diabetic", "acutemi", "ejecfrac", "ves1proc"), treat.var="abcix", perm.test.iters=0, estimand = "ATE")
bal.table(bal.logit)
		
library(survey)		
design.ps = svydesign(ids=~1, weights = ~w.logit, data=lindner)
		
ipw = svyglm(sixMonthSurvive~abcix, family = "binomial", design=design.ps)
summary(ipw)

#double robust

dr = svyglm(sixMonthSurvive~abcix +stent + height + female + diabetic + acutemi + ejecfrac + ves1proc, family = "binomial", design=design.ps)
summary(dr)		

#outcome
summary(model)$coef[2,]
#ipw
summary(ipw)$coef[2,]
#dr
summary(dr)$coef[2,]
