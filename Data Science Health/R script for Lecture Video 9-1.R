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


#As an example, let us consider the data set “pharmacoSmoking” in the “asaur” package, where the primary goal is to compare the time to relapse (defined in this study as return to smoking) between two treatment groups. The purpose of this study was to evaluate extended duration of a triple-medication combination versus therapy with the nicotine patch alone in smokers with medical illnesses. Patients with a history of smoking were randomly assigned to the triple-combination or patch therapy and followed for up to six months. The primary outcome variable was time from randomization until relapse (return to smoking); individuals who remained non-smokers for six months were censored.

head(pharmacoSmoking)

#here we care about comparing the two groups, patch only vs combination

plot(survfit(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$grp), main = "Kaplan Meier by group", xlab = "Time, t", ylab = "Survival Probability", conf.int=FALSE, col=c("blue", "red"))
legend("topright", legend=c("Combination", "Patch only"), col=c("blue","red") , lwd=2)

#the legend is always a little tricky for KM, survfit orders alphabetically
#This is to motivate our next section which is comparing survival between two groups.

#Log-rank test
survdiff(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$grp)




#Stratify by gender

ss = survfit(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$grp + strata(pharmacoSmoking$gender))
summary(ss)

plot(ss, main = "Kaplan Meier by group and gender", xlab = "Time, t", ylab = "Survival Probability", conf.int=FALSE, col=c("red", "blue", "red", "blue"),lty = c(1,1,2,2), lwd = 2)
legend("topright", legend=c("Combination, Female", "Combination, Male","Patch only, Female","Patch only, Male"), col=c("red","blue", "red","blue"), lty = c(1,1,2,2) , lwd=2)

#stratified log-rank test
survdiff(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$grp + strata(pharmacoSmoking$gender))

#note that this is a randomized study

#Are there differences in survival by age?
table(pharmacoSmoking$ageGroup4)

#let's use some slightly prettier colors
library(RColorBrewer)
cols <- brewer.pal(4, "Dark2")

plot(survfit(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$ageGroup4), main = "Kaplan Meier by agegroup", xlab = "Time, t", ylab = "Survival Probability", conf.int=FALSE, col=cols, lwd=3)
legend("topright", legend=c("Age 21-34", "Age 35-49", "Age 50-64", "Age 65+"), col=cols , lwd=3)

#multi-group test
survdiff(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$ageGroup4)

#Exercises

#The variable levelSmoking has two levels:heavy and light

#Focusing on this variable, create the KM plot with a curve for each group

#What is an estimate of S(60) = P(T>60) in each group?

#Test the null hypothesis that S(60) is the same between the two groups. Do you reject the null hypothesis?

#Test for a difference between the two survival CURVES using the log rank test 


#The variable levelSmoking has two levels:heavy and light
#Focusing on this variable, create the KM plot with a curve for each group

km=survfit(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$levelSmoking)
plot(km, main = "Kaplan Meier by Smoking level", xlab = "Time, t", ylab = "Survival Probability", conf.int=FALSE, col=cols[1:2], lwd=3)
legend("topright", legend=c("Heavy","Light"), col=cols[1:2], lwd=3)

#What is an estimate of S(60) = P(T>60) in each group?

summary(km)
#heavy S(60) = 0.449
#light S(60) = 0.389

#Test the null hypothesis that S(60) is the same between the two groups. Do you reject the null hypothesis?

#s_heavy - s_light = 0.449-0.389
s.difference = 0.449-0.389
var.difference = 0.0527^2 + 0.0812^2
conf.interval = c(s.difference - 1.96*sqrt(var.difference), s.difference + 1.96*sqrt(var.difference))
conf.interval
#0 is in this interval so we fail to reject.

#if you want an explicit p-value, you calculate the Z test-statistic
z.test.statistic = s.difference/sqrt(var.difference)
z.test.statistic

#now you look at the area under the Normal(0,1) curve, and multiply by 2 for 2-sided test
p.value = 2*(1-pnorm(z.test.statistic))
p.value

#not significant

#Test for a difference between the two survival CURVES using the log rank test 

survdiff(Surv(pharmacoSmoking$ttr, pharmacoSmoking$relapse) ~ pharmacoSmoking$levelSmoking)

#no difference



