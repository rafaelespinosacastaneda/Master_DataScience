################################
### Biostatistical Methods ##
################################

library(survival)
head(jasa)

#Problematic

#plot km plot
km = survfit(Surv(futime, fustat) ~ transplant, data=jasa)
plot(km, col = c(1,2), lwd = 2, ylab = "Survival probability", xlab = "Time")
legend("topright", lty = 1, col = c(1,2), lwd = 2, c("No transplant", "Transplant"))

#logrank test
survdiff(Surv(futime, fustat) ~ transplant, data=jasa)

#see that it looks like receiving a transplant improves survival

#now let's do landmark prediction using 30 days as the landmark time

#first I make a variable splitting my sample
groupb  =  jasa[jasa$futime >= 30,]
groupb$transplant30 = 1*(groupb$transplant == 1 & groupb$wait.time < 30)

km = survfit(Surv(futime, fustat) ~ transplant30, data=groupb)
plot(km, col = c(1,2), lwd = 2, ylab = "Conditional survival probability", xlab = "Time")
legend("topright", lty = 1, col = c(1,2), lwd = 2, c("No transplant before 30 days", "Transplant before 30 days"))

survdiff(Surv(futime, fustat) ~ transplant30, data=groupb)
#no difference

#can do a cox model with group b
#note that surgery is something we have at baseline, it is an indicator of prior bypass surgery
my.model = coxph(Surv(futime, fustat) ~ transplant30 + age +surgery, data=groupb)
summary(my.model)

#now let's go back to the full dataset and model transplant as a time-varying covariate to examine the hazard

#we can use the tmerge function to set up the data the we need to
#first make a unique id
jasa2 = jasa
jasa2$id = 1:length(jasa[,1])
#tdc stands for time dependent covariate
#there is one person with a futime of 0 and the function will not take that
jasa2$futime[jasa2$futime == 0] = 0.01
jasa2 = tmerge(jasa2, jasa2, id = id, death=event(futime, fustat), transpl=tdc(wait.time))

#let's take a look, look at person 4 and 7
jasa2[1:10,c(5,8,9,10,15,16,17,18,19)]

#fit the cox model - notice how you have to give it the times

my.model = coxph(Surv(tstart, tstop, death) ~ transpl + age +surgery, data=jasa2)
summary(my.model)


