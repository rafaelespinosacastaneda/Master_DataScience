library(tidyverse)
library(reshape)
library(gridExtra)

##################################
#Calculating power
##################################

#let's do a t-test
sd=1
n=10
mydata = data.frame(Control = rnorm(n,mean=1,sd=sd), Treat = rnorm(n,mean=2,sd=sd))
tt = t.test(mydata$Control, mydata$Treat)
tt$p.value

#now let's do it many times
n.reps = 100
hold.p = vector(length = n.reps)
for(i in 1:n.reps){
	mydata = data.frame(x = rnorm(n,mean=1,sd=sd), y = rnorm(n,mean=2,sd=sd))
	hold.p[i] =  t.test(mydata$x, mydata$y)$p.value
}
#how many times would you have rejected the null hypothesis?
power = mean(hold.p<0.05)
power

#now change n
n=20

#now change n.reps to 1000, 10000

#what is the effect size of this difference?
effect.size = (2-1)/sd

library(pwr)
#what is my power to detect this effect size assuming this sample size?
pwr.t2n.test(n1=20,n2=20, d=1)

#what is the minimum effect size I can detect with 80% power?
pwr.t2n.test(n1=20,n2=20, power=0.80)

#you can't leave both n1 and n2 empty, but let's say n1 is fixed at 20, how many n2 do you need to detect effect size of 1 with 80% power?
pwr.t2n.test(n1=20,d=1, power=0.80)

#you can use pwr.t.test if you can assume the n's are equal
pwr.t.test(d=1,power=0.80)


##################################
#Class activity
##################################

#1. Let's say you are designing a study and your sample size is fixed at n1=30, n2=25. You have found a previous pilot study similar to what you are testing and they report a difference in means of 2 and a pooled standard deviation of 3. What is the effect size reported in this pilot study? What is your power to detect that effect size or greater?


#2. Let's say you are designing a study and your sample size is NOT fixed, but you need to be able to detect a small effect size to justify carrying out your study in the first place. What sample size in each group do you need to ensure your study has 90% power?


#1. Let's say you are designing a study and your sample size is fixed at n1=30, n2=25. You have found a previous pilot study similar to what you are testing and they report a difference in means of 2 and a pooled standard deviation of 3. What is the effect size reported in this pilot study? What is your power to detect that effect size or greater?


ES = 2/3
pwr.t2n.test(n1=30,n2=25, d=ES)


#2. Let's say you are designing a study and your sample size is NOT fixed, but you need to be able to detect a small effect size to justify carrying out your study in the first place. What sample size in each group do you need to ensure your study has 90% power?

pwr.t.test(power=0.90, d=0.2)



##################################
#Other power calculations
##################################

?pwr







