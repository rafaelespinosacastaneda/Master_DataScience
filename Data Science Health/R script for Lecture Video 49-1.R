library(tidyverse)
library(reshape)
library(gridExtra)

##################################
#Concept of Power
##################################


#we will look at the distributions of two groups
#things we will see
#when the SD is smaller, it is easier to tell them apart.
#when the means are farther apart, it is easier to tell them apart.
#when the sample size is larger, it is easier to tell them apart.

#when the SD is smaller, it is easier to tell them apart.
set.seed(5)
sd=0.2
n=100
mydata = data.frame(Control = rnorm(n,mean=1,sd=sd), Treat = rnorm(n,mean=2,sd=sd))
mydata = melt(mydata)
names(mydata)[1] = "Group"

p1=ggplot(mydata, aes(x=value, fill=Group)) + geom_density(alpha=.7) + xlim(-3,6)+labs(x="Outcome", y = "Density")
n=100
sd=2
mydata = data.frame(Control = rnorm(n,mean=1,sd=sd), Treat = rnorm(n,mean=2,sd=sd))
mydata = melt(mydata)
names(mydata)[1] = "Group"

p2=ggplot(mydata, aes(x=value, fill=Group)) + geom_density(alpha=.7) + xlim(-3,6)+labs(x="Outcome", y = "Density")
  
grid.arrange(p1, p2, nrow=1)

#when the means are farther apart, it is easier to tell them apart.
set.seed(5)
sd=1
n=100
mydata = data.frame(Control = rnorm(n,mean=1,sd=sd), Treat = rnorm(n,mean=1.5,sd=sd))
mydata = melt(mydata)
names(mydata)[1] = "Group"

p1=ggplot(mydata, aes(x=value, fill=Group)) + geom_density(alpha=.7) + xlim(-3,6)+labs(x="Outcome", y = "Density")
n=100
sd=1
mydata = data.frame(Control = rnorm(n,mean=1,sd=sd), Treat = rnorm(n,mean=4,sd=sd))
mydata = melt(mydata)
names(mydata)[1] = "Group"

p2=ggplot(mydata, aes(x=value, fill=Group)) + geom_density(alpha=.7) + xlim(-3,6)+labs(x="Outcome", y = "Density")
  
grid.arrange(p1, p2, nrow=1)

#when the sample size is larger, it is easier to tell them apart. Not as easy to see here, but the mean is estiamted with more precision.
set.seed(5)
sd=1
n=50
mydata = data.frame(Control = rnorm(n,mean=1,sd=sd), Treat = rnorm(n,mean=2,sd=sd))
mydata = melt(mydata)
names(mydata)[1] = "Group"

p1=ggplot(mydata, aes(x=value, fill=Group)) + geom_density(alpha=.7) + xlim(-3,6)+labs(x="Outcome", y = "Density")
n=5000
sd=1
mydata = data.frame(Control = rnorm(n,mean=1,sd=sd), Treat = rnorm(n,mean=2,sd=sd))
mydata = melt(mydata)
names(mydata)[1] = "Group"

p2=ggplot(mydata, aes(x=value, fill=Group)) + geom_density(alpha=.7) + xlim(-3,6)+labs(x="Outcome", y = "Density")
  
grid.arrange(p1, p2, nrow=1)

