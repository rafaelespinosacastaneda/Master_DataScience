###################################################
## COMPUTING DEMO:  Hidden Markov Models
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
## Last updated: September 1, 2021
###################################################

# load libraries
library(tidyverse)
library(gridExtra)
library(xts)
library(depmixS4)
library(gamlss.data)


#----------------------- Question a -------------------
data(polio)

months<-seq(as.Date("1970-01-01"), as.Date("1983-12-01"), by="months")


data_complete<-data.frame("Count"=polio,"Time"=months)

###################################################
# Fit a 2-state Poisson HMM to the earthquake data from
# "Hidden Markov Models for Time Series" by 
# Zucchini, MacDonald, and Langrock (2016)

# get the data
#quakes <- read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")$V2 

#year <- 1900:2006

#eq_dat <- data.frame("count" = quakes,"year" = year)

# create the HMM (non-stationary)
polio_mod <- depmix(Count ~ 1,
                  data = data_complete,
                  family = poisson(),
                  nstates = 2,
                  ntimes = length(months))

# fit the model using an EM algorithm
polio_fit <- fit(polio_mod)



summary(polio_fit)
print(polio_fit)


print("The transition matrix values are")

print(paste(" From State 1 to State 1 ",getpars(polio_fit)[3]))
print(paste(" From State 1 to State 2 ",getpars(polio_fit)[4]))
print(paste(" From State 2 to State 1 ",getpars(polio_fit)[5]))
print(paste(" From State 2 to State 2 ",getpars(polio_fit)[6]))



print("The AIC is")
AIC(polio_fit)




print("The parameters lambda values are")
exp(getpars(polio_fit)[7:8])



# get the posterior distribution of the states
data_complete$states <- posterior(polio_fit, type = "viterbi")$state 


# plot the earthquake data with the most likely state

ggplot(data_complete,
       aes(x = Time, y = Count)) +
  geom_point(size = .7, color = "darkblue") +
  geom_line(color = "darkblue")  +
  geom_text(label=data_complete$states,nudge_y =0.45)+
  ggtitle("Polio cases over time") 


#----------------------- Question b -------------------



count_1<-0
count_2<-0

for (i in 1:length(data_complete$Count))
{
  
  if (data_complete$states[i]==1 &&data_complete$Count[i]>=4)
  {
    count_1<-count_1+1
  }
  
  if (data_complete$states[i]==2 &&data_complete$Count[i]>=4)
  {
    count_2<-count_2+1
  }
}


print("The lambdas represent the mean number of cases of people with polio per month for each state.")

if(count_1>count_2)
{print("It is more probable to be in state 1 given that number of observed patients with polio is greater or equal to 4")
  }

if(count_2>count_1)
{print("It is more probable to be in state 2 given that number of observed patients with polio is greater or equal to 4")
}


print("From the transition matrix we can see that is much more probable to stay in state 2 than going to state 1")
print("Hence, C_(t+1)=2")

two_cases_prob<-1-ppois(2, lambda = exp(getpars(polio_fit)[8]))
zero_cases_prob<-dpois(2, lambda = exp(getpars(polio_fit)[8]))

if(two_cases_prob>zero_cases_prob)
{print("It is more likely to observe 2 or more cases in t+1 given that at t we observed at least 4")
}

if(zero_cases_prob>two_cases_prob)
{print("It is more likely to observe zero cases  in t+1 given that at t we observed at least 4")
}

print("The model is consistent with basic knowledge of diasease dynamics. If this month we observed")
print("more than four infected people, it is likely that those four will spread the disease to other people or ")
print("keep infected at least for another month. Hence, it is more probable that we observe  at least two cases") 
print("with the desease than no observing it ")

#----------------------- Question c -------------------


months<-seq(as.Date("1970-01-01"), as.Date("1983-12-01"), by="months")


data_complete<-data.frame("Count"=polio,"Time"=months)
###################################################
# Fit a 3-state Poisson HMM to the earthquake data from
# "Hidden Markov Models for Time Series" by 
# Zucchini, MacDonald, and Langrock (2016)

# get the data
#quakes <- read.table("http://www.hmms-for-time-series.de/second/data/earthquakes.txt")$V2 

#year <- 1900:2006

#eq_dat <- data.frame("count" = quakes,"year" = year)

# create the HMM (non-stationary)
polio_mod2 <- depmix(Count ~ 1,
                    data = data_complete,
                    family = poisson(),
                    nstates = 3,
                    ntimes = length(months))

# fit the model using an EM algorithm
polio_fit2 <- fit(polio_mod2)

summary(polio_fit2)

print(polio_fit2)

# get estimate of lambda
getpars(polio_fit2)

exp(getpars(polio_fit2)[7:8])



# get the posterior distribution of the states
data_complete$states2 <- posterior(polio_fit2, type = "viterbi")$state 


# plot the earthquake data with the most likely state

ggplot(data_complete,
       aes(x = Time, y = Count)) +
  geom_point(size = .7, color = "darkblue") +
  geom_line(color = "darkblue")  +
  geom_text(label=data_complete$states2,nudge_y =0.45)+
  ggtitle("Polio cases over time") 




print("The AIC is")
AIC(polio_fit2)



print("The parameters lambda values are")
getpars(polio_fit2)[13:15]


print(polio_fit2)

summary(polio_fit2)

print("The transition matrix values are")

print(paste(" From State 1 to State 1 ",getpars(polio_fit2)[4]))
print(paste(" From State 1 to State 2 ",getpars(polio_fit2)[5]))
print(paste(" From State 1 to State 3 ",getpars(polio_fit2)[6]))
print(paste(" From State 2 to State 1 ",getpars(polio_fit2)[7]))
print(paste(" From State 2 to State 2 ",getpars(polio_fit2)[8]))
print(paste(" From State 2 to State 3 ",getpars(polio_fit2)[9]))
print(paste(" From State 3 to State 1 ",getpars(polio_fit2)[10]))
print(paste(" From State 3 to State 2 ",getpars(polio_fit2)[11]))
print(paste(" From State 3 to State 3 ",getpars(polio_fit2)[12]))






