################################

install.packages("survival")
library(survival)
library(tdROC)
library(pROC)
library(medicaldata)
library(survival)
library(asaur)
library(dplyr)
#install.packages("timeROC")

library(timeROC)
library(twang)




mydata <- read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/hw2_braincancer-1.csv", header=TRUE,sep =",")





km = survfit(Surv(mydata$time, mydata$status) ~mydata$stereo)


summary(km)



time <- c(50, 27,81,36,75,50,48,75)
delta_i <- c(0,0,1,1,0,1,0,1)

df <- data.frame(time, delta_i)

print(df)



km = survfit(Surv(df$time, df$delta_i)~1)


summary(km)



#----

Y<- c(32, 34,31,29,32,30,28,26,29,25)
Z <- c(1,1,1,1,1,0,0,0,0,0)
P<-c(0.8,0.7,0.9,0.65,0.75,0.6,0.2,0.4,0.3,0.15)
mydata <- data.frame(Y, Z,P)

mydata$w<-ifelse(mydata$Z==1,1/P[mydata$Z==1],1/(1-P[mydata$Z==0]))
treat<-Y[mydata["Z"]==1]
treat_w<-mydata$w[mydata["Z"]==1]
control_w<-mydata$w[mydata["Z"]==0]
control<-Y[mydata["Z"]==0]
mydata$w
mtreat<-mean(treat)
mcontrol<-mean(control)

ate<-mtreat-mcontrol

ate

table(mydata$Y, mydata$Z)
summary(glm( mydata$Y ~mydata$Z))



# ATE NO WEIGHTING -> 4

treat_w*treat
control_w*control
ate_w<- sum(treat_w*treat)/sum(treat_w)-sum(control_w*control)/sum(control_w)

ate_w
ate-ate_w


#----

data2<-read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/final_nafld.csv", header=TRUE,sep =",")

data2
my.model7 = coxph(Surv( futime,status) ~ age+ male+bmi, data = data2)
summary(my.model7)

base.haz.estimate = basehaz(my.model7,centered=FALSE)

t= 3500
base.haz.estimate.35 = approx(base.haz.estimate$time, base.haz.estimate$hazard, xout=t, method = "constant")$y


new_data <- data.frame(45,1,27.5) 
colnames(new_data) <- c('age','male','bmi') 
new_data

#calculate beta*Z
beta.z = predict(my.model7, newdata = new_data, type = "lp", reference = "zero")

#calculate survival
pred.surv = exp(-base.haz.estimate.35*exp(beta.z))
pred.surv


#-----------------------------------
data3<-read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/final_green.csv", header=TRUE,sep =",")
rev_psf_treatment<-data3$rev_psf[data3["green_rating"]==1]
rev_psf_control<-data3$rev_psf[data3["green_rating"]==0]
ate_3<-mean(rev_psf_treatment)-mean(rev_psf_control)
ate_3
#size, stories, age, renovated, and class_a







set.seed(1)

ps.lind = ps(green_rating~ size+stories+age+renovated+class_a, data = data3, verbose = FALSE, estimand = "ATE", stop.method = "es.mean")





#The stop.method argument specifies a set (or sets) of rules and measures for assessing the balance, or equivalence, established on the pretreatment covariates of the weighted treatment and control groups. The ps function selects the optimal number of GBM iterations to mini- mize the differences between the treatment and control groups as measured by the rules of the given stop.method object. The package includes four built-in stop.method objects. They are es.mean, es.max, ks.mean, and ks.max. The four stopping rules are defined by two components: a balance metric for covariates and rule for summarizing across covariates. The balance metric summarizes the difference between two univariate distributions of a single pre-treatment variable (e.g., age). The default stopping rules in twang use two balance metrics: absolute standardized bias (also referred to as the absolute standardized mean difference or the Effect Size) and the Kolmogorov-Smirnov (KS) statistic. The stopping rule use two different rules for summarizing across covariates: the mean of the covariate balance metrics (“mean”) or the maximum of the balance metrics (“max”). The first piece of the stopping rule name identifies the balance metric (ES or KS) and the second piece specifies the method for summarizing across balance metrics. For instance, es.mean uses the effect size or the absolute standardized bias and summarizes across variables with the mean and the ks.max uses the KS statistics to assess balances and summarizes using the maximum across variables and the other two stopping rules use the re- maining two combinations of balance metrics and summary statistics. 

#Before you do anything with the outcome, you MUST check balance. There are several diagnostic tools availabe in twang. 

#let's look at the balance table
#missing values are NOT thrown out. twang will attempt to construct weights that also balance of rates of missingness in the two groups. In this case, the bal.table() will have an extra row for each variable that has missing entries.

bal.table(ps.lind)

#tx.mn, ct.mn The treatment means and the control means for each of the variables. The unweighted table (unw) shows the unweighted means. For each stopping rule the means are weighted using weights corresponding to the gbm model selected by ps() using the stopping rule. 

#tx.sd, ct.sd The propensity score weighted treatment and control groups’ standard deviations for each of the variables. The unweighted table (unw) shows the unweighted standard deviations 
#std.eff.sz The standardized effect size, defined as the treatment group mean minus the control group mean divided by the treatment group standard deviation if estimand = "ATT" or divided by the pooled sample (treatment and control) standard deviation if estimand = "ATE". 
#stat, p Depending on whether the variable is continuous or categorical, stat is a t-statistic or a χ2 statistic. p is the associated p-value 
#ks, ks.pval The Kolmogorov-Smirnov test statistic and its associated p-value. P-values for the KS statistics are either derived from Monte Carlo simulations or analytic approximations, depending on the specifications made in the perm.test.iters argument of the ps function. For categorical variables this is just the χ2 test p-value 

#lots of nifty plots, this is my favorite

plot(ps.lind, plots=3)

#you can also see how the increase in number of iterations/trees associates with the balance measure (here, mean ES). Note that as with most machine learning methods, more iterations does not necessarily lead to better "fit". 

plot(ps.lind)

#let's look at the summary function
#The summary() method for ps objects  offers a compact summary of the sample sizes of the  groups and the balance measures. (note we didn't use KS)

summary(ps.lind)

library(survey)

#The get.weights() function extracts the propensity score weights from a ps object. Those weights may then be used as case weights in a svydesign object. 
#the svydesign needs to be made to be given to the survey functions later. The svydesign function from the survey package creates an object that stores the dataset along with design information needed for analyses. 

data3$w = get.weights(ps.lind, stop.method = "es.mean")
data3$rev_psf = 1*data3$rev_psf
design.ps = svydesign(ids=~1, weights = ~w, data=data3)

summary(svyglm(rev_psf~green_rating, design=design.ps))


summary(ps.lind$gbm.obj, plot = TRUE)
ate_3
ate_3-1.6821
#-----------------------------------
data4<-read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/final_green.csv", header=TRUE,sep =",")

total.n = length(data4$CV.fold)
k=10
test.error.vector = vector(length = k)
for(i in 1:k){
  test<-data4[which(data4["CV.fold"] ==i),]
  training = data4[which(data4["CV.fold"] !=i),]
  
  #fit the model with the training data
  model = glm(rev_psf ~ Rent+size+stories+age+renovated+green_rating+class_a, data=training)
  #predict for the test data
  preds = predict(model, newdata = test, type = "response")
 
  test.error.vector[i] = mean((test$rev_psf - preds)^2)
}
mean(test.error.vector)

