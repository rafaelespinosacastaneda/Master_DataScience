library(tdROC)
library(pROC)
library(medicaldata)
library(survival)
library(asaur)
library(dplyr)
install.packages("timeROC")

library(timeROC)
library(twang)

mydata <- read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/hw6_wage.csv", header=TRUE,sep =",")
mydata
wage<-mydata["wage"]

treat<-wage[mydata["Dany"]==1]
control<-wage[mydata["Dany"]==0]

mtreat<-mean(treat)
mcontrol<-mean(control)

mtreat-mcontrol



table(mydata$wage, mydata$Dany)
summary(glm( mydata$wage ~mydata$Dany))



set.seed(1)

ps.lind = ps(Dany~ + sib_u+white + maemp + scht + qmab + qmab2 + qvab+qvab2+paed_u+maed_u+agepa+agema, data = mydata, verbose = FALSE, estimand = "ATE", stop.method = "es.mean")

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

mydata$w = get.weights(ps.lind, stop.method = "es.mean")
mydata$wage = 1*mydata$wage
design.ps = svydesign(ids=~1, weights = ~w, data=mydata)

summary(svyglm(wage~Dany, design=design.ps))


summary(ps.lind$gbm.obj, plot = TRUE)
