############################################

### Observational Studies ###

############################################

install.packages("twang")
library(twang)


############################################
### lindner dataset ###
############################################


#We will use the lindner dataset
#The lindner data contain data on 996 patients treated at the Lindner Center, Christ Hospital, Cincinnati in 1997. Patients received a Percutaneous Coronary Intervention (PCI). 
#The outcome is binary: sixMonthSurvive, whether patients survived to six months post treatment [denoted by TRUE] or did not survive to six months [FALSE]
#The treatment variable is abcix, where 0 indicates PCI treatment and 1 indicates standard PCI treatment and additional treatment in some form with abciximab (a blood thinner). 
#Covariates include acutemi, 1 indicating a recent acute myocardial infarction and 0 not; ejecfrac for the left ventricle ejection fraction, a percentage from 0 to 90; ves1proc giving the number of vessels (0 to 5) involved in the initial PCI; stent with 1 indicating coronary stent inserted, 0 not; diabetic where 1 indicates that the patient has been diagnosed with diabetes, 0 not; height in centimeters and female coding the sex of the patent, 1 for female, 0 for male.

data(lindner)
?lindner
head(lindner)

#naive ATE, we see a treatment effect

table(lindner$sixMonthSurvive, lindner$abcix)
summary(glm(lindner$sixMonthSurvive~lindner$abcix, family = "binomial"))


#set the seed because there is a stochastic component of gbm
set.seed(1)

ps.lind = ps(abcix~stent + height + female + diabetic + acutemi + ejecfrac + ves1proc, data = lindner, verbose = FALSE, estimand = "ATE", stop.method = "es.mean")

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

#what is the ESS?
#In general, weighted means can have greater sampling variance than unweighted means from a sample of equal size. The effective sample size (ESS) of the weighted comparison group captures this increase in variance. The ESS is approximately the number of observations from a simple random sample that yields an estimate with sampling variation equal to the sampling variation obtained with the weighted comparison observations. 
#In essence, you pay a price for using weights. This concept comes in handy when you are thinking about calculating power for a study.

#You can also look at summary of the GBM OBJECT to take a look at the relative influence of each variable in the propensity score model. 

summary(ps.lind$gbm.obj, plot = TRUE)

#Now we are ready to analyze the outcome. They survey package is useful here as its statistical methods account for the weights when computing standard error estimates. 
install.packages("survey")
library(survey)

#The get.weights() function extracts the propensity score weights from a ps object. Those weights may then be used as case weights in a svydesign object. 
#the svydesign needs to be made to be given to the survey functions later. The svydesign function from the survey package creates an object that stores the dataset along with design information needed for analyses. 

lindner$w = get.weights(ps.lind, stop.method = "es.mean")
lindner$sixMonthSurvive = 1*lindner$sixMonthSurvive
design.ps = svydesign(ids=~1, weights = ~w, data=lindner)

summary(svyglm(sixMonthSurvive~abcix, family = "binomial", design=design.ps))
#compare to
summary(glm(lindner$sixMonthSurvive~lindner$abcix, family = "binomial"))

#this is an example where the result is very similar, even though there is selection

############################################
### lalonde dataset ###
############################################

#Let's look at another dataset, this comes from economics
#For the lalonde dataset, the variable treat is the 0/1 treatment indicator, 1 indicates “treatment”by being part of the National Supported Work Demonstration and 0 indicates“comparison” cases drawn from the Current Population Survey. In order to estimate a treatment effect for this demonstration program that is unbiased by pretreatment group differences on other observed covariates, we include these covariates in a propensity score model of treatment assignment: age, education, black, Hispanic, having no degree, married, earnings in 1974 (pretreatment), and earnings in 1975 (pretreatment). 
#treatment started in 1975, outcome will be earnings in 1978
#The National Supported Work Demonstration, a program at 15 sites around the country, was designed to test whether and to what extent 12 to 18 months of employment in a supportive but performance-oriented environment would equip hard-to-employ people to get and hold normal, unsubsidized jobs. The program concentrated on women who had been receiving Aid to Families with Dependent Children (AFDC) for many years, ex-addicts, ex-offenders, and young school dropouts, often with criminal records or histories of delinquency.
#VERY IMPORTANTLY, the causal effect of interest here is the ATT, not the ATE because it is not the case that this is a treatment that would be offered to the whole population. 

data(lalonde)
?lalonde
head(lalonde)

#let's fit the ps model
#set.seed(1)

ps.la= ps(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, data = lalonde, verbose = FALSE, estimand = "ATT", stop.method = "es.mean")

#### Class activity ####

1. Investigate the balance - look at the balance table, examine the balance figure. 

2. Was there balance before weighting? Is there sufficient balance after weighting?

3. Which variable had the most imbalance before weighting? 

4. Which variable has the most influence in the propensity score model?

5. Now fit a propensity score weighted model. What is the conclusion?


1. Investigate the balance - look at the balance table, examine the balance figure. 

bal.table(ps.la)
plot(ps.lind, plots=3)

2. Was there balance before weighting? Is there sufficient balance after weighting?

#No
#Pretty good, yes. 

3. Which variable had the most imbalance before weighting? 
#black

4. Which variable has the most influence in the propensity score model?

#black
summary(ps.la$gbm.obj, plot = TRUE)

5. Now fit a propensity score weighted model examining the treatment effect on the outcome. What is the conclusion?

lalonde$w = get.weights(ps.la, stop.method = "es.mean")
design.ps = svydesign(ids=~1, weights = ~w, data=lalonde)

summary(svyglm(re78~treat, design=design.ps))




############################################
### Additional ###
############################################

#Let's compare this to fitting a regular linear regression model with covariate adjustment
summary(lm(re78~treat + age + educ + black + hispan + nodegree + married + re74 + re75, data=lalonde))
#significant treatment effect

############################################
### ATT weights ###
############################################

#Importantly for ATT weights, those in the treated group get a weight of 1. 
#Those in the control group get a weight of p(z=1|x)/(1-p(z=1|x)) because we are trying to make them look like the treated group.
#see here

lalonde$w

#What if we just estimated the weights using logistic regression?

ps.logit = glm(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, data=lalonde, family = "binomial")
my.preds = predict(ps.logit, type = "response")
lalonde$w.logit = rep(1,nrow(lalonde))
lalonde$w.logit[lalonde$treat==0] = my.preds[lalonde$treat==0]/(1-my.preds[lalonde$treat==0])

#dx.wts() from the twang package diagnoses the balance for an arbitrary set of weights producing a balance table. This function requires the user to specify the estimand argument in order to perform the appropriate calculations relative to the target group on which we are drawing inferences. 

bal.logit <- dx.wts(x = lalonde$w.logit, data=lalonde,vars=c("age","educ","black","hispan","nodegree","married","re74","re75"), treat.var="treat", perm.test.iters=0, estimand = "ATT")
bal.table(bal.logit)
		
		
#let's look at the outcome model
		
design.ps = svydesign(ids=~1, weights = ~w.logit, data=lalonde)
		
summary(svyglm(re78~treat, design=design.ps))
		
#similar conclusion
		


