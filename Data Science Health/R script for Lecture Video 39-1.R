############################################

### Longitudinal Data###

############################################

#this is a collection of packages. This may take a while to install.
install.packages(c("tidyverse", "dplyr","labelled","rstatix","ggpubr","GGally","car","Epi","lme4","lmerTest","emmeans","multcomp","geepack","ggeffects","gt"))



library(tidyverse)
library(labelled)   # labeling data
library(rstatix)    # summary statistics
library(ggpubr)     # convenient summary statistics and plots
library(GGally)     # advanced plot
library(car)        # useful for anova/wald test
library(Epi)        # easy getting CI for model coef/pred
library(lme4)       # linear mixed-effects models
library(lmerTest)   # test for linear mixed-effects models
library(emmeans)    # marginal means
library(multcomp)   # CI for linear combinations of model coef
library(geepack)    # generalized estimating equations
library(ggeffects)  # marginal effects, adjusted predictions
library(gt)         # nice tables
library(dplyr)

install.packages("JM")
library(JM)

############################################

### Joint modeling ###

############################################

#note one row per measurement
head(aids)

#note one row per person
head(aids.id)

#random intercept and slope model
#lme is a different function that can be used to fit linear mixed models

lmeFit <- lme(CD4 ~ obstime*drug, random = ~ obstime | patient, data = aids)

summary(lmeFit)

#cox model, notice only drug is in the model, x=TRUE just means to save some things about the model in the object, because the jointModel function will need it

coxFit <- coxph(Surv(Time, death) ~ drug, data = aids.id, x = TRUE)

summary(coxFit)

#joint model

jointFit <- jointModel(lmeFit, coxFit, timeVar = "obstime", method = "spline-PH-GH")
summary(jointFit)

#numerical integration involved, GH = standard Gauss-Hermite quadrature rule
#Available options for method are: "piecewise-PH-GH": PH model with piecewise-constant baseline hazard, "spline-PH-GH": PH model with B-spline-approximated log baseline hazard, "weibull-PH-GH": PH model with Weibull baseline hazard, "weibull-AFT-GH": AFT model with Weibull baseline hazard, "Cox-PH-GH": PH model with unspecified baseline hazard


#no evidence of a treatment effect on CD4
#there is evidence of a treatment effect on survival, and there is a strong association between CD4 and survival
#if you did separate models, you would come to the same conclusion, but the parameters estimates would be different. 

Output:
Call:
jointModel(lmeObject = lmeFit, survObject = coxFit, timeVar = "obstime", 
    method = "spline-PH-GH")

Data Descriptives:
Longitudinal Process		Event Process
Number of Observations: 1405	Number of Events: 188 (40.3%)
Number of Groups: 467

Joint Model Summary:
Longitudinal Process: Linear mixed-effects model
Event Process: Relative risk model with spline-approximated
		baseline risk function
Parameterization: Time-dependent 

 log.Lik  AIC  BIC
   -4334 8707 8785

Variance Components:
             StdDev    Corr
(Intercept)  4.5219  (Intr)
obstime      0.1702 -0.0520
Residual     1.8757        

Coefficients:
Longitudinal Process
                  Value Std.Err
(Intercept)      7.0709  0.1729
obstime         -0.1813  0.0218
drugddI          0.3024  0.2695
obstime:drugddI  0.0039  0.0305
                z-value p-value
(Intercept)      40.902 <0.0001
obstime          -8.310 <0.0001
drugddI           1.122  0.2619
obstime:drugddI   0.128  0.8982

Event Process
         Value Std.Err z-value
drugddI  0.356  0.1583   2.250
Assoct  -0.300  0.0385  -7.789
bs1     -3.803  0.5720  -6.648
bs2     -1.069  0.5814  -1.838
bs3     -3.881  0.6271  -6.190
bs4     -1.231  0.3767  -3.268
bs5     -2.705  0.4489  -6.026
bs6     -2.152  0.7165  -3.004
bs7     -2.295  1.3577  -1.690
bs8     -2.142  2.5906  -0.827
bs9     -4.864  5.7627  -0.844
        p-value
drugddI  0.0244
Assoct  <0.0001
bs1     <0.0001
bs2      0.0661
bs3     <0.0001
bs4      0.0011
bs5     <0.0001
bs6      0.0027
bs7      0.0910
bs8      0.4083
bs9      0.3986

Integration:
method: Gauss-Hermite
quadrature points: 15 

Optimization:
Convergence: 0 
