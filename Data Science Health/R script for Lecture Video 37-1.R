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
#This is now starting to use the tidyverse. If you are not familiar with the tidyverse, see "R for Data Science" https://r4ds.had.co.nz

#Load data
load(url("http://alecri.github.io/downloads/data/dental.RData"))

#look at the data
dental

# See that the data is presented in a wide format where repeated measurements are contained in separate variables (only one row for each individual).
#Oftentimes, a longitudinal format, where repeated measurements are reported as separate rows, is preferable for the analysis. 
#The pivot_longer function is useful for restruring a data from a wide to a long format:

###########################################
### Making the dataset ###
############################################

#let's go step by step
#"piping" is a way to do this in a MUCH CLEANER WAY. I will not use piping here as I think it's very difficult to follow unless you are familiar with it. 
#first make multiple rows per person
dental_long = pivot_longer(dental, cols = starts_with("y"), names_to = "measurement", values_to = "distance") 
dental_long
#pull ages to a numeric column
dental_long = mutate(dental_long, age = parse_number(measurement), measurement = fct_inorder(paste("Measure at age", age)))
dental_long
#make nice labels, this will be helpful when plotting
dental_long = set_variable_labels(dental_long, age = "Age of the child at measurement", measurement = "Label for time measurement", distance = "Measurement")
dental_long


###########################################
### Interaction model: GEE ###
############################################

#the function is geeglm
# here, we will fit the interaction model (parallel to where we left off using mixed models) with linear age
#family = gaussian 
#correlation structure: a character string specifying the correlation structure. 
#The following are permitted: '"independence"', '"exchangeable"', '"ar1"', '"unstructured"' and '"userdefined"'
#exchangeable is the same as the compound symmetry we have learned previously


gee_inter <- geeglm(distance ~ age + sex + sex*age, data = dental_long, id = id, corstr = "exchangeable")
summary(gee_inter)

#notice all the pieces it estimated, the regression coefficients but also the scale and alpha

#can get confidence intervels for parameter
tidy(gee_inter, conf.int = TRUE)


#let's plot

pred_geeinter <- ggpredict(gee_inter, terms = c("age", "sex"))
ggplot(pred_geeinter, aes(x, predicted, col = group)) + 
  geom_line() +
   labs(x = "Age, years", y = "Dental growth, mm", col = "Sex")


#let's compare to the random effects model. The estimates for the coefficients are the same - this is because
#this is a linear model, but notice that the inference is NOT the same. Because you are handling the 
#repeated measures correlation differently. The interpretation here is the same only because it is linear.
#the intepretation is not the same in non-linear models.

lin_agecsexinterr = lmer(distance ~ age*sex + (age | id), data = dental_long)
summary(lin_agecsexinterr)
ci.lin(lin_agecsexinterr)
tidy(gee_inter, conf.int = TRUE)

###########################################
### Class activity ###
############################################

#Use the "aids" dataset in the JM package
install.packages("JM")
library(JM)
aids[1:10,]

#do ?aids to take a look at the variables

#1. Focusing on the CD4 count measured, plot a spaghetti plot showing the CD4 trajectories over time (obstime).

#Use a random effects model to answer these questions. Use a random intercepts model.

#2. Is there a time effect? Does CD4 change over time?

#3. Is there a difference in CD4 trajectory by drug group? Is there a difference in CD4 trajectory by whether or not the patient had a previous opportunistic infection?

#4. Does the CD4 trajectory over time differ by drug? Or by whether or not the patient had a previous opportunistic infection?
 
#5 Fit the model from #3 but using a marginal model i.e. GEE. Are the coefficients different? Why?

###########################################
### Class activity ###
############################################

#Use the "aids" dataset in the JM package
install.packages("JM")
library(JM)
aids[1:10,]

#do ?aids to take a look at the variables

#1. Focusing on the CD4 count measured, plot a spaghetti plot showing the CD4 trajectories over time.

ggplot(aids, aes(obstime, CD4, col = factor(patient))) +
  geom_line() +
  labs(x = "Observation time", y = "CD4 count", col = "Patient ID") +
  guides(col = guide_legend(nrow = 3)) + theme(legend.position="none")

#Use a random effects model to answer these questions. Use a random intercepts model.

#2. Is there a time effect? Does CD4 change over time?

model1 = lmer(CD4 ~ obstime + (1 | patient), data = aids)
summary(model1)

#3. Is there a difference in CD4 trajectory by drug group? Is there a difference in CD4 trajectory by whether or not the patient had a previous opportunistic infection?

model2 = lmer(CD4 ~ obstime + drug + (1 | patient), data = aids)
summary(model2)

model3 = lmer(CD4 ~ obstime + prevOI + (1 | patient), data = aids)
summary(model3)

#4. Does the CD4 trajectory over time differ by drug? Or by whether or not the patient had a previous opportunistic infection?

 model4 = lmer(CD4 ~ obstime*drug + (1 | patient), data = aids)
summary(model4)
 
 model4 = lmer(CD4 ~ obstime*prevOI + (1 | patient), data = aids)
summary(model4)

#5 Fit the model from #3 but using a marginal model i.e. GEE. Are the coefficients different? Why?

gee_aids <- geeglm(CD4 ~ obstime*prevOI, data = aids, id = patient, corstr = "exchangeable")
summary(gee_aids)


#different, because of missing data, handled differently


