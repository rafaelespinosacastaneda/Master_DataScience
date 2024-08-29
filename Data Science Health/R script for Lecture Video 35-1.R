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
### Time effect ###
############################################
#the function we use for a mixed model is lmer
#the specification looks almost like a regular regression with just an intercept term but 
#the +(1 | id) part is the part that tells it you want random intercepts. 
#It is saying that each id gets its own random intercept.
## This is the random intercepts model with fixed effects = indicators for age
lin_age = lmer(distance ~ measurement + (1 | id), data = dental_long)
summary(lin_age)

#note Satterthwaite's method and the use of REML
#notice the random effects and fixed effects portions of the results

#The mean dental distance at age 8 years (baseline) is 22.18 mm.
#The difference between the mean responses of 10 and 8 years-old children is 0.98 mm.
#The difference between the mean responses of 12 and 8 years-old children is 2.46 mm.
#The difference between the mean responses of 14 and 8 years-old children is 3.91 mm.

#you can get a confidence interval for the fixed effect terms
ci.lin(lin_age)

#you can test the variance components; this is testing whether there is significant evidence 
#that you need the random intercepts. If this was not significant, you could perhaps justify a #model only fixed effects.
ranova(lin_age)

#The emmeans function can be usefull for computing marginal means with corresponding confidence intervals.
tidy(emmeans(lin_age, "measurement"), conf.int = TRUE)

#now let's test if all beta's are 0 which means the distance is constant over time 
Anova(lin_age)

#Results from the Wald-test suggest that the trajectory of the mean response over time is not flat.

#We can show the estimated means at each age on top of the observed trajectories using the predict function. Here re.form = ~0 means no random effects. If you remove that part, you can see it puts a dot for each person.

dental_fit = bind_cols(dental_long, pred_age = predict(lin_age, re.form = ~ 0))
ggplot(dental_fit, aes(age, distance)) +
  geom_line(aes(group = factor(id))) +
  geom_point(aes(y = pred_age), col = "blue", size = 2) + 
  labs(x = "Age, years", y = "Dental growth, mm")

###########################################
### Group effect ###
############################################

#now add in sex
lin_agesex = lmer(distance ~ measurement + sex + (1 | id), data = dental_long)
summary(lin_agesex)

#can get CI
ci.lin(lin_agesex)

#The difference between mean dental distances of boys vs. girls is 2.3 (95% CI = 0.83-3.81) mm higher than girls, adjusting for time.
#The emmeans can be used for estimating the mean responses for different combinations of sex and time measurements.

tidy(emmeans(lin_agesex, c("measurement", "sex")), conf.int = TRUE)

#we can visualize this as:

dental_fit$pred_agesex = predict(lin_agesex, re.form = ~ 0)
ggplot(dental_fit, aes(age, distance)) +
  geom_line(aes(group = factor(id))) +
  geom_point(aes(y = pred_agesex, col = sex), size = 2) + 
  labs(x = "Age, years", y = "Dental growth, mm", col = "Sex")
  

###########################################
### Time and Group Interaction ###
############################################

  
lin_agesexinter = lmer(distance ~ measurement*sex + (1 | id), data = dental_long)
lin_agesexinter

#test interaction
#type 3: tests the effect of removing a term while leaving all of its higher-order interactions in place.
Anova(lin_agesexinter, type = 3)

#There is ~marginal evidence of an interaction between age and sex ( = 0.07).
#The marginal prediction for the combination of sex and time of measurement can be obtained as:

tidy(emmeans(lin_agesexinter, c("measurement", "sex")), conf.int = TRUE)

#visualization:

dental_fit$pred_agesexinter = predict(lin_agesexinter, re.form = ~ 0)
ggplot(dental_fit, aes(age, distance)) +
  geom_line(aes(group = factor(id))) +
  geom_point(aes(y = pred_agesexinter, col = sex), size = 2) + 
  labs(x = "Age, years", y = "Dental growth, mm", col = "Sex")

###########################################
### Linear Time ###
############################################

#linear age, only age in the model

lin_agec = lmer(distance ~ age + (1 | id), data = dental_long)
lin_agec

#linear with age, sex, and interaction

lin_agecsexinter = lmer(distance ~ sex*age + (1 | id), data = dental_long)
summary(lin_agecsexinter)

#visualization
#make a grid for plotting
pred_agecsexinter = expand.grid(
  age = seq(8, 14, .5),
  sex = levels(dental_long$sex))

#made on the predictions from the model
pred_agecsexinter = bind_cols(pred_agecsexinter,pred = predict(lin_agecsexinter, newdata = pred_agecsexinter, re.form = ~ 0))

ggplot(pred_agecsexinter, aes(age, pred, col = sex)) +
  geom_line() +
  labs(x = "Age, years", y = "Dental growth, mm", col = "Sex")

###########################################
### Subject-specific trajectories ###
############################################


#let's estimate the subject specific trajectories for persons 10 and 21
sid = c(10, 21)
pred.data =  expand.grid(
  age = seq(8, 14, .5),
  id = sid)
#notice that for indiv_pred, we do NOT have re.form = ~ 0, this means I am asking for the subject-level predictions

pred.data = bind_cols(pred.data,
    indiv_pred = predict(lin_agec, newdata = pred.data),
    marg_pred = predict(lin_agec, newdata = pred.data, re.form = ~ 0)) 

#this is just so I can add the points for the actual measurements

pred.data = left_join(pred.data, filter(dental_long, id %in% sid), by = c("id", "age"))

#plot with marginal mean and then subject-specific trajectories
ggplot(pred.data, aes(age, indiv_pred, group = id, col = factor(id))) +
  geom_line() +
  geom_point(aes(y = distance)) +
  geom_line(aes(y = marg_pred, col = "Marginal"), lwd = 1.5) +
  labs(x = "Age, years", y = "Dental growth, mm", col = "Curve")


###########################################
### Random slopes and intercepts ###
############################################

#notice now that we say (age | id) to specify random slopes and intercepts

lin_agecr = lmer(distance ~ age + (age | id), data = dental_long)
summary(lin_agecr)

#this pulls out just the random effects

VarCorr(lin_agecr)

#let's look at the time and group effect with an interaction model with linear time and random intercepts and random slopes
lin_agecsexinterr = lmer(distance ~ age*sex + (age | id), data = dental_long)
summary(lin_agecsexinterr)

#similar conclusions as random intercepts only

#let's plot these effects along with the predicted subject-specific trajectories
pred.data = expand.grid(
  age = seq(8, 14, .5),
  id = unique(dental_long$id))
pred.data =  left_join(pred.data, dplyr::select(dental, sex, id), by = "id")
pred.data = bind_cols(pred.data,
    indiv_pred = predict(lin_agecsexinterr, newdata = pred.data),
    marg_pred = predict(lin_agecsexinterr, newdata = pred.data, re.form = ~ 0)) 
ggplot(pred.data, aes(age, indiv_pred, group = id)) +
  geom_line(col = "grey") +
  geom_line(aes(y = marg_pred, col = sex), lwd = 1.5) +
  labs(x = "Age, measurements", y = "Mean Dental growth, mm")
  
  
#for reference, random slopes only
summary(lmer(distance ~ age*sex + (0+age | id), data = dental_long))

