library(tdROC)
library(pROC)
library(medicaldata)
library(survival)
library(asaur)
library(dplyr)
install.packages("timeROC")

library(timeROC)


mydata <- read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/hw5_missingdata.csv", header=TRUE,sep =",")
mydata<-mydata[,-which(names(mydata) == "X")]
mydata<-na.omit(mydata)
mydata

mean(mydata$X2)


linear_model<-lm(Y~X1+X2+X3,data=mydata)

summary(linear_model)


model1 = lmer(CD4 ~ obstime + (1 | patient), data = aids)
summary(model1)


mydata2 <- read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/hw5_missingdata.csv", header=TRUE,sep =",")
mydata2<-mydata2[,-which(names(mydata2) == "X")]



for(i in 1:ncol(mydata2)){
  mydata2[is.na(mydata2[,i]), i] <- mean(mydata2[,i], na.rm = TRUE)
}

mydata2

linear_model2<-lm(Y~X1+X2+X3,data=mydata2)

summary(linear_model2)



new_data <- data.frame(8,-2,1.5) 
colnames(new_data) <- c('X1','X2','X3') 
new_data

p_m1<-predict(linear_model, newdata = new_data, type = "response", reference = "zero")
p_m2<-predict(linear_model2, newdata = new_data, type = "response", reference = "zero")

diff<-abs(p_m2-p_m1)
diff



library(tidyverse)
library(labelled)   # labeling data
library(rstatix)    # summary statistics
library(ggpubr)     # convenient summary statistics and plots
library(GGally)     # advanced plot
library(car)        # useful for anova/wald test
library(Epi)        # easy getting CI for model coef/pred
install.packages("lme4", type = "source")
install.packages("Matrix", type = "source")

library(lmerTest)   # test for linear mixed-effects models
library(emmeans)    # marginal means
library(multcomp)   # CI for linear combinations of model coef
library(geepack)    # generalized estimating equations
library(ggeffects)  # marginal effects, adjusted predictions
library(gt)         # nice tables
library(dplyr)
tools::package_dependencies("Matrix", which = "LinkingTo", reverse = TRUE)[[1L]]
install.packages("lme4", type = "source")


oo <- options(repos = "https://cran.r-project.org/")
install.packages("Matrix")
install.packages("lme4")
options(oo)

library(lme4)       # linear mixed-effects models
library(car)


mydata3 <- read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/hw5_aids.csv", header=TRUE,sep =",")
head(mydata3)

model3 = lmer(CD4 ~ obstime + (1 | patient)+AZT+AZT*obstime, data = mydata3)
summary(model3)

Anova(model3)

library(geepack)    # generalized estimating equations
library(ggeffects)  # marginal effects, adjusted predictions
gee_aids <- geeglm(CD4 ~ obstime*AZT+AZT+obstime, data = mydata3, id = patient, corstr = "exchangeable")
summary(gee_aids)

