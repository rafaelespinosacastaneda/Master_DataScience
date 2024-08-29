################################
### Biostatistical Methods ##
################################


install.packages("pROC")
library(pROC)
library(dplyr)


library(survival)
library(asaur)


######################## 
# Validation

#let's again run our code where we build the model and evaluate it in the same dataset

rm(mydata) 
mydata <- read.table("/Users/rafa/Desktop/Master Austin/MAESTRIA_AUSTIN/DataScienceHealth/hw3_heart-1.csv", header=TRUE,sep =",")

mydata<-mutate(mydata, heart_disease = case_when(class >0 ~ 1, class <1 ~ 0))

mydata

model = glm(heart_disease ~ trestbps, family = "binomial", data=mydata)

preds = predict(model, type = "response")
roc_object = roc(mydata$heart_disease, preds, direction = "<")
roc_object$auc


model_1 = glm(heart_disease ~ age+trestbps+ chol, family = "binomial", data=mydata)


model_2 = glm(heart_disease ~age+trestbps+chol+ thalach, family = "binomial", data=mydata)

preds_1 = predict(model_1, type = "response")
roc_object_1 = roc(mydata$heart_disease, preds_1, direction = "<")


preds_2 = predict(model_2, type = "response")
roc_object_2 = roc(mydata$heart_disease, preds_2, direction = "<")
roc_object_2$auc-roc_object_1$auc



AIC(model_1)-AIC(model_2)



mydata<-mutate(mydata, sex_b = case_when(sex==TRUE ~ 1, sex==FALSE ~ 0))


mydata<-mutate(mydata, fbs_b = case_when(fbs==TRUE ~ 1, fbs==FALSE ~ 0))

mydata<-mutate(mydata, exang_b = case_when(exang==TRUE ~ 1, exang==FALSE ~ 0))

mydata




#let's now do a single split
total.n = length(mydata$age)

#now let's do LOOCV
#here, the AUC for 1 observation actually doesn't make sense, so let's look at the brier score
test.error.vector = vector(length = total.n)
for(i in 1:total.n){
  training.data = mydata[-i,]
  test.data = mydata[i,]
  #fit the model with the training data
  model_all = glm(heart_disease ~ age+sex_b+cp+trestbps+chol+fbs_b+restecg+thalach+exang_b+oldpeak+slope+ca+thal, family = "binomial", data=training.data)
  
  #predict for the test data
  preds = predict(model_all, newdata = test.data, type = "response")
  test.error.vector[i] = (preds - test.data$heart_disease)^2
}
mean(test.error.vector)



