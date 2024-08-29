################################
### Biostatistical Methods ##
################################


install.packages("pROC")
library(pROC)

######################## 
# Validation

#let's again run our code where we build the model and evaluate it in the same dataset

model = glm(outcome.binary ~ age + gender + s100b + ndka, family = "binomial", data=aSAH)
preds = predict(model, type = "response")
roc_object = roc(aSAH$outcome.binary, preds, direction = "<")
roc_object$auc
#0.8096
#this is the training error rate, or in the case of AUC, training performance

#let's now do a single split
total.n = length(aSAH$age)

#randomly split into training and test
training = sample(total.n, round(0.5*total.n), replace = F)
test = setdiff(1:total.n, training)
training.data = aSAH[training,]
test.data = aSAH[test,]

#fit the model with the training data
model = glm(outcome.binary ~ age + gender + s100b + ndka, family = "binomial", data=training.data)
#predict for the test data
preds = predict(model, newdata = test.data, type = "response")
roc_object = roc(test.data$outcome.binary, preds, direction = "<")
roc_object$auc
#this is an estimate of the test error rate
#now run it again
#see that it is variable

#now let's do LOOCV
#here, the AUC for 1 observation actually doesn't make sense, so let's look at the brier score
test.error.vector = vector(length = total.n)
for(i in 1:total.n){
	training.data = aSAH[-i,]
	test.data = aSAH[i,]
	#fit the model with the training data
	model = glm(outcome.binary ~ age + gender + s100b + ndka, family = "binomial", data=training.data)
	#predict for the test data
	preds = predict(model, newdata = test.data, type = "response")
	test.error.vector[i] = (preds - test.data$outcome.binary)^2
}
mean(test.error.vector)
#we should all get the same answer here

#now let's do 5-fold CV, back to AUC
k=5
test.error.vector = vector(length = k)
#randomly split my data into 5 groups. There are multiple ways to do this.
assign = sample(1:5, total.n, replace = T)
for(i in 1:k){
	test = which(assign == i)
	training = setdiff(1:total.n, test)
	training.data = aSAH[training,]
	test.data = aSAH[test,]

	#fit the model with the training data
	model = glm(outcome.binary ~ age + gender + s100b + ndka, family = "binomial", data=training.data)
	#predict for the test data
	preds = predict(model, newdata = test.data, type = "response")
	roc_object = roc(test.data$outcome.binary, preds, direction = "<")
	test.error.vector[i] = roc_object$auc
}
mean(test.error.vector)

#we will all get different answers
#try flipping training and test, so training uses 1/5 instead of 4/5
#see that model building has trouble

#now let's do generalized CV with h=0.75
M=100
h=0.75
test.error.vector = vector(length = M)
for(i in 1:M){
	training = sample(total.n, round(h*total.n), replace = F)
	test = setdiff(1:total.n, training)
	training.data = aSAH[training,]
	test.data = aSAH[test,]

	#fit the model with the training data
	model = glm(outcome.binary ~ age + gender + s100b + ndka, family = "binomial", data=training.data)
	#predict for the test data
	preds = predict(model, newdata = test.data, type = "response")
	roc_object = roc(test.data$outcome.binary, preds, direction = "<")
	test.error.vector[i] = roc_object$auc
}
mean(test.error.vector)

########################################
#class activity 

install.packages("medicaldata")
library(medicaldata)

#We will use the dataset opt. Take a look at the data.
#Randomized Clinical Trial on the Effect of Treatment of Maternal Periodontal Disease Can Reduce Preterm Birth Risk. 823 participants enrolled at 4 centers underwent stratified randomization, resulting in 413 women assigned to the treatment group and 410 to control. All participants were 13-16 weeks pregnant at time of randomization (baseline/visit 1) and went on to attend monthly follow-up visits defined as visits 2, 3, 4, and 5 corresponding to gestational age ranges of 17-20, 21-24, 25-28, and 29-32 weeks.
#https://cran.r-project.org/web/packages/medicaldata/medicaldata.pdf 

#For this exercise only:

opt.data = opt[!is.na(opt$Birthweight), ]
#WHO definition of low birthweight
opt.data$lowbw = 1*(opt.data$Birthweight < 2500)

#1. Fit a logistic regression model using the opt.data where the outcome is lowbw and the predictors are group, age, black, native american, asian, education, hypertension, diabetes. 

#2. Calculate the training error rate in terms of the AUC.

#3. Calculate the test error rate in terms of AUC using 5-fold CV.

#4. Pick a variable (biomarker). Assess the incremental value of the biomarker i.e., calculate the test error rate in terms of AUC using 5-fold CV with the biomarker now added to the model. Calculate the difference in the two CV AUCs. 

#1. Fit a logistic regression model using the opt.data where the outcome is lowbw and the predictors are group, age, black, native american, asian, education, hypertension, diabetes.

model = glm(lowbw ~ Group + Age + Black + Nat.Am + Asian + Education + Hypertension + Diabetes, family = "binomial", data=opt.data)

#2. Calculate the training error rate in terms of the AUC.

preds = predict(model, type = "response")
roc_object = roc(opt.data$lowbw, preds, direction = "<")
roc_object$auc

#note that if you have missingness in your data, you'll have to address it because predict will only give you predictions for non-missing rows

#3. Calculate the test error rate in terms of AUC using 5-fold CV.
total.n = length(opt.data$lowbw)
k=5
test.error.vector = vector(length = k)
assign = sample(1:5, total.n, replace = T)
for(i in 1:k){
	test = which(assign == i)
	training = setdiff(1:total.n, test)
	training.data = opt.data[training,]
	test.data = opt.data[test,]

	#fit the model with the training data
	model = glm(lowbw ~ Group + Age + Black + Nat.Am + Asian + Education + Hypertension + Diabetes, family = "binomial", data=training.data)
	#predict for the test data
	preds = predict(model, newdata = test.data, type = "response")
	roc_object = roc(test.data$lowbw, preds, direction = "<")
	test.error.vector[i] = roc_object$auc
}
mean(test.error.vector)


#4. Pick a variable (biomarker). Assess the incremental value of the biomarker i.e., calculate the test error rate in terms of AUC using 5-fold CV with the biomarker now added to the model. Calculate the difference in the two CV AUCs. 

#I picked BL.Anti.inf = Did participant report use of antiinflammatory medication at or less than 6 months before baseline?, integer, 0 = No; 1 = Yes; There are no missing data

#without biomarker again, set seed
set.seed(50)
total.n = length(opt.data$lowbw)
k=5
test.error.vector = vector(length = k)
assign = sample(1:5, total.n, replace = T)
for(i in 1:k){
	test = which(assign == i)
	training = setdiff(1:total.n, test)
	training.data = opt.data[training,]
	test.data = opt.data[test,]

	#fit the model with the training data
	model = glm(lowbw ~ Group + Age + Black + Nat.Am + Asian + Education + Hypertension + Diabetes, family = "binomial", data=training.data)
	#predict for the test data
	preds = predict(model, newdata = test.data, type = "response")
	roc_object = roc(test.data$lowbw, preds, direction = "<")
	test.error.vector[i] = roc_object$auc
}
auc.without = mean(test.error.vector)

#with biomarker
set.seed(50)
test.error.vector = vector(length = k)
assign = sample(1:5, total.n, replace = T)
for(i in 1:k){
	test = which(assign == i)
	training = setdiff(1:total.n, test)
	training.data = opt.data[training,]
	test.data = opt.data[test,]

	#fit the model with the training data
	model = glm(lowbw ~ Group + Age + Black + Nat.Am + Asian + Education + Hypertension + Diabetes + BL.Anti.inf, family = "binomial", data=training.data)
	#predict for the test data
	preds = predict(model, newdata = test.data, type = "response")
	roc_object = roc(test.data$lowbw, preds, direction = "<")
	test.error.vector[i] = roc_object$auc
}
auc.with = mean(test.error.vector)

auc.without
auc.with
auc.with-auc.without


#################################################
# AUC variance

#single split
total.n = length(aSAH$age)
training = sample(total.n, round(0.5*total.n), replace = F)
test = setdiff(1:total.n, training)
training.data = aSAH[training,]
test.data = aSAH[test,]

#fit the model with the training data
model = glm(outcome.binary ~ age + gender + s100b + ndka, family = "binomial", data=training.data)
#predict for the test data
preds = predict(model, newdata = test.data, type = "response")
roc_object = roc(test.data$outcome.binary, preds, direction = "<")
roc_object$auc

#CI with delong method
ci.auc(roc_object, method = "delong")

#CI with bootstrap method
ci.auc(roc_object, method = "bootstrap")



