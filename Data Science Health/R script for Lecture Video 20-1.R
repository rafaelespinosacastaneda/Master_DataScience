################################
### Biostatistical Methods ##
################################


#reproduces the plot from class exercise in lecture notes

sens = c(1, 0.9, 0.7, 0)
spec = c(0,0.375,0.75, 1)

plot(1-spec, sens, typ = "o", pch = 20, lwd = 2, main = "ROC curve", xlab = "1-Specificity", ylab = "Sensitivity")
polygon(c(1-spec,1), c(sens,0), col = rgb(0.51, 0.44, 1, alpha = 0.2), border = 0)
abline(a=0, b=1, lwd = 2, lty =2)

##calculate AUC for class example using tdROC package, useful if you already have sensitivity and specificity calculated

install.packages("tdROC")
library(tdROC)

calc.AUC(sens = sens, spec=spec)

###calculate AUC using raw data using pROC function

install.packages("pROC")
library(pROC)

outcome = c(rep(1,10), rep(0,8))
pred = c(2,5,7,9,10,7,4,15,35,51,9,7,12,26,49,87,99,60)

roc.output = roc(response=outcome, predictor=pred)

#notice that it is directly telling you what it is assuming
#Setting levels: control = 0, case = 1
#Setting direction: controls > cases
#setting direction means it is assuming that HIGHER values of the pred variable indicate controls. This is correct for our cognitive functioning example. If you want to flip this, you add in: direction = "<". It is ideal to always explicitly give the direction. R picks the direction based on a calculation involving the median. It does NOT always default to >.


#look at threshold = 11 and 50, compare to our calculations
cbind(roc.output$thresholds, roc.output$sensitivities, roc.output$specificities)
roc.output$auc

#compare to tdROC
#match
calc.AUC(sens = roc.output$sensitivities, spec = roc.output$specificities)

#plot the ROC curve
plot(roc.output)

#you could also give it the data in terms of cases and controls
my.cases = pred[1:10]
my.controls = pred[11:18]

roc.output = roc(cases=my.cases, controls = my.controls)
roc.output$auc
#matches




# smoothed, using binormal (parametric)
roc.output = roc(response=outcome, predictor=pred, smooth = TRUE, smooth.method = "binormal")
plot(roc.output)


###################

#now let's use a model to get p
mydata = data.frame( z1= c(1,2,3,4,5,3,4,9),
                  z2= c(1,5,8,15,26,8,92,2),
                  outcome=c(0,1,1,0,0,1,1,0))

# fit logistic model
model = glm(outcome ~ z1+z2, family = "binomial", data=mydata)

#get predictions
preds = predict(model, type = "response")

#take a look
cbind(mydata$outcome, preds)

# create roc curve
roc_object = roc( mydata$outcome, preds, direction = "<")

roc_object$auc

################

#let's look at some real data
# This dataset summarizes several clinical and one laboratory variable of 113 patients with an aneurysmal subarachnoid hemorrhage.
#Natacha Turck, Laszlo Vutskits, Paola Sanchez-Pena, Xavier Robin, Alexandre Hainard, Marianne Gex-Fabry, Catherine Fouda, Hadiji Bassem, Markus Mueller, Frédérique Lisacek, Louis Puybasset and Jean-Charles Sanchez (2010) “A multiparameter panel method for outcome prediction following aneurysmal subarachnoid hemorrhage”. Intensive Care Medicine 36(1), 107–115. DOI: doi: 10.1007/s00134-009-1641-y.
# we will look at these predictors: age, gender, s100b, and ndka. The last 2 are biomarkers

head(aSAH)

#be explicit about the outcome
aSAH$outcome.binary = 1*(aSAH$outcome == "Good")

#fit the logistic regression model
model = glm(outcome.binary ~ age + gender + s100b + ndka, family = "binomial", data=aSAH)

#get predictions
preds = predict(model, type = "response")

#take a look
cbind(aSAH$outcome.binary, preds)

# create roc curve
roc_object = roc(aSAH$outcome.binary, preds, direction = "<")
roc_object$auc

plot(roc_object)


####################################
#incremental value

#model with both biomarkers
model = glm(outcome.binary ~ age + gender + s100b + ndka, family = "binomial", data=aSAH)
summary(model)

model.without.ndka = glm(outcome.binary ~ age + gender + s100b, family = "binomial", data=aSAH)
preds.without.ndka = predict(model.without.ndka, type = "response")
roc_object.without.ndka = roc(aSAH$outcome.binary, preds.without.ndka, direction = "<")
roc_object.without.ndka$auc

#difference
roc_object$auc-roc_object.without.ndka$auc

#show difference in plot
plot(roc_object, lwd = 2)
plot(roc_object.without.ndka, add = TRUE, col = 2, lwd = 2)
legend("bottomright", lty = c(1,1), col = c(1,2), c("ROC with ndka", "ROC without ndka"))

#now what about the incremental value of s100b?
model.without.s100b = glm(outcome.binary ~ age + gender + ndka, family = "binomial", data=aSAH)
preds.without.s100b = predict(model.without.s100b, type = "response")
roc_object.without.s100b = roc(aSAH$outcome.binary, preds.without.s100b, direction = "<")
roc_object.without.s100b$auc

#difference
roc_object$auc-roc_object.without.s100b$auc

#show difference in plot
plot(roc_object)
plot(roc_object.without.s100b, add = TRUE, col = 2)
legend("bottomright", lty = c(1,1), col = c(1,2), c("ROC with s100b", "ROC without s100b"))


