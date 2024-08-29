# load library
library(ggplot2)
library(gridExtra)
library(tidyverse) 
library(astsa)
library(xts)
library(lubridate)

#--------------------------------question b-----------------------------------------
data1 <- read.csv("/Users/rafa/Documents/Master Austin/MAESTRÍA_AUSTIN/Advanced Predictive Models/HW2/file3.xls", header=TRUE, stringsAsFactors=FALSE)
avg_temps<-data1$AvgTemp
#Fitting to data all combinations except  for P=1,D=0,Q=1 and d=0
count<-0
lowest<-100
for (p_ar in 0:1) 
{
  for (q_ar in 0:1)
  {
    for (d_ar in 0:1)
    {
      for (P__ar in 0:1 )
      {
        for (Q__ar in 0:1 )
        {
          for (D__ar in 0:1 )
          {
            if (d_ar!=0 | P__ar !=1 | D__ar!=0 | Q__ar != 1)
              {
              # Fit model  
              fit <- sarima(avg_temps,
                            p = p_ar, 
                            d = d_ar, 
                            q = q_ar, 
                            P=P__ar,
                            D=D__ar,
                            Q=Q__ar,
                            S=3,
                            no.constant = TRUE,
                            details = FALSE)
              
              if (fit$AICc < lowest)
              {
                p_low<-p_ar
                d_low<-d_ar
                q_low<-q_ar
                P_low<-P__ar
                D_low<-D__ar
                Q_low<-Q__ar
                lowest<-fit$AICc 
              }
              # Examine estimated model parameters
              print(paste("SARIMA(p=",p_ar,"d=",d_ar, "q=", q_ar,  ") x (P=",P__ar,",D=",D__ar,"Q=",Q__ar,") with AICc =,", fit$AICc))
            }
        }
      }
      
    }
   
    }
  }
}
  

print(paste("Lowest SARIMA(p=",p_low,"d=",d_low, "q=", q_low,  ") x (P=",P_low,",D=",D_low,"Q=",Q_low,") with AICc =,", lowest))


fit <- sarima(avg_temps,
              p = p_low, 
              d = d_low, 
              q = q_low, 
              P=P_low,
              D=D_low,
              Q=Q_low,
              S=3,
              no.constant = TRUE,
              details = FALSE)
fit
#--------------------------------question c-----------------------------------------

#Index of Month of January 2015
limit<-12*15+1
# Number of Predictions from 2015 to 2020 (5 years)
limit2<-limit+12*5-1

preds<-numeric(limit2-limit)
SE<-numeric(limit2-limit)

#predicting for each day in one year advance 
for (i in limit:limit2)
{
  x_train<-data1[-(i+1:length(data1$Month)),]
  fit_for <- sarima.for(x_train$AvgTemp, 
                        n.ahead = 12, 
                        p = 1, 
                        d = 1, 
                        q = 0, 
                        P=1,
                        D=0,
                        Q=1,
                        S=3,
                        plot = F)
  preds[i-limit+1]<-fit_for$pred[1]
  SE[i-limit+1]<-fit_for$se[1]
  
  print(x_train$Month[length(x_train$Month)])
  print(preds[i-limit+1])
  print(preds[i-limit+1]- 1.96*SE[i-limit+1])
  print(preds[i-limit+1]+1.96*SE[i-limit+1])
}
#Creating the data frames
pred_lim1<-limit+12
pred_lim2<-limit2+12

lim10<-12*10+1
lim20<-pred_lim2
pred_time<-data.frame(Type=factor(rep("Prediction", pred_lim2-pred_lim1+1)),Time=as.Date(data1$Month[pred_lim1:pred_lim2]),Temperature=preds)
f2010_2020<-data.frame(Type=factor(rep("Observed Data", lim20-lim10+1)),Time=as.Date(data1$Month[lim10:lim20]),Temperature=data1$AvgTemp[lim10:lim20])

fit_data <- bind_rows(pred_time,f2010_2020)
fit_data
# Plot data and forecasts
gg_fit <- ggplot(fit_data,aes(x = Time,y=Temperature,group=Type),colour=Type) +geom_point()+geom_line(aes(color=Type)) +
  geom_ribbon(data = pred_time,
              aes(x = Time, 
                  ymin = Temperature - 1.96*SE,
                  ymax = Temperature + 1.96*SE),
              alpha = .2)+
  labs(title = "Maximum daily temperatures in Dallas")

gg_fit


#--------------------------------question d-----------------------------------------
data1 <- read.csv("/Users/rafa/Documents/Master Austin/MAESTRÍA_AUSTIN/Advanced Predictive Models/HW2/file3.xls", header=TRUE, stringsAsFactors=FALSE)
#fitting to obtain AIC_c
fit <- sarima(data1$AvgTemp,
              p = 3, 
              d = 1, 
              q = 1, 
              no.constant = TRUE,
              details = FALSE)
fit 
#Index of the month January
limit<-12*15+1
#Number of predictions in 5 years
limit2<-limit+12*5-1
preds<-numeric(limit2-limit)
SE<-numeric(limit2-limit)
for (i in limit:limit2)
{
  x_train<-data1[-(i+1:length(data1$Month)),]
  fit_for <- sarima.for(x_train$AvgTemp, 
                        n.ahead = 12, 
                        p = 3, 
                        d = 1, 
                        q = 1, 
                        plot = F)
  preds[i-limit+1]<-fit_for$pred[1]
  #Standard error 
  SE[i-limit+1]<-fit_for$se[1]
  #Getting the prediction and upper and lowe bound 
  print(x_train$Month[length(x_train$Month)])
  print(preds[i-limit+1])
  print(preds[i-limit+1]- 1.96*SE[i-limit+1])
  print(preds[i-limit+1]+1.96*SE[i-limit+1])
}
#Making dataframes
pred_lim1<-limit+12
pred_lim2<-limit2+12
lim10<-12*10+1
lim20<-pred_lim2

pred_time<-data.frame(Type=factor(rep("Prediction", pred_lim2-pred_lim1+1)),Time=as.Date(data1$Month[pred_lim1:pred_lim2]),Temperature=preds)
f2010_2020<-data.frame(Type=factor(rep("Observed Data", lim20-lim10+1)),Time=as.Date(data1$Month[lim10:lim20]),Temperature=data1$AvgTemp[lim10:lim20])
fit_data <- bind_rows(pred_time,f2010_2020)
fit_data
# Plot data and forecasts
gg_fit <- ggplot(fit_data,aes(x = Time,y=Temperature,group=Type),colour=Type) +geom_point()+geom_line(aes(color=Type)) +
  geom_ribbon(data = pred_time,
              aes(x = Time, 
                  ymin = Temperature - 1.96*SE,
                  ymax = Temperature + 1.96*SE),
              alpha = .2)+
  labs(title = "Maximum daily temperatures in Dallas")

gg_fit



#--------------------------------question e-----------------------------------------


#Same procedure as d) ...just changing the model to ARIMA(12,1,0)
data1 <- read.csv("/Users/rafa/Documents/Master Austin/MAESTRÍA_AUSTIN/Advanced Predictive Models/HW2/file3.xls", header=TRUE, stringsAsFactors=FALSE)
fit <- sarima(data1$AvgTemp,
              p = 12, 
              d = 1, 
              q = 0, 
              no.constant = TRUE,
              details = FALSE)
fit 
limit<-12*15+1
limit2<-limit+12*5-1
preds<-numeric(limit2-limit)
SE<-numeric(limit2-limit)
for (i in limit:limit2)
{
  x_train<-data1[-(i+1:length(data1$Month)),]
  fit_for <- sarima.for(x_train$AvgTemp, 
                        n.ahead = 12, 
                        p = 12, 
                        d = 1, 
                        q = 0, 
                        plot = F)
  preds[i-limit+1]<-fit_for$pred[1]
  SE[i-limit+1]<-fit_for$se[1]
  print(x_train$Month[i])
  print(preds[i-limit+1])
  print(preds[i-limit+1]- 1.96*SE[i-limit+1])
  print(preds[i-limit+1]+1.96*SE[i-limit+1])
}
pred_lim1<-limit+12
pred_lim2<-limit2+12
lim10<-12*10+1
lim20<-pred_lim2
pred_time<-data.frame(Type=factor(rep("Prediction", pred_lim2-pred_lim1+1)),Time=as.Date(data1$Month[pred_lim1:pred_lim2]),Temperature=preds)
f2010_2020<-data.frame(Type=factor(rep("Observed Data", lim20-lim10+1)),Time=as.Date(data1$Month[lim10:lim20]),Temperature=data1$AvgTemp[lim10:lim20])
fit_data <- bind_rows(pred_time,f2010_2020)
fit_data
# Plot data and forecasts
gg_fit <- ggplot(fit_data,aes(x = Time,y=Temperature,group=Type),colour=Type) +geom_point()+geom_line(aes(color=Type)) +
  geom_ribbon(data = pred_time,
              aes(x = Time, 
                  ymin = Temperature - 1.96*SE,
                  ymax = Temperature + 1.96*SE),
              alpha = .2)+
  labs(title = "Maximum daily temperatures in Dallas")

gg_fit





