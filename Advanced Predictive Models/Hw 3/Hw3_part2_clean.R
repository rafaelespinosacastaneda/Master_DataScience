
# --------------------------Setting the parameters to be used------------------------
# load libraries
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(xts)
library(dlm)
# set true values of parameters
sigma2v_tr <- 9
sigma2w_tr <- 4
m0_tr <- 0
C0_tr <- 25




n <- 40
R <- rep(NA, n)
C <- rep(NA, n)
Q <- rep(NA, n)
m<- rep(NA, n)
a<- rep(NA, n)
f<- rep(NA, n)
e<- rep(NA, n)
G<-0.8
C_0<-25
m_0<-0
F<-1.2
W<-4
V<-9





sim_data<-read.csv("/Users/rafa/Documents/Master Austin/MAESTRIA_AUSTIN/Advanced Predictive Models/HW3/Data_hw3.xls")



gg_sim <- ggplot(sim_data,
                 aes(y = yt,
                     x = time)) +
  geom_line(linetype = "dashed",
            color = "black") 


###################################################
# ANALYZE SIMULATED DATA USING A DLM

# construct a dlm model object 
#with parameters fixed at their true values
dlm_mod <- dlm(FF = F,
               GG = G,
               V = V,
               W = W,
               m0 = m_0,
               C0 = C_0)   

# filter the simulated data y using the dlm
sim_data_filtered <- dlmFilter(y = sim_data$yt,
                               mod = dlm_mod)



#--------------------------------------------------------------------Part a---------------------------------

# store and plot the one-step-ahead predictions 
# of theta and standard errors
sim_data$pred <- sim_data_filtered$a
sim_data$pSE <- sqrt(unlist(
  dlmSvd2var(sim_data_filtered$U.R, 
             sim_data_filtered$D.R)))

gg_sim +
  geom_line(data = sim_data, 
            aes(y = pred,
                x = time),
            color = "red",
            size = 1.2) +
  geom_ribbon(data = sim_data,
              aes(x = time, 
                  ymin = pred - 1.96 * pSE,
                  ymax = pred + 1.96 * pSE),
              fill = "red",
              alpha = 0.2) +
  labs(title = expression(
    paste("One-Step-Ahead Predictions of ",
          theta[t],
          " w/ Standard Errors"))) +
  ylab("")




print("Parameter R_40")
dlmSvd2var(sim_data_filtered$U.R, 
           sim_data_filtered$D.R)[40]

print("Parameter a_40")
sim_data_filtered$a[40]


#--------------------------------------------------------------------Part b---------------------------------



n <- length(sim_data_filtered$f)
R <- rep(NA, n)
C <- rep(NA, n)
Q <- rep(NA, n)
m<- rep(NA, n)
a<- rep(NA, n)
f<- rep(NA, n)
e<- rep(NA, n)
G<-0.8
C_0<-25
m_0<-0
F<-1.2
W<-4
V<-9
R[1] <- G*C_0*G+W
Q[1]<-F*R[1]*F+V
C[1] <- R[1]-R[1]*F*(1/Q[1])*F*R[1]
a[1]<-m_0*G
f[1]<-F*a[1]
e[1]<-sim_data$yt[1]-f[1]
m[1]<-a[1]+R[1]*F*(1/Q[1])*e[1]


for(t in 2:n) 
{
  
  R[t] <- G*C[t-1]*G+W
  Q[t]<-F*R[t]*F+V
  C[t] <- R[t]-R[t]*F*(1/Q[t])*F*R[t]
  a[t]<-m[t-1]*G
  f[t]<-F*a[t]
  e[t]<-sim_data$yt[t]-f[t]
  m[t]<-a[t]+R[t]*F*(1/Q[t])*e[t]
  
  
  if ( is.na(sim_data$yt[t]) )
  {
    R[t] <- G*C[t-1]*G+W
    Q[t]<-F*R[t]*F+V
    C[t] <- R[t]
    a[t]<-m[t-1]*G
    f[t]<-F*a[t]
    m[t]<-a[t]
  }
  
}



a[40]
R[40]
m[40]
C[40]
f[40]
Q[40]

sim_data_filtered$m[22]

#store and plot the one-step-ahead predictions 
# of theta and standard errors
sim_data$pred_y <- sim_data_filtered$f
sim_data$pSE2 <- sqrt(Q)

gg_sim +
  geom_line(data = sim_data, 
            aes(y = pred_y,
                x = time),
            color = "purple",
            size = 1.2) +
  geom_ribbon(data = sim_data,
              aes(x = time, 
                  ymin = pred_y - 1.96 * pSE2,
                  ymax = pred_y + 1.96 * pSE2),
              fill = "purple",
              alpha = 0.2) +
  labs(title = expression(
    paste("One-Step-Ahead Predictions of ",
          theta[t],
          " w/ Standard Errors"))) +
  ylab("")


f_calculated <- F*sim_data_filtered$a[40]
R_calculated <-unlist(dlmSvd2var(sim_data_filtered$U.R, 
                          sim_data_filtered$D.R))[40]
Q_calculated <-F*R_calculated*F+V

print("Parameter Q_40")
Q_calculated
print("Parameter f_40")
f_calculated

#--------------------------------------------------------------------Part c---------------------------------



sim_data$filtered_theta_upt <- dropFirst(sim_data_filtered$m)
sim_data$fSE <- dropFirst(sqrt(unlist(
  dlmSvd2var(sim_data_filtered$U.C, 
             sim_data_filtered$D.C))))

gg_sim +
  geom_line(data = sim_data, 
            aes(y = filtered_theta_upt,
                x = time),
            color = "green",
            size = 1.2) +
  geom_ribbon(data = sim_data,
              aes(x = time, 
                  ymin = filtered_theta_upt - 1.96 * fSE,
                  ymax = filtered_theta_upt + 1.96 * fSE),
              fill = "green",
              alpha = 0.2) +
  labs(title = expression(
    paste("One-Step-Ahead Predictions of ",
          theta[t],
          " w/ Standard Errors"))) +
  ylab("")

print("Parameter m_40")
sim_data_filtered$m[23]
m[22]



print("Parameter C_40")

dlmSvd2var(sim_data_filtered$U.C, 
           sim_data_filtered$D.C)[40]


#--------------------------------------------------------------------Part e---------------------------------



sim_data_smoothed <- dlmSmooth(sim_data_filtered)
sim_data$smoothed <- dropFirst(sim_data_smoothed$s)
sim_data$sSE <- dropFirst(sqrt(unlist(
  dlmSvd2var(sim_data_smoothed$U.S, 
             sim_data_smoothed$D.S))))


gg_sim +geom_line(data = sim_data, 
                  aes(y = smoothed,
                      x = time),
                  color = "darkgreen",
                  size = 1.2) +
  geom_ribbon(data = sim_data,
              aes(x = time, 
                  ymin = smoothed - 1.96 * sSE,
                  ymax = smoothed + 1.96 * sSE),
              fill = "green",
              alpha = 0.2) +
  labs(title = expression(
    paste("Smoothed Values of ",
          theta[t],
          " w/ Standard Errors"))) +
  ylab("")


for (t in 1:length(sim_data$smoothed))
{
  if ( is.na(sim_data$yt[t]) )
  {
    print(paste("For missing y in time t=",t,"theta is "))
   print(sim_data$smoothed[t])
}
}

#--------------------------------------------------------------------Part f---------------------------------

dlm_mod2 <- dlm(FF = F,
               GG = G,
               V = V,
               W = W,
               m0 = m_0,
               C0 = C_0)   

# filter the simulated data y using the dlm
sim_data_filtered2<- dlmFilter(y = sim_data$yt,
                               mod = dlm_mod2)



sim_data_forcasted<-dlmForecast(sim_data_filtered2, nAhead = 10, method = c("svd"))

sim_data_forcasted$f
sqrt(unlist(sim_data_forcasted$Q))
pred_forcast <- data.frame(yt=101:110,Time = 101:110,
                            forecast = c(sim_data_forcasted$f),
                            sim_data_fsE2 = c(sqrt(unlist(sim_data_forcasted$Q))))

pred_forcast
gg_sim+geom_line(data=pred_forcast,aes(y = forecast, x =Time)) + geom_ribbon(data = pred_forcast,
                   aes(x = Time, 
                       ymin = forecast - 1.96*sim_data_fsE2,
                       ymax = forecast + 1.96*sim_data_fsE2),
                   alpha = .2) 


print("Q_101 value is")
pred_forcast$sim_data_fsE2[1]**2

print("Q_110 value is")
pred_forcast$sim_data_fsE2[10]**2


print("We are less uncertain about our guess ( forecast) to nearer times to our observations. As we forecast more and more into the future, we ahave less certainty of our guess." )






