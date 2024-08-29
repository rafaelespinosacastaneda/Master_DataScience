###################################################
## COMPUTING DEMO:  Dynamic Linear Models (CORRECTED)
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
## Last updated: December 16, 2021 
###################################################

# load libraries
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(xts)
library(dlm)

###################################################
# SIMULATE DATA: univariate DLM w/ F_t = 1
# and G_t = 1 for t = 1, 2, ...

# set true values of parameters
sigma2v_tr <- 9
sigma2w_tr <- 4
m0_tr <- 0
C0_tr <- 25

# generate y = y_1,...y_n
n <- 500
theta <- rep(NA, n)
y <- rep(NA, n)

theta0 <- rnorm(1, m0_tr, sqrt(C0_tr))
theta[1] <- theta0 + rnorm(1, 0, sqrt(sigma2w_tr))
for(i in 2:n) theta[i] <- theta[i-1] + rnorm(1, 0, sqrt(sigma2w_tr))
for(j in 1:n) y[j] <- theta[j] + rnorm(1, 0, sqrt(sigma2v_tr))








#--------------------------------------------------------------------Part a---------------------------------
#ONE STEP AHEAD of theta given all previous observations 
#Time series (or matrix) of predicted values of the 
#state vectors given the obser- vations 
#up and including the previous time unit.

# set true values of parameters
sigma2v_tr <- 9
sigma2w_tr <- 4
m0_tr <- 0
C0_tr <- 25

sim_data<-read.csv("/Users/rafa/Documents/Master Austin/MAESTRÍA_AUSTIN/Advanced Predictive Models/HW3/Data_hw3.xls")

sim_data

gg_sim <- ggplot(sim_data,
                 aes(y = yt,
                     x = time)) +
  geom_line(linetype = "dashed",
            color = "black") 


###################################################
# ANALYZE SIMULATED DATA USING A DLM

# construct a dlm model object 
#with parameters fixed at their true values
dlm_mod <- dlm(FF = 1.2,
               GG = 0.8,
               V = sigma2v_tr,
               W = sigma2w_tr,
               m0 = m0_tr,
               C0 = C0_tr)   

# filter the simulated data y using the dlm
sim_data_filtered <- dlmFilter(y = sim_data$yt,
                               mod = dlm_mod)

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
                  ymin = pred - 2 * pSE,
                  ymax = pred + 2 * pSE),
                  fill = "red",
              alpha = 0.2) +
  labs(title = expression(
    paste("One-Step-Ahead Predictions of ",
          theta[t],
          " w/ Standard Errors"))) +
  ylab("")
         


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
  
  
  if (is.na(sim_data$yt[t]) )
  {
    
    R[t] <-R[t-1] 
    Q[t]<-Q[t-1]
    C[t] <- C[t-1]
    a[t]<-a[t-1]
    f[t]<-f[t-1]
    e[t]<-e[t-1]
    m[t]<-m[t-1]
  }
    

}

print("For a paramaters")

a[40]
R[40]
C[40]
dlmSvd2var(sim_data_filtered$U.R, 
           sim_data_filtered$D.R)[40]

sim_data_filtered$a[40]

# --------------------- Part b----------------------------



#ONE STEP AHEAD of theta given all previous observations 
#Time series (or matrix) of predicted values of the 
#state vectors given the obser- vations 
#up and including the previous time unit.

# set true values of parameters
sigma2v_tr <- 9
sigma2w_tr <- 4
m0_tr <- 0
C0_tr <- 25

sim_data<-read.csv("/Users/rafa/Documents/Master Austin/MAESTRÍA_AUSTIN/Advanced Predictive Models/HW3/Data_hw3.xls")

sim_data

gg_sim <- ggplot(sim_data,
                 aes(y = yt,
                     x = time)) +
  geom_line(linetype = "dashed",
            color = "black") 


###################################################
# ANALYZE SIMULATED DATA USING A DLM

# construct a dlm model object 
#with parameters fixed at their true values
dlm_mod <- dlm(FF = 1.2,
               GG = 0.8,
               V = sigma2v_tr,
               W = sigma2w_tr,
               m0 = m0_tr,
               C0 = C0_tr)   

# filter the simulated data y using the dlm
sim_data_filtered <- dlmFilter(y = sim_data$yt,
                               mod = dlm_mod)

# store and plot the one-step-ahead predictions 
# of theta and standard errors
sim_data$pred_y <- sim_data_filtered$f
sim_data$pSE <- sqrt(unlist(
  dlmSvd2var(sim_data_filtered$U.R, 
             sim_data_filtered$D.R)))

gg_sim +
  geom_line(data = sim_data, 
            aes(y = pred_y,
                x = time),
            color = "purple",
            size = 1.2) +
  geom_ribbon(data = sim_data,
              aes(x = time, 
                  ymin = pred_y - 2 * pSE,
                  ymax = pred_y + 2 * pSE),
              fill = "purple",
              alpha = 0.2) +
  labs(title = expression(
    paste("One-Step-Ahead Predictions of ",
          theta[t],
          " w/ Standard Errors"))) +
  ylab("")



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
  
  
  if (is.na(sim_data$yt[t]) )
  {
    
    R[t] <-R[t-1] 
    Q[t]<-Q[t-1]
    C[t] <- C[t-1]
    a[t]<-a[t-1]
    f[t]<-f[t-1]
    e[t]<-e[t-1]
    m[t]<-m[t-1]
  }
  
  
}


print("For b paramaters")


a[40]
R[40]
C[40]
dlmSvd2var(sim_data_filtered$U.R, 
           sim_data_filtered$D.R)[40]

sim_data_filtered$a[40]
























#------------------------------------------------------Part c -------------------------------------------




#ONE STEP AHEAD of theta given all previous observations 
#Time series (or matrix) of predicted values of the 
#state vectors given the obser- vations 
#up and including the previous time unit.

# set true values of parameters
sigma2v_tr <- 9
sigma2w_tr <- 4
m0_tr <- 0
C0_tr <- 25

sim_data<-read.csv("/Users/rafa/Documents/Master Austin/MAESTRÍA_AUSTIN/Advanced Predictive Models/HW3/Data_hw3.xls")

sim_data

gg_sim <- ggplot(sim_data,
                 aes(y = yt,
                     x = time)) +
  geom_line(linetype = "dashed",
            color = "black") 


###################################################
# ANALYZE SIMULATED DATA USING A DLM

# construct a dlm model object 
#with parameters fixed at their true values
dlm_mod <- dlm(FF = 1.2,
               GG = 0.8,
               V = sigma2v_tr,
               W = sigma2w_tr,
               m0 = m0_tr,
               C0 = C0_tr)   

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
                  ymin = filtered_theta_upt - 2 * fSE,
                  ymax = filtered_theta_upt + 2 * fSE),
              fill = "green",
              alpha = 0.2) +
  labs(title = expression(
    paste("One-Step-Ahead Predictions of ",
          theta[t],
          " w/ Standard Errors"))) +
  ylab("")







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
  
  
  if (is.na(sim_data$yt[t]) )
  {
    
    R[t] <-R[t-1] 
    Q[t]<-Q[t-1]
    C[t] <- C[t-1]
    a[t]<-a[t-1]
    f[t]<-f[t-1]
    e[t]<-e[t-1]
    m[t]<-m[t-1]
  }
  
  
}

R

dlmSvd2var(sim_data_filtered$U.R, 
           sim_data_filtered$D.R)



print("For 22")

C[22]
m[22]



sim_data$yt



print("For 28")

print("With recursion")
R[28]
a[28]


print("With dlm")

dlmSvd2var(sim_data_filtered$U.R, 
           sim_data_filtered$D.R)[28]

sim_data_filtered$a[28]

print("Value of R with formula using previos value")
print(G*C[27]*G+W)

print("Value of a with formula using previos value")
print(G*m[27])














sim_data_filtered$a[28]

sim_data_filtered$m[40]


dlmSvd2var(sim_data_filtered$U.C, 
           sim_data_filtered$D.C)[40]






f[40]


print(F*sim_data_filtered$a[40])




Q[40]









#----------------------------- part d 


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
R[1] <- G*C_0*G+W
Q[1]<-F*R[1]*F+V
C[1] <- R[1]-R[1]*F*(1/Q[1])*F*R[1]
a[1]<-m_0*G
f[1]<-F*a[1]
e[1]<-sim_data$yt[1]-f[1]
m[1]<-a[1]+R[1]*F*(1/Q[1])*e[1]


for(t in 2:40) 
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

R


dlmSvd2var(sim_data_filtered$U.R, 
           sim_data_filtered$D.R)


# WE CONSIDER AS WE HAVE ALL DATA


t_p<-0


for(t in 23:n) 
{
  if ( !is.na(sim_data$yt[t]) )
  {
    tn<-t-t_p
    R[tn] <- G*C[tn-1]*G+W
    Q[tn]<-F*R[tn]*F+V
    C[tn] <- R[tn]-R[tn]*F*(1/Q[tn])*F*R[tn]
    a[tn]<-m[tn-1]*G
    f[tn]<-F*a[tn]
    e[tn]<-sim_data$yt[t]-f[tn]
    m[tn]<-a[tn]+R[tn]*F*(1/Q[tn])*e[tn]
  }
  
  else 
  {
    t_p<- t_p+1
    print(t_p)
  }
  
}

R
sim_data$yt





# CORRECTED:  store and plot the filtering distribution  
# of theta
sim_data$filtered <- dropFirst(sim_data_filtered$m)
sim_data$fSE <- dropFirst(sqrt(unlist(
  dlmSvd2var(sim_data_filtered$U.C, 
             sim_data_filtered$D.C))))
gg_sim +
  geom_line(data = sim_data, 
            aes(y = pred,
                x = time),
            color = "red",
            size = .5) +
  geom_line(data = sim_data, 
            aes(y = filtered,
                x = time),
            color = "blue",
            size = 1.2) +
  geom_ribbon(data = sim_data,
              aes(x = time, 
                  ymin = filtered - 2 * fSE,
                  ymax = filtered + 2 * fSE),
                  fill = "blue",
              alpha = 0.2) +
  #CORRECTED: new title
  labs(title = expression(
    paste("Mean of the filtering distribution of ",
           theta[t],
           " w/ Standard Errors"))) +
  ylab("")

# store and plot the smoothed 
# predictions of theta and standard errors

sim_data_smoothed <- dlmSmooth(sim_data_filtered)
sim_data$smoothed <- dropFirst(sim_data_smoothed$s)
sim_data$sSE <- dropFirst(sqrt(unlist(
  dlmSvd2var(sim_data_smoothed$U.S, 
             sim_data_smoothed$D.S))))

gg_sim +
  geom_line(data = sim_data, 
            aes(y = pred,
                x = time),
            color = "red",
            size = .5) +
  geom_line(data = sim_data, 
            aes(y = filtered,
                x = time),
            color = "blue",
            size = .5) +
  geom_line(data = sim_data, 
            aes(y = smoothed,
                x = time),
            color = "darkgreen",
            size = 1.2) +
  geom_ribbon(data = sim_data,
              aes(x = time, 
                  ymin = smoothed - 2 * sSE,
                  ymax = smoothed + 2 * sSE),
              fill = "green",
              alpha = 0.2) +
  labs(title = expression(
    paste("Smoothed Values of ",
          theta[t],
          " w/ Standard Errors"))) +
  ylab("")

# find the MLE of sigma2v and sigma2w
build_mod <- function(x) return(dlm(1, 
                                    FF = 1, 
                                    GG = 1,
                                    V = exp(x[1]),  
                                    W = exp(x[2]), 
                                    m0 = m0_tr,
                                    C0 = C0_tr))   

modMLE <- dlmMLE(y, c(0, 0), build_mod, hessian = T)
exp(modMLE$par)
