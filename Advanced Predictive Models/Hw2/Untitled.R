

#--------------------------------question a-----------------------------------------

data1 <- read.csv("/Users/rafa/Documents/Master Austin/MAESTRÍA_AUSTIN/Advanced Predictive Models/HW2/file1.xls", header=TRUE, stringsAsFactors=FALSE)

data1



d<- 0.0

for (p_ar in 1:3) 
{
  for (q_ar in 1:3)
  {
    if (p_ar!=q_ar){

      # Fit model  
      fit <- sarima(data1$x,
                    p = p_ar, 
                    d = d, 
                    q = q_ar, 
                    no.constant = TRUE,
                    details = FALSE)
      
      # Examine estimated model parameters
      
      print(paste("ARMA(p=",p_ar,",q=", q_ar, "), with AICc =,", fit$AICc))
    }
  }
  
}

#--------------------------------question b-----------------------------------------


# Selected ARMA model p=3, q=1

fit_best <- sarima(data1$x,
              p = 3, 
              d = 0, 
              q = 1, 
              no.constant = TRUE,
              details = FALSE)
fit_best



#--------------------------------question c-----------------------------------------


x <- read.csv("/Users/rafa/Documents/Master Austin/MAESTRÍA_AUSTIN/Advanced Predictive Models/HW2/file1_2.xls", header=TRUE, stringsAsFactors=FALSE)

n<-250
n_train <-150
x_train<-x[1:150,]

data_test<-x[151:250,]



fit_train<- sarima(x_train$x,
                   p = 2, 
                   d = 0, 
                   q = 2, 
                   no.constant = TRUE,
                   details = FALSE)
fit_best


fit_for <- sarima.for(x_train$x, 
                      n.ahead = 100, 
                      p = 2, 
                      d = 0, 
                      q = 2,
                      plot = F)

# Collect time series for plotting
fit_data <- bind_rows(
  data.frame(Time = 1:150,
             Type = factor(rep("Training Data", 150)),
             x = c(as.numeric(x_train$x))),
  data.frame(Time = 151:250,
             Type = factor(rep("Test Data", 100)),
             x = c( as.numeric(data_test$x))),
  data.frame(Time = 151:250,
             Type = factor(rep("Prediction", 100)),
             x = c(as.numeric(fit_for$pred ))))

fit_pred_data <- data.frame(Time = 151:n,
                            x = c(as.numeric(fit_for$pred)),
                            SE = c( as.numeric(fit_for$se)))

# Plot data and forecasts
gg_fit <- ggplot(fit_data,
                 aes(x = Time)) +
  geom_line(aes(y = x, col = Type)) +
  geom_ribbon(data = fit_pred_data,
              aes(x = Time, 
                  ymin = x - 1.96*SE,
                  ymax = x + 1.96*SE),
              alpha = .2) +
  geom_vline(xintercept = 150)

gg_fit
              details = FALSE)