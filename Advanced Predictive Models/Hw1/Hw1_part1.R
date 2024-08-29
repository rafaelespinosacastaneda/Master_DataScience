
#The time series mean is 0 and variance 1
mu<-0.0
sigma2<- 1.0
#We create 121 white noise elements taken from the normal distribution N(0,1)
w<-rnorm(121, mu, sqrt(sigma2))
#Create a vector of 121 elements
x<- rep(0, 121)

#Create the time series 
for (t in 0:120)
{
  x[t]<-10*sin(t/5)+w[t]
}
# We make the time series as a data frame
sim_tim_series <- data.frame("Time" = 0:120,"x" =x)
# construct a plot of the time series
gg_time_series <- ggplot(sim_tim_series,
                   aes(x = Time, y = x)) + 
  geom_line(size = 1, color = "blue") +
  geom_point(shape = 19, size = 2, fill = "white", color = "blue")  +
  ggtitle("x_t=Time Series 10 sin(t/5)+ w_t")

#plot the time series
gg_time_series

###################################################
#  QUESTION 2


mu<-0.0
sigma2<- 1.0
w<-rnorm(121, mu, sqrt(sigma2))
x<- rep(0, 121)
for (t in 0:120)
{
  x[t]<-10*sin(t/5)+w[t]
}
# simulate the independent time series
sim_tim_series <- data.frame("Time" = 0:120,"x" =x)
# construct a plot of the independent time series
gg_time_series <- ggplot(sim_tim_series,
                         aes(x = Time, y = x)) + 
  geom_line(size = 1, color = "blue") +
  geom_point(shape = 19, size = 2, fill = "white", color = "blue")+
  coord_cartesian(ylim = c(-15,15))+
  ggtitle("x_t=10 sin(t/5)+ w_t with variance=1")
#The new white noise has variance=16. 
mu_2<-0.0
sigma2_2<- 16.0
#We create 121 random elements from the distribution N(0,16), because the time series starts from t=0
#up to t=120
w_2<-rnorm(121, mu_2, sqrt(sigma2_2))
# Create an array full of 0`s
x_2<- rep(0, 121)
#We get the values of the time series starting from t=0 up to t=120 with the new white noise
for (t in 0:120)
{
  x_2[t]<-10*sin(t/5)+w_2[t]
}
# Make the created time series as dataframe
sim_tim_series_2 <- data.frame("Time" = 0:120,"x" =x_2)
# construct a plot of the new time series
gg_time_series_2 <- ggplot(sim_tim_series_2,
                         aes(x = Time, y = x)) + 
  geom_line(size = 1, color = "blue") +
  geom_point(shape = 19, size = 2, fill = "white", color = "blue")+
  coord_cartesian(ylim = c(-15,15)) +
  ggtitle("x_t=10 sin(t/5)+ w_t with variance=16")
#Plot both time series, first time series in the left and the new times series plotted to the right
grid.arrange(arrangeGrob(gg_time_series,gg_time_series_2,ncol=2))







#We get the time series value at time t=45 of th series x_t= A sin(t/5)+w_t, where
# where w_t= N(0,1)
print(x[45])
#We get the time series value at time t=45 of th series x_t= A sin(t/5)+w_t
# where w_t= N(0,16)
print(x_2[45])
#The expected value for both time series is 10*sin(t/5)
print(10*sin(45/5))

