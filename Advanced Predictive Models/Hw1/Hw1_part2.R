#Tennis Court Plot
#Importing ggplot2
library(ggplot2)

#Creates a data.frame object
tennisCourt = data.frame(x1 = c(0,4.5,18,31.5,36,0,4.5,4.5,0,-2),
                         x2 = c(0,4.5,18,31.5,36,36,31.5,31.5,36,38),
                         y1 = c(-39,-39,-21,-39,-39,39,21,-21,-39,0), 
                         y2 = c(39,39,21,39,39,39,21,-21,-39,0),
                         width = c(rep(1,9),3))

#Creates a plot object called ggTennis
ggTennis = ggplot(tennisCourt) + 
  geom_segment(aes(x = x1,y = y1,xend = x2,yend = y2),size = tennisCourt$width) + 
  labs(x = "X_1",y = 'X_2',
       title = 'Tennis Court')

# -------------------------------------------------------------------------Question a---------------------------------------------------------------------
# creating the covariance matrix  with the information given by the homework
m_sigma <-matrix(data=c(4,4,4,16),nrow=2,byrow=TRUE)
mean_v <- c(29,16)
# Creating 5000 points taken by the bivariate normal distribution 
rand_points<- mvtnorm::rmvnorm(n=5000,mean=mean_v,sigma=m_sigma)
#Giving names to the columns of the data containing the 5000 points
colnames(rand_points)<-c("x","y")
#Making the data points as a dataframe 
rand_points <- as.data.frame(x=rand_points)
#Data frame of the points to add
pointsToAdd = data.frame(x =rand_points$x, y = rand_points$y)
#Now we add in the points, and create a new object
#The geom_point function helps us create points. Note that we give it new data,
ggTennisWithPoints = ggTennis + 
  geom_point(data = pointsToAdd,aes(x = x, y = y),color = 'firebrick')

#Let's see what we made
ggTennisWithPoints


# -------------------------------------------------------------------------Question b---------------------------------------------------------------------


# Using the multivariate normal distribution  function to calculate the theoretical probability.
#We use the mean vector and the covariance matrix given by the homework.
#We use the limits given by the exercise
th_prob<-mvtnorm::pmvnorm(lower=c(18,0),upper=c(31.5,21),mean=mean_v,sigma=m_sigma)

print("The theoretical probability is")
print(th_prob)

counter<- 0 

#Count how many of the 5000 generated points are valid points 
for (i in 1:5000)
{
  if  (rand_points$x[i]<=31.5  & rand_points$x[i]>=18   &   rand_points$y[i]>=0  & rand_points$y[i]<=21 )
  {
    counter <- counter + 1 
  }
}

#The probability will be the number of events that are legal/ total events .
prob<- counter/5000

print("The  probability calculated by the simulation is")
print(prob)


# -------------------------------------------------------------------------Question c---------------------------------------------------------------------
#Calculated mean and standard deviation of the conditional distribution given x_1=30.5
mean_c <-17.5
sd_c <-12**0.5

# Probability of P(X_2 <= 21 | X_1 = 30.5) 
p_21<-pnorm(21,mean = mean_c,sd =sd_c,lower.tail = TRUE, log.p = FALSE)
# Probability of P(X_2 <= 0 | X_1 = 30.5) 
p_0<-pnorm(0, mean = mean_c,sd = sd_c,lower.tail = TRUE, log.p = FALSE)

# Probability of P(0<=X_2 <= 21 | X_1 = 30.5)
p_0_21<-p_21-p_0

print(p_0_21)

# -------------------------------------------------------------------------Question d---------------------------------------------------------------------

#Calculated mean and standard deviation of the conditional distribution given x_1=30.5
mean_c <-17.5
sd_c <-12**0.5
# Create random X_2 coordinate points, using the mean and standard deviation calculated with the conditional distribution
rand_points_cond<- rnorm(n=500,mean=mean_c,sd=sd_c)

#Creating the random points, with the X_2 coordinate found with the conditional distribution, and giving the coordinate of 
#X_1= 30.5

    # Using jitter function  to reduce the effects of overplotting
x_c<- jitter(rep(1, 500)*30.5,7.3)

rand_points_cond <- as.data.frame(x=rand_points_cond)
rand_points_cond2 <- cbind(x_c, rand_points_cond)

# Names to the data as x_c (x conditioned) and y_c (y conditioned)
colnames(rand_points_cond2)<-c("x_c","y_c")

# Conditioned points to be added
pointsToAdd_cond = data.frame(x =rand_points_cond2$x_c, y =rand_points_cond2$y_c)

#Adding, the random points of the original distribution + the new conditioned points in the tennis court
ggTennisWithPoints_wc = ggTennis + 
  geom_point(data = pointsToAdd,aes(x = x, y = y),color = 'firebrick') + 
  geom_point(data = pointsToAdd_cond,aes(x = x, y = y),color = 'blue') +geom_hline(yintercept=21,linetype=2)

#Let's see what we made
ggTennisWithPoints_wc
