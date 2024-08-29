## load libraries
library(ggplot2)
library(gridExtra)
library(sf)
library(sp)
library(spdep)
library(spatialreg)



#---------------------------------   Part a -----------------------------------------
load("/Users/rafa/Documents/Master Austin/MAESTRIA_AUSTIN/Advanced Predictive Models/Hw6/Hw6Part1.RData")


# scatterplot of avg_logPOS vs. logPOS
ggplot(data=crime_dat,
       aes(x = poverty, y = crime)) +
  geom_point() +
  stat_smooth(method = "lm") +
  xlab("Percentage of the population below the poverty line") +
  ylab("Counts of violent crimes ")+
  labs(title="NNCS counts of violent crimes per 1,000 individuals in year 2000 vs 
       Percentage of the population below the poverty line")


#---------------------------------   Part b -----------------------------------------


print("We cannot make a conclusion from aggregate data to individuals")


#---------------------------------   Part c -----------------------------------------

crime_dat$logCrime <- log(crime_dat$crime)
crime_dat$logPoverty <- log(crime_dat$poverty)


# map of the log Crime 
ggplot(data = crime_dat) +
  geom_sf(fill = "white")



ggplot(data = crime_dat) +  
  geom_sf(aes(fill = logCrime))+
  labs(title="Log Crime")


#-------


# map of the log Poverty 


ggplot(data = crime_dat) +  
  geom_sf(aes(fill = logPoverty))+
  labs(title="Log Poverty")


#---------------------------------   Part d -----------------------------------------

# Construct neighborhood list
nb <- poly2nb(crime_dat) 


  

# -> plot
nb_lines <- nb %>%
  nb2lines(coords = coordinates(as(crime_dat, "Spatial"))) %>%
  as("sf") %>%
  st_set_crs(st_crs(crime_dat))

ggplot(data = crime_dat) +
  geom_sf(fill = "white", color = "lightgrey") + 
  geom_sf(data = nb_lines, col = "red") +
  labs(title = "Adjacent Census Tracts")


#---------------------------------   Part e -----------------------------------------

coordinates_centroids<-coordinates(as(crime_dat, "Spatial"))







print("One census tract have no neighbors, i.e. one island")
print(paste("coordinates",-82.93626, 39.81671))


\newpage


#---------------------------------   SECOND PART  --------------------------------------------------


#---------------------------------   Part f-----------------------------------------


crime_dat2<-crime_dat[-195,]
crime_filtered<-crime_dat2[crime_dat2$crime!=0,]

nb_filtered <- poly2nb(crime_filtered) 




# construct binary adjacency matrix
W_mat <- nb2listw(nb_filtered, 
                  style = 'B', 
                  zero.policy = T) 

crime_filtered$log_Crime <- log(crime_filtered$crime)

# SMA Model

fit_SMA <- spautolm(log_Crime ~ poverty,
                    listw = W_mat,
                    family = "SMA",
                    data = crime_filtered)
summary(fit_SMA)


print(paste("The intercept: " ,signif(1.999317 ,3)," with standard error",signif(0.118105,3)))


print(paste("The poverty coefficient: " ,signif(0.045886,3)," with standard error",signif(0.004671,3)))



print("A positive  change in poverty rate is statistically significantly associated with a positive small change in crime rate after adjusting for the spatial structure of crime rate in the city")



#---------------------------------   Part g-----------------------------------------


# non-spatial linear model
fit_ns <- lm(log_Crime ~ poverty,
             data = crime_filtered)

summary(fit_ns)


print(paste("The intercept: " ,signif(1.918417,3)," with standard error",signif(0.094889,3)))


print(paste("The poverty coefficient: " ,signif(0.057356,3)," with standard error",signif(0.004466,3)))

print(paste("Non spatial AIC:",AIC(fit_ns)))
print(paste("SMA AIC : ",AIC(fit_SMA)))

print("The non-spatial model coefficient predicts a greater change in crime rate due to change in poverty rate than the SMA model.")
print("The model that better fits the data is the spatial moving average model (SMA)")



#---------------------------------   Part h-----------------------------------------
coord_stadium<-c(-83.019707, 40.001633)



coord_stadium<-c(-83.019707, 40.001633)


st_stadium<-st_point(coord_stadium)
r_within<-st_contains(crime_dat,st_stadium)
r_within
#let result be T/F list returns from st_contains
#return the polygon as an sf object



for (i in 20:40)
{
  print(i)
  print(crime_dat[i,])
}

tract <-crime_dat[35,]


tract

ggplot(data = crime_dat) +
  geom_sf(fill = "white", color = "lightgrey")+ 
  geom_point(aes(x=coord_stadium[1],y=coord_stadium[2]),color="red") +geom_sf(data=tract, stat = "sf",fill = "blue")





#---------------------------------   Part i-----------------------------------------

st_stadium<-st_point(coord_stadium)
st_within(st_stadium,crime_filtered)

crime_filtered$pred_ns <- fit_ns$fitted.values
crime_filtered$pred_sma <- fit_SMA$fit$fitted.values


crime_filtered

print(paste("Prediction of log crime NS",signif(2.873048,3)))
print(paste("Prediction of log crime SMA: ",signif(3.1450651,3)))
print(paste("Observed log crime", signif(3.6023620,3)))

print("We can notice that the prediction of the SMA model is closer to the actual log crime value than the non spatial model.")

