###################################################
## COMPUTING DEMO:  Kriging
##
## DSC 383: Advanced Predictive Models for Complex Data
## By:  Kate Calder, UT Austin
## Last updated: September 26, 2021
###################################################

## load libraries
library(ggplot2)
library(mapproj)
library(geoR)
library(leaflet)
library(gridExtra)
library(measurements)
library(dplyr)

# load scallops data
rain <- read.table("/Users/rafa/Documents/Master Austin/MAESTRIA_AUSTIN/Advanced Predictive Models/Hw5/rain.txt",header=T)

rain$altitude<-conv_unit(rain$altitude, "ft", "mi") 


# create geodata (geoR) object
rain_geo_altitude <- as.geodata(
  cbind(rain$altitude, 
        rain$x, 
        rain$y), 
  data.col = 1, 
  coords.col = 2:3)

#plot(rain_geo)

points.geodata(rain_geo_altitude,xlab = "x", ylab = "y")
title("Spatial variation in altitude ")



# create geodata (geoR) object
rain_geo_rainfall <- as.geodata(
  cbind(rain$rainfall, 
        rain$x, 
        rain$y), 
  data.col = 1, 
  coords.col = 2:3)

#plot(rain_geo)

points.geodata(rain_geo_rainfall,xlab = "x", ylab = "y")
title("Spatial variation in rainfall ")




#-----------------------------------------part b--------------------------------------------------
lR_RA = lm(sqrt(rainfall)~altitude, data = rain) 

summary(lR_RA)

print("The estimated error variance")
print(4.275^2)

print("The proportion of variance is ")

print(0.03145)


#--------------------------------------- part c------------------------------------------------------

N<-length(rain$rainfall)
#tot<-N*(N-1)

tot<-N*(N-1)/2
print(N)
print(tot)
distances<-rep(1,tot)
count<-0
for (i in 1:N)
     {
       for(j in (i+1):N)
       {
         if (i!=j)
         {
           count<-count+1
         distances[count]<- ( (rain$x[i]-rain$x[j])^2 +(rain$y[i]-rain$y[j])^2 )^(1/2)
         
         if (is.na(distances[count]))
         {
           print(paste("i",i,"j",j))
           print("The x_i")
           print(rain$x[i])
           print("The x_j")
           print(rain$x[j])
           print("The y_i")
           print(rain$y[i])
           print("The y_j")
           print(rain$y[j])
         }
         
         
         
         }
         
        }
       
     }

hist(distances, breaks=seq(from=0, to=300, by=20))


#--------------------------------------- part d------------------------------------------------------


#rain$ranifall^(1/2)-predict(lR_RA, data = list(rain$altitude))


residuals<-resid(lR_RA)

bins_ranges<-c(20,40,60,80,100,120,140,160,180,200,220,240,260,280,300)






df1 <- data.frame(x_1=c(rain$x[1]),
                  x_2=c(rain$x[2]),distance=c( ( (rain$x[1]-rain$x[2])^2 +(rain$y[1]-rain$y[2])^2 )^(1/2) ) , res1=c (residuals[1]), res2=c (residuals[2]))


for (i in 1:N)
  {
  for(j in (i+1):N)
    {
    if (i!=j && (i!=1 && j!=2)  )
      {
      count<-count+1
      distances[count]<- ( (rain$x[i]-rain$x[j])^2 +(rain$y[i]-rain$y[j])^2 )^(1/2)
      
      
          df2 <- data.frame(x_1=c(rain$x[i]),
                            x_2=c(rain$x[j]),distance=c( ( (rain$x[i]-rain$x[j])^2 +(rain$y[i]-rain$y[j])^2 )^(1/2) ) , res1=c (residuals[i]), res2=c (residuals[j]))
          
          df1<-bind_rows(df1, df2)
          
      }
    
  }

  
  }


correlation<- rep(0,length(bins_ranges))
counters_dist<- rep(0,length(bins_ranges))

count<-1

for (bin in bins_ranges)
{
  df_to_use<-filter(df1, distance<bin & distance>=bin-20 & x_1 !=x_2)
  counters_dist[count]<-length(df_to_use$x_1)
  correlation[count]<-cor(df_to_use$res1,df_to_use$res2)
  count<-count+1
}


df_to_use

centers<-c(10,30,50,70,90,110,130,150,170,190,210,230,250,270,290)



df2<-data.frame(centerBins=centers,corr=correlation,num_dist=counters_dist)


counters_dist
as_geo_corr <- as.geodata(
  cbind(counters_dist, 
        centers, 
        correlation), 
  data.col = 1, 
  coords.col = 2:3)

#points.geodata(as_geo_corr,xlab = "center bins", ylab = "correlation")


ggplot() + geom_point(data = df2, aes(x =centerBins , y = corr),cex=10*(counters_dist+min(counters_dist))/max(counters_dist)+2.5)+labs(x = "Center of distance bins",y = "Correlation")























#--------------------------------------------------------------PART 2 -------------------------------------------------------------------------

#----------------------------a -----------------------
rain_geo_rainfall2 <- as.geodata(
  cbind(sqrt(rain$rainfall), 
        rain$x, 
        rain$y,rain$altitude), 
  data.col = 1, 
  coords.col = 2:3,covar.col=4)

rain_geo_rainfall2

fit_spatial <- likfit(
  rain_geo_rainfall2,
  trend =~covar1 ,
  cov.model = "exponential",
  ini.cov.pars = c(5, .01),
  fix.nugget=TRUE, 
  nugget=0.0,nospatial=FALSE)

fit_NO_spatial <- likfit(
  rain_geo_rainfall2,
  trend =~covar1 ,
  cov.model = "exponential",
  ini.cov.pars = c(5, .01),
  fix.nugget=TRUE, 
  nugget=0.0,nospatial=TRUE)

lk_fitted

fit

# ----------------------------------b ---------------------------------------------

corr_func<-exp((-1/42.4066)*c(10,30,50,70,90,110,130,150,170,190,210,230,250,270,290))
df3<-data.frame(centerBins=centers,corr=corr_func)


ggplot() + geom_point(data = df2, aes(x =centerBins , y = corr),cex=10*(counters_dist+min(counters_dist))/max(counters_dist)+2.5)+
  labs(x = "Center of distance bins",y = "Correlation")+ geom_point(data = df3, aes(x =centerBins , y = corr),color='blue')


#-------------------------------------------c ------------------------------------

summary(fit_spatial)
summary(fit_NO_spatial)


AIC(lR_RA)
#AIC(fit)

#AIC(lR_RA)

#AIC(lk_fitted)

lk_double_check<-lk_fitted<-likfit(rain_geo_rainfall2,data=rain_geo_rainfall2$data,coords=rain_geo_rainfall2$coords, trend =~covar1 , ini=c(0.5, 0.5),nospatial=TRUE)



AIC(lk_double_check)
















# create a grid of prediction locations
locs_range <- apply(rain_geo_rainfall2$coords, 2, range)  

pred_locs <- expand.grid(
  seq(locs_range[1,1], 
      locs_range[2,1],
      length = 30), 
  seq(locs_range[1,2], 
      locs_range[2,2], 
      length = 30))
names(pred_locs) <- c("x", "y")

border_locs <- chull(rain_geo_rainfall2$coords)
pred_locs_inside <- locations.inside(
  pred_locs, 
  border=rain_geo_rainfall2$coords[border_locs,])

ggplot(pred_locs_inside,
       aes(x = x, y = y)) +
  geom_point(col = "darkgrey", shape = 3) +
  geom_point(data = as.data.frame(rain_geo_rainfall2$coords),
             aes(x = Coord1, y = Coord2))

# find kriging predictions of the log(tcatch+1)
# at the prediction locations 

pred <- krige.conv(
  scallops_geo, 
  locations = pred_locs_inside, 
  krige=krige.control(type.krige="ok", 
                      obj.model = fit_spatial))

# plot the predicted values and standard errors
pred_dat <- data.frame(
  estimate = pred$predict,
  standard_error = sqrt(pred$krige.var),
  x = pred_locs_inside$x,
  y = pred_locs_inside$y)

p1 <- ggplot() +
  geom_raster(data = pred_dat,
              aes(x = x, y = y, fill = estimate)) +
  geom_point(data = as.data.frame(rain_geo_rainfall2$coords),
             aes(x = Coord1, y = Coord2)) +
  scale_fill_gradient(low = "#ffffb2", high = "#b10026") +
  guides(fill = guide_legend(title = NULL)) +
  ggtitle("Predicted log(tcatch + 1)")

p2 <- ggplot() +
  geom_raster(data = pred_dat,
              aes(x = x, y = y, fill = standard_error)) +
  geom_point(data = as.data.frame(rain_geo_rainfall2$coords),
             aes(x = Coord1, y = Coord2)) +
  scale_fill_gradient(low = "#ffffb2", high = "#b10026") +
  guides(fill = guide_legend(title = NULL))+
  ggtitle("Standard Errors")

grid.arrange(p1, p2, nrow = 2)





