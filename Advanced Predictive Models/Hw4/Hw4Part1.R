### Instalar nuevas librerías
### install.packages("ggplot2")
### install.packages("ggmap")

library(tidyverse)
library(ggmap)
library(tibble) 
library(spatstat)
library(sf)

#pacman::p_load(ggmap, osmdata)
#chicago<-get_map(location = getbb("italy") ,source = "stamen",maptype = "satellite",zoom=7)




#ggmap(chicago)


bbox <- c(left = 12.620544, bottom = 42.602125, right = 13.599014, top = 42.974511)



#left = 12.620544, bottom = 42.521206, right = 13.599014, top = 42.894076
#left = 9.854736, bottom = 41.302571, right = 17.682495, top = 44.280604
#left = 9.853084, bottom = 40.639146, right = 17.680843, top = 43.648172
#left = 9.854736, bottom = 40.971604, right = 17.682495, top = 43.965145

#9.854736,41.302571,17.682495,44.280604

#ggmap(get_stamenmap(bbox, zoom = 8, maptype="terrain-background"), extent = "device")
earthquakes_italy<-read_csv("/Users/rafa/Documents/Master Austin/MAESTRIA_AUSTIN/Advanced Predictive Models/Hw4/Italy_earthquakes.csv")


min_lat <-100000
max_lat<-0


min_lon <-100000
max_lon<-0


for (i in 1:length(earthquakes_italy$Latitude))
{
  if (earthquakes_italy$Latitude[i]<min_lat)
  {
    min_lat<-earthquakes_italy$Latitude[i]
  }
    
  if (earthquakes_italy$Latitude[i]>max_lat)
  {
    max_lat<-earthquakes_italy$Latitude[i]
  }
  
  
  if (earthquakes_italy$Longitude[i]<min_lon)
  {
    min_lon<-earthquakes_italy$Longitude[i]
  }
  
  if (earthquakes_italy$Longitude[i]>max_lon)
  {
    max_lon<-earthquakes_italy$Longitude[i]
  }
  
}


ggmap(get_stamenmap(bbox, zoom =10), extent = "device")+
  geom_point(data = earthquakes_italy,
             aes(x = Longitude, y = Latitude),
             color = "blue",
             size = 1) +
  ggtitle("Puntos de muestreo") +
  labs (x = "longitud", y = "latitud")



ppp_earthquakes<-ppp(earthquakes_italy$Longitude, earthquakes_italy$Latitude, c(min_lon,max_lon), c(min_lat,max_lat))

 

# exploratory analysis -- quadrant count
Q <- quadratcount(ppp_earthquakes, 
                  nx = 6, 
                  ny = 6) 


Q


plot(ppp_earthquakes, 
     cex = 0.5, 
     pch = "+", 
     main="Earthquakes locations")
plot(Q,add = TRUE, cex = 2)




################################################### 
# TESTING FOR CSR (chi-squire test)

ppp_earthquakes.quadtest <- quadrat.test(ppp_earthquakes, 
                             nx = 6, 
                             ny = 6)  # how does number of quadrants affect results?
ppp_earthquakes.quadtest


summary(ppp_earthquakes.quadtest)


plot(ppp_earthquakes, 
     cex = 0.5, 
     pch = "+", 
     main="Earthquakes locations")
plot(ppp_earthquakes.quadtest$expected,add = TRUE, cex = 2)


ppp_earthquakes.quadtest$expected


ppp_earthquakes.quadtest$observed 
