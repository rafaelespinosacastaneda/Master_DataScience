### Instalar nuevas librerías
### install.packages("ggplot2")
### install.packages("ggmap")

library(tidyverse)
library(ggmap)
library(tibble) 
library(spatstat)


pacman::p_load(ggmap, osmdata)
chicago<-get_map(location = getbb("chicago") ,source = "stamen")


ggmap(chicago)

chicago_points<-read_csv("/Users/rafa/Documents/Master Austin/MAESTRIA_AUSTIN/Advanced Predictive Models/Hw4/Hw4_data_chicago.xls")

lon_chicago<-chicago_points$lon
lat_chicago<-chicago_points$lat


ggmap(chicago)+
  geom_point(data = chicago_points,
             aes(x = lon, y = lat),
             color = "red",
             size = 2) +
  ggtitle("Puntos de muestreo") +
  labs (x = "longitud", y = "latitud")



#---------------------------------------------------------

load("/Users/rafa/Documents/Master Austin/MAESTRIA_AUSTIN/Advanced Predictive Models/Hw4/Hw4Part2Data.RData")



class(markets_ppp)

# exploratory analysis -- scatterplot
plot(markets_ppp, 
     main="Markets locations in Chicago")


fit3 <- ppm(markets_ppp, ~ polynom(x, y, 2))

plot(fit3, 
     how = "image", 
     se = FALSE, 
     col = grey(seq(1,0,length=128)))

print("The parameters")
fit3

pred_forcast <- data.frame(x = 443674, y = 4636999)


prediction<-predict(fit3,type="cif",locations=pred_forcast)


prediction
