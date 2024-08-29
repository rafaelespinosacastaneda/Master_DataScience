### Instalar nuevas librerías
### install.packages("ggplot2")
### install.packages("ggmap")

library(tidyverse)
library(ggmap)
library(tibble) 
library(spatstat)


#pacman::p_load(ggmap, osmdata)
#chicago<-get_map(location = getbb("italy") ,source = "stamen",maptype = "satellite",zoom=7)




#ggmap(chicago)


bbox <- c(left = 6.993570, bottom = 36.886108, right = 22.484293, top = 43.776951)
ggmap(get_stamenmap(bbox, zoom = 8, maptype="terrain-background"), extent = "device")





