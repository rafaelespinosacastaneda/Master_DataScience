###################################################
#Importing the libraries to be used
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(astsa)
library(xts)
library(lubridate)

#Importing the data from the csv file
data1 <- read.csv("/Users/rafa/Documents/Master Austin/MAESTRÍA_AUSTIN/Advanced Predictive Models/HW2/file2.xls", header=TRUE, stringsAsFactors=FALSE)
data1_2 <- data.frame(Freq = data1$Freq,Date =as.Date(data1$Date))

# Apply kernel smoothing to the imported data 
a <- c(1/14, rep(1/7, 6), 1/14)

ma_smoothed_data <- data.frame(
  Freq = stats::filter(data1$Freq,
                         sides = 2,
                         filter = a),Date =as.Date(data1$Date))
#Plotting the data and the smoothed time series 
gg_data <-    ggplot(ma_smoothed_data,aes(x = Date, y = Freq,group=1))+ 
  geom_line(size = 3.5, color = "blue") +
  geom_point(shape = 21, size = 1, fill = "white", color = "black") +geom_line(data=data1_2,size=0.5,color="black")


gg_data


#----------------------------------------------------------------Part b--------------------------------------------------------------------------------------

#imorting the data from the csv file
data1 <- read.csv("/Users/rafa/Documents/Master Austin/MAESTRÍA_AUSTIN/Advanced Predictive Models/HW2/file2.xls", header=TRUE, stringsAsFactors=FALSE)
data1_2<-data.frame(Freq = data1$Freq,Date =as.Date(data1$Date),DayOfWeek=data1$DayOfWeek)
#the filtering weights 
a<-c(rep(c(1/7,0,0,0,0,0,0),6),1/7)
#filtered data
ma_smoothed_data <- data.frame(
  Freq = stats::filter(data1$Freq,
                       sides = 2,
                       filter = a),Date =as.Date(data1$Date),DayOfWeek=data1$DayOfWeek)
#Splitting the data by day
monday<-data1_2[data1_2$DayOfWeek=="Monday",]
tuesday<-data1_2[data1_2$DayOfWeek=="Tuesday",]
wednsday<-data1_2[data1_2$DayOfWeek=="Wednsday",]
thursday<-data1_2[data1_2$DayOfWeek=="Thursday",]
friday<-data1_2[data1_2$DayOfWeek=="Friday",]
saturday<-data1_2[data1_2$DayOfWeek=="Saturday",]
sunday<-data1_2[data1_2$DayOfWeek=="Sunday",]
# Splitting the smoothed data by day
monday_s<-ma_smoothed_data[ma_smoothed_data$DayOfWeek=="Monday",]
tuesday_s<-ma_smoothed_data[ma_smoothed_data$DayOfWeek=="Tuesday",]
wednsday_s<-ma_smoothed_data[ma_smoothed_data$DayOfWeek=="Wednsday",]
thursday_s<-ma_smoothed_data[ma_smoothed_data$DayOfWeek=="Thursday",]
friday_s<-ma_smoothed_data[ma_smoothed_data$DayOfWeek=="Friday",]
saturday_s<-ma_smoothed_data[ma_smoothed_data$DayOfWeek=="Saturday",]
sunday_s<-ma_smoothed_data[ma_smoothed_data$DayOfWeek=="Sunday",]
# Making that the levels appear in the ggplot in descendin order as Monday, Tuesday, Wednsday, Thursday , Friday, Saturday, Sunday
#instead of having alphabetical order
ma_smoothed_data$DayOfWeek <- factor(ma_smoothed_data$DayOfWeek, 
                                     levels=c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday"), 
                                     labels=c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday"))
#Making the plot 
gg_all <-    ggplot(ma_smoothed_data,aes(x = Date, y = Freq,group=DayOfWeek),colour=DayOfWeek)+geom_point()+geom_line(lwd=2,aes(color=DayOfWeek)) +
  geom_line(data=monday,size=0.5,aes(color=DayOfWeek))+geom_point(data=monday,aes(x=Date,y=Freq),shape = 21, size = 1, fill = "black", color = "black")+
  geom_line(data=tuesday,size=0.5,aes(color=DayOfWeek))+geom_point(data=tuesday,aes(x=Date,y=Freq),shape = 21, size = 1, fill = "black", color = "black")+
  geom_line(data=wednsday,size=0.5,aes(color=DayOfWeek))+geom_point(data=wednsday,aes(x=Date,y=Freq),shape = 21, size = 1, fill = "black", color = "black")+
  geom_line(data=thursday,size=0.5,aes(color=DayOfWeek))+geom_point(data=thursday,aes(x=Date,y=Freq),shape = 21, size = 1, fill = "black", color = "black")+
  geom_line(data=friday,size=0.5,aes(color=DayOfWeek))+geom_point(data=friday,aes(x=Date,y=Freq),shape = 21, size = 1, fill = "black", color = "black")+
  geom_line(data=saturday,size=0.5,aes(color=DayOfWeek))+geom_point(data=saturday,aes(x=Date,y=Freq),shape = 21, size = 1, fill = "black", color = "black") +
  geom_line(data=sunday,size=0.5,aes(color=DayOfWeek))+geom_point(data=sunday,aes(x=Date,y=Freq),shape = 21, size = 1, fill = "black", color = "black")+
  scale_x_date()+
  labs(title = "Car accidents in Texas",subtitle = "Smoothed Using a Moving Average Filter by Day of Week using a_4 filter")

gg_all


