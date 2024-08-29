df <- read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw6/hw6_data.csv")


program<-df$program
gender<-df$gender
grade<-df$grade

twoANOVA<- aov(grade ~ program*gender,data=df)

summary(twoANOVA)