x= c(1.1,0.3,1,0.75,1.8)
y= c(4.5,2,5,2.4,4.8)
n=5
xy<- x*y
sxy<-sum(xy)
exy<-mean(xy)
sx<-sum(x^2)
print(sx)
sy<-sum(y^2)
print(xy)
xbar<-mean(x)
ybar<-mean(y)
vx<-var(x)
vy<-var(y)
print(vy)
cvxy<-cov(x,y)

corr_x_y_2<- (1/n-1)*(sxy-n*xbar*ybar)/sqrt(vx*vy)
print(corr_x_y_2)

corr_x_y <- cvxy/sqrt(vx*vy)
print(corr_x_y )

cxy<-cor(x,y)

print(cxy)

bh<-(n*sum(x*y)-n^2*xbar*ybar)/(n*sx-n^2*xbar^2)
ah<-ybar-bh*xbar
print(ah)
print(bh)


sigma_2 <- 1/(n-2)*sum((y-c(1,1,1,1,1)*ah-bh*x)^2)
print(sigma_2)
print(sx)
sigma<-sqrt(0.8)
z<-bh/(sigma*sqrt(1/(sx-n*xbar^2)))
print(z)
print((1-pnorm(z))*2)

sx_mean=sum((x-xbar)^2)
print(sx_mean)