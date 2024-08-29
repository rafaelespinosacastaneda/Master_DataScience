x= c(1.1,0.3,1,0.75,1.8)
y= c(4.5,2,5,2.4,4.8)
n=5
sx<-sum(x^2)
print(sx)
sy<-sum(y^2)
xbar<-mean(x)
x_0<-x-xbar
s_x0<-sum(x_0^2)
ybar<-mean(y)
bh<-(n*sum(x*y)-n^2*xbar*ybar)/(n*sx-n^2*xbar^2)
ah<-ybar-bh*xbar
sx_mean=sum((x-xbar)^2)
sigma_h <- 1/(n-2)*sum((y-c(1,1,1,1,1)*ah-bh*x)^2)

t=bh*sqrt(sx_mean)/sigma_h
print(t)
print((1-pt(t,3))*2)
e<-y-c(1,1,1,1,1)*ah-bh*x
e1<-e[1]
print(e1)


h<-1/n+x_0^2/s_x0

ve<- 1-h
print(ve[1])
ste<-e/(sigma_h*sqrt(1-h))
print(ste[1])
print(h)
sigma<-sqrt(0.8)
x_touse<-0.5
h_pred<-1/n+(x_touse^2)/s_x0
y_pred<-ah+bh*x_touse
interval_min<-y_pred-sigma*sqrt(h_pred)*abs(qnorm(0.025))
interval_max<-y_pred+sigma*sqrt(h_pred)*abs(qnorm(0.025))
print(interval_min)
print(interval_max)