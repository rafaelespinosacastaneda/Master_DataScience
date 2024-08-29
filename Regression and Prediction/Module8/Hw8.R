n<-100

l<-0
c<-0
x<-0
y<-0
t<-0
tt<-0
v1<-0
v2<-0
z<-0
zz<-0
v3<-0
yh<-0
eh<-0
yy<-0
xx<-0
q<-0
b<-0
ttt<-0
bb<-0
yh<-0
eh<-0
lntt_x<-0
m<-0
lambda<-0
evar<-0

# original data

df<-read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw7/HW7_data.csv")

#Data 

for(i in 1:10)
{
  x[i]<- (6*i/10)-3
  
}
t<-0

for(k in 2:100000)
{
  u= 1551-sum(x*exp(t*x))
  
  d=sum(-1*x*x*exp(t*x))
  t<-t-u/d
}

lambda<-exp(t*x)

for (i in 1:10)
{
  m[i]<-lambda[i]^i*exp(-lambda[i])/factorial(i)
  
}


#m<-exp(t*x)
#sigma_p1<-sum((y-m)*(y-m))
#sigma<-sqrt(sigma_p1/99)
#print(sigma)
print(t)
#taking 100 samples


p1<-sum(x*x*exp(t*x))
p2<-(sum(x*x*exp(t*x)))*(sum(x*x*exp(t*x)))
evar<-p1/p2
print(evar)

statistic<- abs(1.99-2)/(sqrt(evar)/sqrt(10))
print(statistic)


print((1-(pnorm(statistic)))*2)    

print(   2*(pnorm(1.99,mean=2,sd=sqrt(evar)))          )