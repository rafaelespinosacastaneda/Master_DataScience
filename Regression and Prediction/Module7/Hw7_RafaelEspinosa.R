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


# original data

df<-read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw7/HW7_data.csv")

#Data 

y<-df$y
x<-df$x
t<-1

for(k in 2:10000)
{
  u<- sum((y-exp(t*x))*x*exp(t*x))
  
  p11<- -1*x*x*exp(t*x)*exp(t*x)
  p22<-(y-exp(t*x))*exp(t*x)
  p33<-x*x*(y-exp(t*x))*exp(t*x)
  d<-sum(p11+p22+p33)
  #print(d)
  #print(u/d)
  t<-t-u/d
}

#print(t)

m<-exp(t*x)
sigma_p1<-sum((y-m)*(y-m))
sigma<-sqrt(sigma_p1/99)



print(sigma)
#taking 100 samples
for(kk in 1:100)
{
for (i in 1:100)
{
yy[i]<-rnorm(1, mean = exp(t*x[i]), sd =sigma )
xx[i]<-x[i]
}


#bootstrap samples
#qq<-sample(q,replace=TRUE)
#xx<-x[qq]
#yy<-y[qq]

#second method using the model 
yyy<-exp(t*x)+0.1*rnorm(n)

# initial value of theta -> estimator of the second model 
tt[1]<-1
#initial value of theta -> estimator of the first model 
ttt[1]<-1
k<-2
difference<-1
while( difference >0.00000001) 
{  

	u<-sum((yy-exp(tt[k-1]*xx))*xx*exp(tt[k-1]*xx))
	
	p1<- -1*xx*xx*exp(tt[k-1]*xx)*exp(tt[k-1]*xx)
	
	p2<-(yy-exp(tt[k-1]*xx))*exp(tt[k-1]*xx)
	p3<-xx*xx*(yy-exp(tt[k-1]*xx))*exp(tt[k-1]*xx)
	d<-sum(p1+p2+p3)
	#estimation of theta by Newton Raphson
	tt[k]<-tt[k-1]-u/d
	
	#second method 
	#uu<--sum(yyy/(ttt[k-1]+x)-(log(ttt[k-1]+x)/(ttt[k-1]+x)))
	#dd<-sum((1+yyy)/(ttt[k-1]+x)^2-(log(ttt[k-1]+x))/(ttt[k-1]+x)^2 )
	#estimation of theta by Newton Raphson
	#ttt[k]<-ttt[k-1]-uu/dd
	difference<-abs(tt[k]-tt[k-1])
	k<-k+1
	
}
  

b[kk]<-exp(tt[k])
# sample values of log(1+tt)
#bb[kk]<-log(ttt[10]+1)
}
#print(bb[kk])
print(tt)


