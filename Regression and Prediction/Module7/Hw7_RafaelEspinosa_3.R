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
t_x<-0


# original data

df<-read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw7/HW7_data.csv")

#Data 

y<-df$y
x<-df$x
t<-1

for(k in 2:100000)
{
  u= -sum(  (y-exp(t*x))*x*exp(t*x)  )
  
  p11= -1*(x*exp(t*x))*(x*exp(t*x))
  #p22=(y-exp(t*x))*exp(t*x)
  #p33=x*x*(y-exp(t*x))*exp(t*x)
  #d=-1*sum(p11+p22+p33)
  p22=(y-exp(t*x))*(1+x*x)*exp(t*x)
  d=-1*sum(p11+p22)
  t<-t-u/d
}

m<-exp(t*x)
sigma_p1<-sum((y-m)*(y-m))
sigma<-sqrt(sigma_p1/99)
print(sigma)
print(t)
#taking 100 samples
for(kk in 1:100)
{

for (i in 1:100)
  {
  yyy[i]<-exp(t*x[i])+sigma*rnorm(1)   
 }
 

# initial value of theta 
tt[1]<-1

k<-2
difference<-1
#while( difference >0.0000001) 

while(k<=1000)
{  

	u=-sum((yyy-exp(tt[k-1]*x))*x*exp(tt[k-1]*x))
	
	p1= -1*x*x*exp(tt[k-1]*x)*exp(tt[k-1]*x)
	
	p2=(yyy-exp(tt[k-1]*x))*exp(tt[k-1]*x)
	p3=x*x*(yyy-exp(tt[k-1]*x))*exp(tt[k-1]*x)
	d=-sum(p1+p2+p3)
	#estimation of theta by Newton Raphson
	tt[k]<-tt[k-1]-u/d

	difference<-abs(tt[k]-tt[k-1])
	k<- k+1
}

m_x<-mean(x)
print(m_x)
b[kk]<-tt[k-1]

t_x[kk]<-tt[k-1]*m_x

}

variance_t<-var(b)
variance_t_x<-var(t_x)

print(b)
print(variance_t)
print(variance_t_x)

