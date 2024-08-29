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
x<-2*runif(n)
y<-log(2+x)+0.1*rnorm(n)
t<-3
for(k in 2:10)
{
	u<--sum(y/(t+x)-(log(t+x)/(t+x)))
	d<-sum((1+y)/(t+x)^2-(log(t+x))/(t+x)^2 )
	t<-t-u/d
}

yh<-log(t+x)
eh<-y-yh




#taking 100 samples
for(kk in 1:100)
{
  

q<-c(1:n)

#bootstrap samples
qq<-sample(q,replace=TRUE)
xx<-x[qq]
yy<-y[qq]
print(x[qq])
print("Una iteracion")
#second method using the model 
yyy<-log(t+x)+0.1*rnorm(n)

# initial value of theta -> estimator of the second model 
tt[1]<-3

#initial value of theta -> estimator of the first model 
ttt[1]<-3
for(k in 2:10)
{
  
  # l'(theta)
	u<--sum(yy/(tt[k-1]+xx)-(log(tt[k-1]+xx)/(tt[k-1]+xx)))
	#l´´(theta)
	d<-sum((1+yy)/(tt[k-1]+xx)^2-(log(tt[k-1]+xx))/(tt[k-1]+xx)^2 )
	#estimation of theta by Newton Raphson
	tt[k]<-tt[k-1]-u/d
	
	#second method 
	uu<--sum(yyy/(ttt[k-1]+x)-(log(ttt[k-1]+x)/(ttt[k-1]+x)))
	dd<-sum((1+yyy)/(ttt[k-1]+x)^2-(log(ttt[k-1]+x))/(ttt[k-1]+x)^2 )
	#estimation of theta by Newton Raphson
	ttt[k]<-ttt[k-1]-uu/dd
	
}
# sample values of log(1+tt)  
b[kk]<-exp(tt[10])
# sample values of log(1+tt)
bb[kk]<-log(ttt[10]+1)
}
print(b[kk])
print(bb[kk])



