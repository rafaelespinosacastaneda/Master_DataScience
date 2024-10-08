n<-50
k<-4
b<-0
y<-0
p<-0
l<-0
bb<-0
h<-0
fm<-0
sm<-0
nm<-0
mg<-0
vg<-0
pg<-0
rg<-0
ll<-matrix(nrow=(k-1),ncol=(k-1))
for(j in 1:(k-1))
{
  b[j]<--0.4+0.2*j
}
x<-0
y<-0
x<-rnorm(n)


df<-read.csv("D:/MAESTR�A_AUSTIN/RegressionAndPrediction/hw7/HW7_data.csv")
x<-df$x
y<-df$y


for(j in 1:k)
{
  nm[j]<-sum(ifelse(y==j,1,0))
  fm[j]<-sum(x*(ifelse(y==j,1,0)))
  sm[j]<-sum(x^2*(ifelse(y==j,1,0)))
  mg[j]<-fm[j]/nm[j]
  vg[j]<-sqrt((sm[j]-nm[j]*mg[j]^2)/(nm[j]))
}



for(j in 1:(k-1))
{
  bb[j]<-b[j]
}

for(t in 1:100)
{
  d<-0
  for(i in 1:n)
  {
    d[i]<-1+sum(exp(bb*x[i]))
  }
  
  for(j in 1:(k-1))
  {
    l[j]<-sum(x*ifelse(y==j,1,0))-sum(x*exp(bb[j]*x)/d)
    for(jj in 1:(k-1))
    {
      ll[j,jj]<--ifelse(j==jj,1,0)*sum(x^2*exp(bb[j]*x)/d)+sum(x^2*exp((bb[jj]+bb[j])*x)/d^2)
    }
  }
  
  bb<-bb-solve(ll)%*%l
  
}







print(bb)

d2<-0
x_prom<- sum(x)/n

d2<-1+sum(exp(bb*x_prom))



prob<-0
for(i in 1:(k-1))
{
  prob[i]<-exp(x_prom*bb[i])/d2
  
}

print(prob)


print(1-sum(prob))






