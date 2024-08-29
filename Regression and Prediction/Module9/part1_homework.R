x<-0
W<-matrix(0,nrow=10,ncol=10)
p_i<-0
beta<- -0.34
for (i in 1:10)
  {
  x[i]<-i/10
  p_i<- exp(x[i]*beta)/(1+exp(x[i]*beta))
  W[i,i]<- p_i
}


print(W)
variance<-solve(t(x)%*%W%*%x)

print(variance)
  

print(-0.34/0.5989221)



statistic<- -0.34/sqrt(0.5989221)

print((pnorm(statistic))*2)
