# number of parameters
p<-5
#numbeer of data points
n<-100
#matrix X
xx<-0
#matrix (X'X)^-1
xi<-0
#estimator bhat 
bh<-0
#values of dependant variable
y<-0
# estimator hat y 
yh<-0

eh<-0
rs<-0
t<-0
f<-0
m<-100
#hat matrix
ht<-0
#studiantized residuals
es<-0
#hat matrix 
hd<-0
l<-0
b<-c(2,-1,3,-2,0)
xx<-matrix(nrow=n,ncol=p)
xi<-matrix(nrow=p,ncol=p)
ht<-matrix(nrow=n,ncol=n)



df <- read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw4/data.csv")

y<-df$y
x_1<-df$x1
x_2<-df$x2
x_3<-df$x3
xx<- cbind(x_1,x_2,x_3)


xx_no23<-cbind(x_1,x_2)
#(X^'X)^-1
xi_no23<-solve(t(xx_no23)%*%xx_no23)
bh_no23<-xi_no23%*%t(xx_no23)%*%y
yh_no23<-xx_no23%*%bh_no23
eh_no23<-y-yh_no3
#X^'X
xt_x_no23<-t(xx_no23)%*%xx_no23

factor2<-(t(bh_no23)%*% xt_x_no23%*%bh_no23)/(1+0.01)

  
print(eh_no23)



xi<-solve(t(xx)%*%xx)
bh<-xi%*%t(xx)%*%y
yh<-xx%*%bh
eh<-y-yh

#X^'X
xt_x<-t(xx)%*%xx

factor1<-(t(bh)%*% xt_x%*%bh)/(1+0.01)

result<-0.5*(factor1-factor2)+0.5*log(0.01/1.01)

print(result)

to_predict2<-c(1,0.12,0.56)
tilde_beta<- bh/(1+0.01)

exp_y<-t(to_predict2)%*% tilde_beta

print(exp_y)

var_y <-1+1/(1+0.01)*t(to_predict2)%*%xi%*%to_predict2

print(var_y)

print(bh/1.01)
print(bh)
print(xi/(1.01))

print((length(y)*mean(y))/(length(y)+0.01))