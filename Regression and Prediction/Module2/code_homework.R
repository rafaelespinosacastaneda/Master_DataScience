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



df <- read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw2/data.csv")

y<-df$y
x_1<-df$x1
x_2<-df$x2
x_3<-df$x3
xx<- cbind(x_1,x_2,x_3)


xx_no3<-cbind(x_1,x_2)
xi_no3<-solve(t(xx_no3)%*%xx_no3)
bh_no3<-xi_no3%*%t(xx_no3)%*%y
yh_no3<-xx_no3%*%bh_no3
eh_no3<-y-yh_no3

print(eh_no3)

xi<-solve(t(xx)%*%xx)
bh<-xi%*%t(xx)%*%y
yh<-xx%*%bh
eh<-y-yh

sum_squares_no3<- t(eh_no3)%*%eh_no3
sum_squares<- t(eh)%*%eh
print(sum_squares)

F_1_27<- (sum_squares_no3-sum_squares)/(sum_squares/27)

ht<-xx%*%xi%*%t(xx)
hd<-diag(ht)

for(i in 1:n)
{
es[i]<-eh[i]/sqrt(1-hd[i])
}
print(bh)


print(xi[3,3])


print(qf(0.99,1,27))

t_27=(bh[3]/sqrt(xi[3,3]))/sqrt(sum_squares/27)
print(t_27)
print(qt(0.99,27))
print((1-pt(t_27,27))*2)

print(ht[1,1])
print(ht[2,2])

to_predict=c(1,0.12,0.56)
prediction=to_predict%*%bh
print(prediction)
sigma_hat=sqrt((t(y-yh)%*%(y-yh))/27)
print(sigma_hat)
t_0.025_27=qf(0.975,1,27)

print(qt(0.95,95))
raiz_xx=sqrt(t(to_predict)%*%xi%*%to_predict)

lower_interval=prediction-sigma_hat*t_0.025_27*raiz_xx
upper_interval=prediction+sigma_hat*t_0.025_27*raiz_xx

print(lower_interval)
print(upper_interval)
