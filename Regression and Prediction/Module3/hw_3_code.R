n<-100
x1<-0
x2<-0
y<-0
b1<-1
b2<--3
b3<-0
eh<-0
yh<-0
bh<-0
seh<-0
bhh<-0
yhh<-0
ehh<-0
f<-0



#-------------------------------------------------Model 1------------------------------------------------------------
xx<-matrix(nrow=n,ncol=2)
xxx<-matrix(nrow=n,ncol=3)
xxi<-matrix(nrow=3,ncol=3)

#x
x1<-2+rnorm(n)
#m
x2<-rnorm(n)

#y=b_1*x+b_2*m+b_3*x*m+error
y<-b1*x1+b2*x2+b3*x1*x2+rnorm(n)

#Matrix X  , colum1 is x (named here as x1) and column 2 is m (named here as x2)
xx[,1]<-x1
xx[,2]<-x2

#Bhat
bh<-solve(t(xx)%*%xx)%*%t(xx)%*%y
#yhat
yh<-xx%*%bh
#errors
eh<-y-yh

#-----------------------------------------------------Model 2 ---------------------------------------------------------
#X with interaction term
xxx[,1]<-x1
xxx[,2]<-x2
xxx[,3]<-x1*x2
xxi<-solve(t(xxx)%*%xxx)
bhh<-xxi%*%t(xxx)%*%y
yhh<-xxx%*%bhh
ehh<-y-yhh
#-----------------------------------------------------F statistic-------------------------------------------------------

f<-(n-3)*sum((y-yh)^2-(y-yhh)^2)/sum((y-yhh)^2)


#-----------------------------------------------------Mi parte---------------------------------------------------------

#Modelo y= b_1+b_2*m + b_3*x +error

df <- read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw3/data.csv")

y<-df$y
vec_ones<-rep(c(1),times=200)

x<-df$x
m<-df$m
X_med<- cbind(vec_ones,m,x)
#print(X_med)

#Bhat
bh<-solve(t(X_med)%*%X_med)%*%t(X_med)%*%y
#yhat
yh<-X_med%*%bh
#errors
eh<-y-yh
print(bh)

#-------------------------------------------------------------------------------------------------------------------------
#Modelo y= b_1+b_2*x +error

df <- read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw3/data.csv")

y<-df$y
vec_ones<-rep(c(1),times=200)

x<-df$x
m<-df$m
X_med<- cbind(vec_ones,x)
#print(X_med)

#Bhat
bh<-solve(t(X_med)%*%X_med)%*%t(X_med)%*%y
#yhat
yh<-X_med%*%bh
#errors
eh<-y-yh
print(bh)


#-------------------------------------------------------------------------------------------------------------------------
#Modelo y= b_1+b_2*m +error

df <- read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw3/data.csv")

y<-df$y
vec_ones<-rep(c(1),times=200)

x<-df$x
m<-df$m
X_med<- cbind(vec_ones,m)
#print(X_med)

#Bhat
bh<-solve(t(X_med)%*%X_med)%*%t(X_med)%*%y
#yhat
yh<-X_med%*%bh
#errors
eh<-y-yh
print(bh)

#-----------------------------------------------------------------------------------

# m=b_21 + b_22 x_i 

df <- read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw3/data.csv")

y<-df$y
vec_ones<-rep(c(1),times=200)

x<-df$x
m<-df$m
X_med<- cbind(vec_ones,x)
#print(X_med)

#Bhat
bh<-solve(t(X_med)%*%X_med)%*%t(X_med)%*%m
C_js<-solve(t(X_med)%*%X_med)
#yhat
mh<-X_med%*%bh
#errors
eh<-m-mh
#print(bh)
print(C_js)
#print(C_js*1)

#-----------------------------------------------------------------------------------

#Modelo y= b_31+b_32*m + b_33*x +error

df <- read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw3/data.csv")

y<-df$y
vec_ones<-rep(c(1),times=200)

x<-df$x
m<-df$m
X_med<- cbind(vec_ones,m,x)
#print(X_med)

#Bhat
bh<-solve(t(X_med)%*%X_med)%*%t(X_med)%*%y

C_js<-solve(t(X_med)%*%X_med)


#yhat
yh<-X_med%*%bh
#errors
eh<-y-yh
print(C_js)
#---------------------------------------------------

df <- read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw3/data.csv")

y<-df$y
vec_ones<-rep(c(1),times=200)

x<-df$x
m<-df$m

X_med_x<- cbind(vec_ones,x)
X_med_m<- cbind(vec_ones,x)
X_med_mx<- cbind(vec_ones,m,x)


bh_x<-solve(t(X_med_x)%*%X_med_x)%*%t(X_med_x)%*%y
bh_m<-solve(t(X_med_m)%*%X_med_m)%*%t(X_med_m)%*%m
bh_mx<-solve(t(X_med_mx)%*%X_med_mx)%*%t(X_med_mx)%*%y

b_11<-bh_x[1]
b_12<-bh_x[2]
b_21<-bh_m[1]
b_22<-bh_m[2]
b_31<-bh_mx[1]
b_32<-bh_mx[2]
b_33<-bh_mx[3]
print(b_11)
print(b_12)
print(b_21)
print(b_22)
print(b_31)
print(b_32)
print(b_33)
C_js_m<-solve(t(X_med_m)%*%X_med_m)
C_js_mx<-solve(t(X_med_mx)%*%X_med_mx)
z=(b_12-b_33)/sqrt(b_22^2*C_js_mx[2,2]+b_32^2*C_js_m[2,2])
print(z)

print((1-pnorm(z))*2)

print(C_js_m[2,2])
print(C_js_mx[2,2])
