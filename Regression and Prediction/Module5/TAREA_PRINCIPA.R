n<-200
x<-0
y<-0
p<-5
z<-0

a<-1
b<-3
#estimator
bh<-0
bb<-0
bl<-0
xvi<-matrix(nrow=n-1,ncol=2)
yi<-0
gg<-0
qqq<-0
#fitted values
yh<-0
#residuals
eh<-0

#----------------------------------------------------------------------------Largest eigenvalue estimators----------------------------------------
qqq<-0
bbb<-0
yhh<-0
ehh<-0

#---------------------------------------------------------------------------Matrices for principal component analysis--------------------------
pp<-matrix(nrow=n,ncol=p)
qq<-matrix(nrow=p,ncol=p)
dd<-matrix(nrow=n,ncol=p)
for(i in 1:n)
{
	for(j in 1:p)
	{
		dd[i,j]<-0
	}
}
d<-0

# Random numbers 
x<-runif(n)

xv<-matrix(nrow=n,ncol=5)


df <- read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw5/data.csv")

#Data 

y<-df$y
x_1<-df$x1
x_2<-df$x2
x_3<-df$x3
x_4<-df$x4
x_5<-df$x5
xx<- cbind(x_1,x_2,x_3,x_4,x_5)





# XV has x1 and x2 values 
xv[,1]<-x_1
xv[,2]<-x_2
xv[,3]<-x_3
xv[,4]<-x_4
xv[,5]<-x_5





#xx<-matrix(nrow=2,ncol=2)
#xx1<-matrix(nrow=2,ncol=2)
xi<-matrix(nrow=2,ncol=2)

#xx1[1,1]<-n
#xx1[1,2]<-n*mean(x)
#xx1[2,1]<-xx1[1,2]
#xx1[2,2]<-sum(x^2)


#X´X
xx<-t(xv)%*%xv

#(X´X)^-1
xi<-solve(xx)

#beta hat estimation
bh<-xi%*%t(xv)%*%y


#SVD 
pp<-t(svd(xv,nu=n,nv=p)$u)
qq<-t(svd(xv,nu=n,nv=p)$v)
d<-svd(xv,nu=n,nv=p)$d

for(i in 1:p)
{
	dd[i,i]<-d[i]
}	

#gamma
gg<-0

# z for principal component analysis
z<-pp%*%y


for(j in 1:p)
{
gg[j]<-z[j]/d[j]
}


# ------------------------------------------------------------------All estimators---------------------------------------------------------------- 
bb<-t(qq)%*%gg
yh<-xv%*%bb
eh<-y-yh

#print(gg)

#print(bb)
#------------------------------------------------------------------Using the largest eigenvalue---------------------------------------------------- 

#gamma
qqq<-qq[1,]
#estimator beta
bbb<-qqq*gg[1]
#predicted value
yhh<-xv%*%bbb
# residuals
ehh<-y-yhh
#---------------------------------------------------------------------Using the 4 largest eigenvalues-------------------------------------------

q4<-cbind(qq[1,],qq[2,],qq[3,],qq[4,])
gg4<-t(cbind(gg[1],gg[2],gg[3],gg[4]))
print(gg4)
print(q4)
print(qq)
b4<-q4%*%gg4
#predicted value
yh4<-xv%*%bbb
# residuals
eh4<-y-yhh

print(q4)
print(b4)
#--------------------------- Keping the smallest

todos<-d[1]^2+d[2]^2+d[3]^2+d[4]^2+d[5]^2
c1<- d[1]^2/todos
c2<-(d[1]^2+d[2]^2)/todos
c3<-(d[1]^2+d[2]^2+d[3]^2)/todos
c4<-(d[1]^2+d[2]^2+d[3]^2+d[4]^2)/todos


print(c1)
print(c2)
print(c3)
print(c4)

#------------------------------------------------------------Using the 2 largest eigenvalues-------------------------------------------

q2<-cbind(qq[1,],qq[2,])
gg2<-t(cbind(gg[1],gg[2]))
print(gg2)
print(q2)
b2<-q2%*%gg2
#predicted value
yh2<-xv%*%b2
# residuals
eh2<-y-yhh

print(q2)
print(b2)
