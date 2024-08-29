k<-3
a<-0
nn<-0
y1<-0
y2<-0
y3<-0
f<-0


m<-5
a[1]<-0
a[2]<-0
a[3]<--0

n1<-5
n2<-5
n3<-4
n<-n1+n2+n3
y1<-cbind(8,9,6,8,5)
y2<-cbind(5,4,7,6,6)
y3<-cbind(9,3,2,4)
Y<-cbind(y1,y2,y3)
yb<-(sum(y1)+sum(y2)+sum(y3))/n
yb1<-sum(y1)/n1
yb2<-sum(y2)/n2
yb3<-sum(y3)/n3

f1<-sum(n1*(yb1-yb)^2)+sum(n2*(yb2-yb)^2)+sum(n3*(yb3-yb)^2)
f2<-sum((y1-yb1)^2)+sum((y2-yb2)^2)+sum((y3-yb3)^2)
f3<-sum((y1-yb)^2)+sum((y2-yb)^2)+sum((y3-yb)^2)
f<-(f1/2)/(f2/(n-3))

print(f1)
print(f2)
print(f)

1-pf(f,2,11)

print(n3*y3^2)