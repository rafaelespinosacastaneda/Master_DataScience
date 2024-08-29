df<-read.csv("D:/MAESTRÍA_AUSTIN/RegressionAndPrediction/hw10/Hw_10_data.csv")
x<-0
y<-0
y<-df$y

x<-df$x
h<-0.1
k<-0

for (i in 1:100)
{
  if ( abs(-x[i]/h)< 1)
  {
      k[i]<-0.5
  }
  else{
    k[i]<-0
  }
}

print(k)

m_hat<-sum(y*k)/sum(k)

print (m_hat)






sum_x<-0
sum_1<-0
for (i in 1:100)
{
  if ( abs(-x[i])< h)
  {
    sum_x<-sum_x+x[i]
    sum_1<- sum_1+1
  }
}



bias_m<- 1.2*(sum_x/sum_1)

print(bias_m)





sum_x_ones<-0
for (i in 1:100)
{
  if ( abs(x[i])< h)
  {
    sum_x_ones<-sum_x_ones+1
  }
}

variance_factor<-0
for (i in 1:100)
{
  if ( abs(x[i])< h)
  {
    variance_factor<-variance_factor+1/(sum_x_ones*sum_x_ones)
  }
}

print(variance_factor)





alpha_2<-abs(qnorm(0.025))
print(alpha_2)
sqrt_var<-sqrt(variance_factor*0.1*0.1)



print(m_hat-sqrt_var*alpha_2)
print(m_hat+sqrt_var*alpha_2)













