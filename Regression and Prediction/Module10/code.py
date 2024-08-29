import pandas as pd
import numpy as np
import matplotlib 

df=pd.read_csv("Hw_10_data.csv")

df.sort_values(by=["x"])

x=df["x"]
y=df["y"]
r=[]
m1=[]
m2=[]
x_r=[]
y_r=[]
t_r=[]
first_sum=[]
second_sum=[]

for  i in range(len(x)-1):
   r.append((x[i]+x[i+1])/2)



for i in range(len(r)):
    count_m1 = np.count_nonzero(x<= r[i])
    count_m2 = np.count_nonzero(x> r[i])
    x_r=x[x<=r[i]]
    y_r=y[x<=r[i]]
    m1.append((1/count_m1)*sum(y_r))
    
    first_sum.append(y_r-m1[-1])
    
    x_r2=x[x>r[i]]
    y_r2=y[x>r[i]]
    m2.append((1/count_m2)*sum(y_r2))
    second_sum.append(y_r2-m2[-1])
for i in range(len(r)): 
     t_r.append(sum(first_sum[i]**2)+sum(second_sum[i]**2))
matplotlib.pyplot.scatter( r,t_r)

count_m_hat1= np.count_nonzero(x<=0.1)
count_m_hat2= np.count_nonzero(x>0.1)

x_hat=x[x<=0.2]
y_hat=y[x<=0.2]
mhat1=1/count_m_hat1*sum(y_hat)
mhat2=1/count_m_hat2*sum(y_hat)
print(mhat1)
print(mhat2)
