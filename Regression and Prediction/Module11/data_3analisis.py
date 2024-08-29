# -*- coding: utf-8 -*-
"""
Created on Thu Apr 28 22:33:19 2022

@author: rafae
"""

import pandas as pd 
import math 
import numpy as np 



def beta_hat(beta_0,x):
    x_sum= sum(x)
 
    beta=beta_0
    for t in range(100):
        total_sum=0
        for  i in range(len(x)):
            sum_num=0
            sum_den=0
            for j in range(i,len(x)):
                num=math.exp(x[j]*beta)*x[j]
                sum_num=sum_num+num
                den=math.exp(x[j]*beta) 
                sum_den=sum_den+den 
            total_sum=total_sum+ sum_num/sum_den
        derivative= x_sum-total_sum
        sum_factor1=0
        sum_factor2=0
        sum_factor3=0
        second_derivative=0
        for i in range (len(x)):
            sum_factor1=0
            sum_factor2=0
            sum_factor3=0
            for j in range(i,len(x)):
                factor1=math.exp(x[j]*beta)*x[j]**2
                sum_factor1=sum_factor1+factor1
                factor2=math.exp(x[j]*beta)
                sum_factor2=sum_factor2+factor2
                factor3=math.exp(x[j]*beta)*x[j]
                sum_factor3=sum_factor3+factor3
            numerator=sum_factor1*sum_factor2-sum_factor3**2
            denominator=sum_factor2**2
            second_derivative=second_derivative-numerator/denominator
        beta=beta-derivative/second_derivative
    return beta 
    

# Data Frame with already sorted values( sorted
# in the excel file)
df=pd.read_csv("data2.csv")

x=df["x"]
b_0=0
beta=beta_hat(b_0,x)
print(beta)
            

 
beta_samples=[]
for samples in range(100):
    t=[]

    
    for i in range(len(x)):
        t.append(0)
    
    for i in range(len(x)):
        t[i]=np.random.exponential(scale=1/math.exp(beta*x[i]))
        #t[i]=np.random.exponential()
        
    df=pd.DataFrame({"t":t,"x":x})
    
    df2=df.sort_values(by=["t"])
    x_sample=df2["x"]

    beta_samples.append(beta_hat(beta,x_sample))
    
print(beta_samples)
variance=np.var(beta_samples)
print(variance)
