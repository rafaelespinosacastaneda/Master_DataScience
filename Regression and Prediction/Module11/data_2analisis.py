# -*- coding: utf-8 -*-
"""
Created on Thu Apr 28 22:33:19 2022

@author: rafae
"""

import pandas as pd 
import math 
import numpy as np 


def beta_hat(x):
    x_sum= sum(x)
    beta=0
    for t in range(100):
        
    
    #Calculo de derivada
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
                factor1=math.exp(x[j]*beta*x[j]**2)
                sum_factor1=sum_factor1+factor1
                factor2=math.exp(x[j]*beta)
                sum_factor2=sum_factor2+factor2
                factor3=math.exp(x[j]*beta)*x[j]
                sum_factor3=sum_factor3+factor3
        
            numerator=sum_factor1*sum_factor2-sum_factor3**2
            denominator=sum_factor2**2
        
            second_derivative=-1*second_derivative+numerator/denominator
    
        beta=beta-derivative/second_derivative    
        print(beta)
        return beta 
    


df=pd.read_csv("data2.csv")

x=df["x"]
beta_hat(x)

"""
x_sum= sum(x)
beta=0
for t in range(100):
    
    #Calculo de derivada
    
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
            factor1=math.exp(x[j]*beta*x[j]**2)
            sum_factor1=sum_factor1+factor1
            factor2=math.exp(x[j]*beta)
            sum_factor2=sum_factor2+factor2
            factor3=math.exp(x[j]*beta)*x[j]
            sum_factor3=sum_factor3+factor3
        
        numerator=sum_factor1*sum_factor2-sum_factor3**2
        denominator=sum_factor2**2
        
        second_derivative=-1*second_derivative+numerator/denominator
    
    beta=beta-derivative/second_derivative    
print(beta)

            
t=[]
for i in range(len(x)):
    t.append(0)
    
for i in range(len(x)):
    t[i]=np.random.exponential(scale=1/math.exp(beta*x[i]))

df=pd.DataFrame({"t":t,"x":x})

print(df)

df = df.sort_values(by ='t' )
"""
