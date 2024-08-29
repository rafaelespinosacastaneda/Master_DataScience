
##################################
#Calculating power:complex
##################################

library(pwr)

##################################
#Example
##################################

#For the mode experiment, how many patients do I need to sample from each of 10 hospitals to have 80% power to detect mode effects of small effect size magnitude?

# "guess and check"
#Let's say I sample 100 patients from each hospital. 
#I need to assume a response rate because the mode effects are only testable among survey responders. First, I need to think about post-sampling ineligbility - people who have died, people who are ineligible for other reasons. Let's assume 5% are ineligible. This leaves me with 95 people per hospital. I also need to assume a response rate. Based on prior studies, let's assume a RR of 20%. This means I end up with 95*0.2 = 19 responders per hospital. 

#Now I need to account for the clustering nature of this data - people are clustered with hospitals, much like repeated longitudinal measurements are clustered within people. I need to use some estimate of the ICC. 

#see this paper: Ye, F., Parast, L., Hays, R. D., Elliott, M. N., Becker, K., Lehrman, W. G.et al. (2022). Development and validation of a patient experience of care survey for emergency departments. Health services research, 57(1), 102-112.
#https://onlinelibrary.wiley.com/doi/full/10.1111/1475-6773.13853?casa_token=4lwfXaJ8x_sAAAAA%3AfvHBE8zEiCPLI4PG9h_xXWkMZAssWEVr0YEH15rZz9CIlqA2Ju986Njw66zLWwl2JRpGzvhrA1IA7QA 
#"we estimated intra-class correlations (ICC; an estimate of the degree of similarity of responses from patients who received care from the same ED) of the items analyzed; item ICCs ranged from 0.004 to 0.045. "
#using 0.045 is conservative

#so what is our design effect?
DE = 1+ (19-1)*0.045

ESS = 19/DE
ESS

#so my effective sample size is 10 per hospital. Let's say I randomize half to mail and half to phone, that is 5 per mode in each hospital. 
#so now I can simplify to basically a two-sample t-test with n1 = 5*10 vs. n2= 5*10

#what is my power to detect a small effect size?
pwr.t.test(n=5*10, d=0.2)
#not good

#what sample size do I need for 80% power
pwr.t.test(power=0.80, d=0.2)

#I need 394 people per mode
#response rate and ICC should be considered fix, don't mess with those. But the number you sample per hospital and the # of hospitals could vary. You can do the algebra to work backwards but let's guess and check again assuming I sample 200 patients from each of 50 hospitals. 

n.responders = 200*0.95*.2
n.responders
DE = 1+ (n.responders-1)*0.045
ESS = n.responders/DE

pwr.t.test(n=50*ESS/2, d=0.2)

#the gain mostly comes from increasing the number of hospitals

#now let's use the lower range of the ICC
DE = 1+ (n.responders-1)*0.004
ESS = n.responders/DE
pwr.t.test(n=50*ESS/2, d=0.2)

##################################
#Class activity
##################################


#1. What is your power if you go back to 100 people from each of 10 hospitals, and use an ICC of 0.004?

#2. What is your power if you go back to 100 people from each of 10 hospitals, and use a RR of 50%?

#3. What is your power if you go back to 100 people from each hospital but you sample from 80 hospitals?


#1. What is your power if you go back to 100 people from each of 10 hospitals, and use an ICC of 0.004?

n.responders = 100*0.95*.2
DE = 1+ (n.responders-1)*0.004
ESS = n.responders/DE
pwr.t.test(n=10*ESS/2, d=0.2)

#2. What is your power if you go back to 100 people from each of 10 hospitals, and use a RR of 50%?

n.responders = 100*0.95*.5
DE = 1+ (n.responders-1)*0.004
ESS = n.responders/DE
pwr.t.test(n=10*ESS/2, d=0.2)


#3. What is your power if you go back to 100 people from each hospital but you sample from 80 hospitals?

n.responders = 100*0.95*.2
DE = 1+ (n.responders-1)*0.004
ESS = n.responders/DE
pwr.t.test(n=80*ESS/2, d=0.2)

