############################################

### Longitudinal Data###

############################################

#this is a collection of packages. This may take a while to install.
install.packages(c("tidyverse", "dplyr","labelled","rstatix","ggpubr","GGally","car","Epi","lme4","lmerTest","emmeans","multcomp","geepack","ggeffects","gt"))




library(tidyverse)
library(labelled)   # labeling data
library(rstatix)    # summary statistics
library(ggpubr)     # convenient summary statistics and plots
library(GGally)     # advanced plot
library(car)        # useful for anova/wald test
library(Epi)        # easy getting CI for model coef/pred
library(lme4)       # linear mixed-effects models
library(lmerTest)   # test for linear mixed-effects models
library(emmeans)    # marginal means
library(multcomp)   # CI for linear combinations of model coef
library(geepack)    # generalized estimating equations
library(ggeffects)  # marginal effects, adjusted predictions
library(gt)         # nice tables
library(dplyr)
#This is now starting to use the tidyverse. If you are not familiar with the tidyverse, see "R for Data Science" https://r4ds.had.co.nz

#Load data
load(url("http://alecri.github.io/downloads/data/dental.RData"))

#look at the data
dental

# See that the data is presented in a wide format where repeated measurements are contained in separate variables (only one row for each individual).
#Oftentimes, a longitudinal format, where repeated measurements are reported as separate rows, is preferable for the analysis. 
#The pivot_longer function is useful for restruring a data from a wide to a long format:

###########################################
### Making the dataset ###
############################################

#let's go step by step
#"piping" is a way to do this in a MUCH CLEANER WAY. I will not use piping here as I think it's very difficult to follow unless you are familiar with it. 
#first make multiple rows per person
dental_long = pivot_longer(dental, cols = starts_with("y"), names_to = "measurement", values_to = "distance") 
dental_long
#pull ages to a numeric column
dental_long = mutate(dental_long, age = parse_number(measurement), measurement = fct_inorder(paste("Measure at age", age)))
dental_long
#make nice labels, this will be helpful when plotting
dental_long = set_variable_labels(dental_long, age = "Age of the child at measurement", measurement = "Label for time measurement", distance = "Measurement")
dental_long

###########################################
### Means over time ###
############################################

#let's look at the mean response over time
get_summary_stats(group_by(dental_long, age), distance)

#Overall, the mean response increases with age. The variance of distance also increases with age. The mean distance increases by 4 mm (26-22) from age 8 to age 14 years.

#let's plot this
#practice understanding what this does. First only run the first piece. aes stands for aesthetics. Generally give your x and your y first here. Additionally can give a fill. Then add the second. What if you leave out fill here? Try changing it to orange. What does geom_jitter do? What happens if you change it to 0? What did guides do? what does labs do? what does theme do? Practice looking up how to make font bigger in the plot (theme)?
ggplot(dental_long, aes(measurement, distance, fill = measurement)) + geom_boxplot() + geom_jitter(width = 0.2) + guides(fill = "none") + labs(x = "", y = "Dental growth, mm") + theme(text = element_text(size = 18)) 

#now let's split by sex
get_summary_stats(group_by(dental_long, sex, measurement), distance, show = c("mean", "sd"))

#now let's plot
ggplot(dental_long, aes(sex, distance, fill = measurement)) + geom_boxplot() + labs(x = "", y = "Dental growth, mm", fill = "") + theme(text = element_text(size = 18)) 

#here is another way to look at it:
ggplot(summarise(group_by(dental_long, sex, measurement), mean_distance = mean(distance), .groups = "drop"), aes(sex, mean_distance, fill = measurement, label = round(mean_distance))) + geom_col(position = "dodge") + geom_text(position = position_dodge(width = 0.9), vjust = -0.5) + coord_flip() + labs(x = "", y = "Mean Dental growth, mm", fill = "") + theme(text = element_text(size = 18)) 

###########################################
### Correlations ###
############################################

#let's take a look at the correlation of the responses
# covariance matrix
cov_obs = cov(dplyr::select(dental, starts_with("y")))
cov2cor(cov_obs)

# A positive (linear) correlation is observed for consecutive measurements.

#we can view this as:
#try removing the lower part
ggpairs(dplyr::select(dental, starts_with("y")), lower = list(continuous = "smooth"))

#now we can plot by sex
#notice that you give it the data differently because it needs the sex column
ggpairs(dental, mapping = aes(colour = sex), columns = 3:6, lower = list(continuous = "smooth"))


###########################################
### Trajectories over time ###
############################################

#format our data a bit
#this can all be done at once with piping but we will due step by step
plot_data = group_by(dental_long, sex, age)
#notice groups at the top
#the .groups = "drop" just means that now, you can drop the grouping, I just used it to make the mean and ci and no longer need it
plot_data = summarise(plot_data, mean = list(mean_ci(distance)),.groups = "drop") 
#calculates mean and ci 
plot_data = unnest_wider(plot_data,mean)
#unnests the column with the list
#This creates a "wiggled" age so the error bars are not exactly on top of each other
plot_data =  mutate(plot_data,agex = age - .05 + .05*(sex == "Boy")) 
ggplot(plot_data, aes(agex, y, col = sex, shape = sex)) + geom_point() + geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) + geom_line() + labs(x = "Age, years", y = "Mean Dental growth, mm", shape = "Sex", col = "Sex")


#now let's plot the individual trajectories, facet_wrap makes multiple panels. Try removing the theme piece.
ggplot(dental_long, aes(age, distance, col = factor(id))) +
  geom_point() +
  geom_line() +
  facet_wrap(~ id) +
  labs(x = "Age, years", y = "Dental growth, mm", col = "Child id") +
  guides(col = guide_legend(nrow = 3)) + theme(legend.position="bottom")

#spaghetti plot
ggplot(dental_long, aes(age, distance, col = factor(id))) +
  geom_line() +
  labs(x = "Age, years", y = "Dental growth, mm", col = "Child id") +
  guides(col = guide_legend(nrow = 3)) + theme(legend.position="bottom")

#spaghetti plot by sex
ggplot(dental_long, aes(age, distance)) +
  geom_line(aes(group = factor(id))) +
  geom_smooth() +
  facet_grid(~ sex)


###########################################
### Class activity ###
############################################

#Prospective study on body fat accretion in a cohort of 162 girls from the MIT Growth and Development Study. At start of study, all the girls were pre-menarcheal and non-obese.
#All girls were followed over time according to a schedule of annual measurements until four years after menarche. The final measurement was scheduled on the fourth anniversary of their reported date of menarche. At each examination, a measure of body fatness was obtained based on bioelectric impedance analysis.

load(url("http://alecri.github.io/downloads/data/bodyfat.RData"))
bodyfat[1:10,]

#Our response of interest is pbf
#Notice that even though we have occassion 1,2,3,4,5,6, it corresponds to different ages. It does not make sense to calculate the mean at each age because everyone has different ages. 

#1. Create the boxplot figure (the first one we plotted) with a boxplot for each occassion. Notice that you don't have to pivot this dataset, it already has multiple rows per person. You will also find that occasion needs to be a factor. 

#2. Create the correlation plot. You will need to make a wide format (code below). You also need to drop occasion 10 from the figure because there is only 1 person.

bodyfat_wide = bodyfat[,c(1,5,6)]
bodyfat_wide = pivot_wider(bodyfat_wide, names_from = "occasion", values_from = "pbf" )

#3. Create a spaghetti plot to show individual trajectories. Remove any legend. Can you figure out how to make all the lines black?



#1. Create the boxplot figure (the first one we plotted) with a boxplot for each occassion. Notice that you don't have to pivot this dataset, it already has multiple rows per person. You will also find that occasion needs to be a factor. 

bodyfat = mutate(bodyfat, occasion_f = as.factor(occasion))
ggplot(bodyfat, aes(occasion_f, pbf, fill = occasion_f)) + geom_boxplot() + geom_jitter(width = 0.2) + guides(fill = "none") + labs(x = "", y = "Percent Body Fat, %") 

#2. Create the correlation plot. You will need to make a wide format (code below). You also need to drop occasion 10 from the figure because there is only 1 person.

bodyfat_wide = bodyfat[,c(1,5,6)]
bodyfat_wide = pivot_wider(bodyfat_wide, names_from = "occasion", values_from = "pbf" )
ggpairs(bodyfat_wide[,-c(1,11)], lower = list(continuous = "smooth"))

#3. Create a spaghetti plot to show individual trajectories. Remove any legend. Can you figure out how to make all the lines black?
ggplot(bodyfat, aes(occasion, pbf, col = factor(id))) +
  geom_line() +
  labs(x = "Occasion", y = "Percent Body Fat, %", col = "Person id")  + theme(legend.position="none")


ggplot(bodyfat, aes(occasion, pbf)) +
   geom_line(aes(group = id)) +
  labs(x = "Occasion", y = "Percent Body Fat, %", col = "Person id")  + theme(legend.position="none")
  

