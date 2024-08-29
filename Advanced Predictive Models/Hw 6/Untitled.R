
#---------------------------------   Part a -----------------------------------------
load("/Users/rafa/Documents/Master Austin/MAESTRIA_AUSTIN/Advanced Predictive Models/Hw6/Hw6Part1.RData")


# scatterplot of avg_logPOS vs. logPOS
ggplot(data=crime_dat,
       aes(x = poverty, y = crime)) +
  geom_point() +
  stat_smooth(method = "lm") +
  xlab("Percentage of the population below the poverty line") +
  ylab("Counts of violent crimes ")+
  labs(title="NNCS counts of violent crimes per 1,000 individuals in year 2000 vs 
       Percentage of the population below the poverty line")


#---------------------------------   Part b -----------------------------------------


print("We cannot make a conclusion from aggregate data to individuals")
