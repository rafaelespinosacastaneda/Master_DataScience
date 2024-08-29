#Tennis Court Plot
#In my opinion, the best plotting package in R (or coding language I've used)
library(ggplot2)

#Creates a data.frame object, the easy structure to use for ggploting
tennisCourt = data.frame(x1 = c(0,4.5,18,31.5,36,0,4.5,4.5,0,-2),
                         x2 = c(0,4.5,18,31.5,36,36,31.5,31.5,36,38),
                         y1 = c(-39,-39,-21,-39,-39,39,21,-21,-39,0), 
                         y2 = c(39,39,21,39,39,39,21,-21,-39,0),
                         width = c(rep(1,9),3))

#Creates a plot object called ggTennis
ggTennis = ggplot(tennisCourt) + 
  geom_segment(aes(x = x1,y = y1,xend = x2,yend = y2),size = tennisCourt$width) + 
  labs(x = "A bad x-axis label",y = 'An easily replaceable y-axis label',
       title = 'A Non-relevant Title')

#Running the next line will show your tennis court as a plot
ggTennis


################################################################################
# Everything above this is what you actually need for the homework problem. 
# Everything following this is just tutorial and some intro to ggplot. If you
# are comfortable with ggplot already, the following will likely not be super
# helpful.
################################################################################

#If you want to add some points to your tennis court plot, you must literally
#   "add" them to the plot
#Lets say I want some points at:
#   (10,10),
#   (12,17), and
#   (33,-5)
#Here's the points data frame to use in the plotting
pointsToAdd = data.frame(x = c(10,12,33), y = c(10,17,-5))

#Now we add in the points, and create a new object
#The geom_point function helps us create points. Note that we give it new data,
ggTennisWithPoints = ggTennis + 
  geom_point(data = pointsToAdd,aes(x = x, y = y),color = 'firebrick')

#Let's see what we made
ggTennisWithPoints

#Other functions that will help you over time:
#   geom_line - makes lines
#   geom_text/geom_label - helps you annotate things
#   geom_polygon/geom_ribbon - helps makes shapes (helpful for error bars)
#   coord_cartesian - helps change the x and y limits
#   geom_histogram - histograms
#   theme - encompasses a lot of things, but most notably can be used to help 
#           with legends and stuff

#As with many things, it will take time to really understand how everything in
#   this works, but it can be a very helpful visualization package. 

#For a color palette, see the following (I'm a big fan of burlywood4):
#   http://sape.inf.usi.ch/quick-reference/ggplot2/colour


