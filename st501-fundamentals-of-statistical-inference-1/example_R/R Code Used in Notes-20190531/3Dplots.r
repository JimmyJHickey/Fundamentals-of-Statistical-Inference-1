#############################################################
##3 dimensional representation of a few joint distributions
##Justin Post - Fall 2015
#############################################################

#Read in necessary packages
library(scatterplot3d) 
library(plotly)

#Create pmf for the executives married example
#create values of x and y to use together
y<-rep(0:3,each=4)
x<-rep(0:3,4)
cbind(x,y)

#initialize a probability vector to correspond to each x,y combo
probability<-rep(0,4*4)

#create probabilities using joint pmf
for (i in 1:16){
  probability[i]<-(choose(4,y[i])*choose(3,x[i])*choose(2,3-y[i]-x[i]))/choose(9,3)
}

discreteData<-data.frame(x=x,y=y,z=probability)
#plot in 3-d

p <- plot_ly(discreteData, x = ~x, y = ~y, z = ~probability, color = ~probability, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = '# Married'),
                      yaxis = list(title = '# Never Married'),
                      zaxis = list(title = 'Joint PMF')))
p



#Now the continuous example, 30xy^2, x in (0,1), y in (x-1,1-x)
xgrid<-seq(from=0,to=1,by=0.05)
ygrid<-seq(from=-1,to=1,by=0.05)

#create grid of values
grid<-expand.grid(xgrid,ygrid)

#create joint PDF function
jointPDF<-function(grid){
  x<-grid$Var1
  y<-grid$Var2
  ifelse((x>1)|(x<0)|(y<(x-1))|(y>(1-x)),0,30*x*y^2)
}

#evaluate it at all grid points
pdf<-jointPDF(grid)

#plot in 3-d
scatterplot3d(x=grid$Var1,y=grid$Var2,z=pdf, pch=16, box=TRUE, highlight.3d=TRUE, type="p", main="Joint PMF of Y and X",angle=60,zlim=c(0,6),xlab="x",ylab="y")

continuousData<-data.frame(x=grid$Var1,y=grid$Var2,z=pdf)
p <- plot_ly(continuousData, x = ~x, y = ~y, z = ~pdf, marker = list(color = ~pdf, colorscale = c('#FFE1A1', '#683531'))) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'x'),
                      yaxis = list(title = 'y'),
                      zaxis = list(title = 'PDF')))
p





