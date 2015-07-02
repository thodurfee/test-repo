values <- seq(0,10,.1)
x1 <- values^.25
x2 <- values^.75
ind.plot.df <- subset(data.frame(expand.grid(x1,x2)), (x1*x2)==10)
library(ggplot2)
ggplot(ind.plot.df,aes(x=x1,y=x2)) + geom_point() + geom_line() +
       scale_x_continuous(breaks=seq(0,2,.1)) + scale_y_continuous(breaks=seq(0,6,1)) + labs(x="x^0.25",y="x^0.75",title="U = x^0.75 * x^0.25")
