setwd("Google Drive/The Data Scientist’s Toolbox/test-repo/") #ignore this because this is my GitHub repo that's on GitHub and on my computer
condition <- c(rep("A",1000),rep("B",1000)) #make condition factor
dv1 <- c(rnorm(1000,4,1.5),rnorm(1000,6,1.5)) #make one dv
dv2 <- c(rnorm(1000,6,1.5),rnorm(1000,4,1.5)) #and another
fakedata <- data.frame(condition,dv1,dv2) #combine above into a dataframe
write.table(fakedata,"fakedata.csv",sep=",",row.names = FALSE,col.names = TRUE) #save as a csv

#get a GitHub account!!

fakedata.url <- url("https://raw.githubusercontent.com/nmmichalak/test-repo/master/fakedata.csv") #make the url an object with the url function (make sure it's raw on GitHub)
fakedata <- read.csv(fakedata.url,sep=",",header = TRUE) #load it like any other csv
head(fakedata) #see what it looks like
library(ggplot2) #duh
densities <- sapply(list("dv1","dv2"),function(i) {
  ggplot(data=fakedata,aes_string(x=i,fill="condition")) +
    geom_density(aes_string(x=i,fill="condition"),alpha=.2) +
    labs(x=paste(toupper(i)),color="Condition",title="Distributions by Condition") #sapply plot function over list of variables names in the dataset
},
simplify = FALSE,USE.NAMES = TRUE) #simplify returns output as list, USE.NAMES (only in sapply) names the objects in the list (nice for calling them later)
plots <- sapply(list("dv1","dv2"),function(i) {
  ggplot(data=fakedata,aes_string(x="condition",y=i,fill="condition")) +
  stat_summary(fun.data="mean_cl_boot",aes_string(x="condition",y=i),geom="bar") +
  stat_summary(fun.data="mean_cl_boot",aes_string(x="condition",y=i),geom="errorbar",width=.2) +
  labs(x=NULL,y=paste(toupper(i)),fill="Condition",title="95% CIs")
},
simplify = FALSE,USE.NAMES = TRUE)
anovas <- sapply(list("dv1","dv2"),function(i) {
  formula <- paste0(i,"~condition") #for some reason you need to paste the text for the formula argument here
  summary(aov(lm(formula,fakedata)))
},
simplify = FALSE,USE.NAMES = TRUE)
densities
plots
anovas #these are lists of named objects--they're named by the dvs they're plotting
