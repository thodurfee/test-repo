setwd("Google Drive/The Data Scientist’s Toolbox/test-repo/")
condition <- c(rep("A",100),rep("B",100))
dv1 <- c(rnorm(100,4,1.5),rnorm(100,6,1.5))
dv2 <- c(rnorm(100,6,1.5),rnorm(100,4,1.5))
fakedata <- data.frame(condition,dv1,dv2)
write.table(fakedata,"fakedata.csv",sep=",",row.names = FALSE,col.names = TRUE)

fakedata.url <- url("https://raw.githubusercontent.com/nmmichalak/test-repo/master/fakedata.csv")
fakedata <- read.csv(fakedata.url,sep=",",header = TRUE)
head(fakedata)
library(ggplot2)
library(Hmisc)
densities <- sapply(list("dv1","dv2"),function(i) {
  ggplot(data=fakedata,aes_string(x=i,fill="condition")) +
    geom_density(aes_string(x=i,fill="condition"),alpha=.2) +
    labs(x=paste(toupper(i)),color="Condition",title="Distributions by Condition")
},
simplify = FALSE,USE.NAMES = TRUE)
plots <- sapply(list("dv1","dv2"),function(i) {
  ggplot(data=fakedata,aes_string(x="condition",y=i,fill="condition")) +
  stat_summary(fun.data="mean_cl_boot",aes_string(x="condition",y=i),geom="bar") +
  stat_summary(fun.data="mean_cl_boot",aes_string(x="condition",y=i),geom="errorbar",width=.2) +
  labs(x=NULL,y=paste(toupper(i)),fill="Condition",title="95% CIs")
},
simplify = FALSE,USE.NAMES = TRUE)
anovas <- sapply(list("dv1","dv2"),function(i) {
  formula <- paste0(i,"~condition")
  summary(aov(lm(formula,fakedata)))
},
simplify = FALSE,USE.NAMES = TRUE)
densities
plots
anovas
