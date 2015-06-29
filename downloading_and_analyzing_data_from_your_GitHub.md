# Analyzing and plotting across multiple Dependent Measures
Nick Michalak  
June 29, 2015  
##Make fake data and save it to your directory/folder that's also your GitHub repository

```r
setwd("~/")
setwd("Google Drive/The Data Scientist’s Toolbox/test-repo/")
#ignore this because this is my GitHub repo that's on GitHub and on my computer
condition <- c(rep("A",100),rep("B",100)) #make condition factor
dv1 <- c(rnorm(100,4,1.5),rnorm(100,6,1.5)) #make one dv
dv2 <- c(rnorm(100,6,1.5),rnorm(100,4,1.5)) #and another
fakedata <- data.frame(condition,dv1,dv2) #combine above into a dataframe
write.table(fakedata,"fakedata.csv",sep=",",row.names = FALSE,col.names = TRUE) #save as a csv
```

> ###get a GitHub [(https://github.com)](https://github.com/) account, dude.

##Load data from your GitHub repository (badass, I know)

```r
fakedata.url <- url("https://raw.githubusercontent.com/nmmichalak/test-repo/master/fakedata.csv")
#make the url an object with the url function (make sure it's raw on GitHub)
fakedata <- read.csv(fakedata.url,sep=",",header = TRUE)
#load it like any other csv
head(fakedata) #see what it looks like
```

```
##   condition         dv1      dv2
## 1         A 0.007044629 5.095607
## 2         A 3.528289159 4.280789
## 3         A 4.193007879 6.350207
## 4         A 4.800110951 6.632543
## 5         A 2.333643832 5.043709
## 6         A 4.640153863 7.550138
```

##Plot code
* ### using sapply to apply plot code (any code, really) over a list of variable names in my fake dataset


```r
library(ggplot2) #duh
densities <- sapply(c("dv1","dv2"),function(i) {
  ggplot(data=fakedata,aes_string(x=i,fill="condition")) +
    geom_density(aes_string(x=i,fill="condition"),alpha=.2) +
    labs(x=paste(toupper(i)),color="Condition",title="Distributions by Condition")
#sapply plot function over list of variables names in the dataset
},
simplify = FALSE,USE.NAMES = TRUE)
#simplify returns output as list, USE.NAMES (only in sapply) names the objects in the list (nice for calling
#them later)
plots <- sapply(c("dv1","dv2"),function(i) {
  ggplot(data=fakedata,aes_string(x="condition",y=i,fill="condition")) +
  stat_summary(fun.data="mean_cl_boot",aes_string(x="condition",y=i),geom="bar") +
  stat_summary(fun.data="mean_cl_boot",aes_string(x="condition",y=i),geom="errorbar",width=.2) +
  labs(x=NULL,y=paste(toupper(i)),fill="Condition",title="95% CIs")
},
simplify = FALSE,USE.NAMES = TRUE)
anovas <- sapply(c("dv1","dv2"),function(i) {
  formula <- paste0(i,"~condition")
#for some reason you need to paste the text for the formula argument here
  anova(lm(formula,fakedata))
},
simplify = FALSE,USE.NAMES = TRUE)
```

###DV 1: Density Plots (read: fancy histograms)

```r
densities[["dv1"]]
```

![](downloading_and_analyzing_data_from_your_GitHub_files/figure-html/unnamed-chunk-4-1.png) 

###DV1: Mean Plots with 95%CIs

```r
plots[["dv1"]]
```

![](downloading_and_analyzing_data_from_your_GitHub_files/figure-html/unnamed-chunk-5-1.png) 

###DV1: ANOVA

```r
library(knitr)
options(digits=3)
kable(anovas[["dv1"]],format="pandoc")
```

              Df   Sum Sq   Mean Sq   F value   Pr(>F)
----------  ----  -------  --------  --------  -------
condition      1      272    272.10       145        0
Residuals    198      371      1.87        NA       NA

###DV 2: Density Plots (read: fancy histograms)

```r
densities[["dv2"]]
```

![](downloading_and_analyzing_data_from_your_GitHub_files/figure-html/unnamed-chunk-7-1.png) 

###DV 2: Mean Plots with 95%CIs

```r
plots[["dv2"]]
```

![](downloading_and_analyzing_data_from_your_GitHub_files/figure-html/unnamed-chunk-8-1.png) 

###DV 2: ANOVA

```r
options(digits=3)
kable(anovas[["dv2"]],format="pandoc")
```

              Df   Sum Sq   Mean Sq   F value   Pr(>F)
----------  ----  -------  --------  --------  -------
condition      1      225    225.22       107        0
Residuals    198      418      2.11        NA       NA
