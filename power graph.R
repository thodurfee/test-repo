library(pwr)
power.combo.table <- data.frame(expand.grid(n=seq(25,100,5),d=seq(.2,1,.05)),sig.level=rep(.05,272))
current.power <- c()
for(i in 1:length(power.combo.table[,1])){
  current.power[[i]] <- pwr.t.test(n=power.combo.table[i,"n"],power.combo.table[i,"d"],power.combo.table[i,"sig.level"])
}
n <- c()
d <- c()
sig.level <- c()
p <- c()
for(x in 1:length(current.power)){
  n[[x]] <- current.power[[x]]$n
  d[[x]] <- current.power[[x]]$d
  sig.level[[x]] <- current.power[[x]]$sig.level
  p[[x]] <- current.power[[x]]$p
}

power.table <- data.frame(n,d,sig.level,p)
library(dplyr)
library(ggplot2)
lclpower.table <- tbl_df(power.table)

lclpower.table %>% ggplot(data=.,aes(y=p,x=d,color=n,group=n)) + geom_point() + geom_line() +
                   labs(x="Cohen's d",y="Power",title="Power at alpha = 0.05") + scale_x_continuous(breaks=seq(.2,1,.05)) +
                   scale_y_continuous(breaks=seq(.1,1,.05)) + theme_bw()
