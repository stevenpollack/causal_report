require(ggplot2)

source("./code/unweighted_bootstrap_all_fxn.R")
load("../causal_data/Rdata/sdw.Rdata")

## estimate ghat and derive weights from it
data <- createDataUnits(sdw,bootstrap=F)
ghat <- with(data=data, expr={ghat <- calcGhat(W,A,Y)})
weights <- with(data=data, expr={
  weight <- (A==1)/ghat + (A==0)/(1-ghat)
  data.frame(weight=weight)})

## create ggplot
weightPlot <- ggplot(data=weights) + geom_histogram(aes(x=weight,y=..density..), binwidth=0.2, show_guide=F, fill='grey')  + geom_density(aes(x=weight,y=..density..), adjust=1, show_guide=F) + xlab(expression(g[n]^-1*"("*1*"|"*w*")")) + ylab("Density") + xlim(c(0,7))

ggsave(filename="./project_report/figures/IPTWDensityPlot.pdf")