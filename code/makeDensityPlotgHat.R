library(SuperLearner)

data <- createDataUnits(sdw,bootstrap=F)
weights <- with(data=data, expr={
  ghat <- calcGhat(W,A,Y)
  weights <- (A==1)/ghat + (A==0)/(1-ghat)})

weightPlot <- ggplot() + geom_density(aes(x=weights,y=..density..), colour='black',show_guide=F) + geom_histogram(aes(x=weights,y=..density..),alpha=0.35,binwidth=0.25,show_guide=F)  + xlab("value of weights") + ggtitle("Distribution of weights for IPTW") + xlim(c(0,7.5))

ggsave("./project_report/density-plot_ghat.pdf", plot=weightPlot)

summaryOut <- t(summary(weights))
colnames(summaryOut) <- rep("",times=6)

print(xtable(summaryOut))