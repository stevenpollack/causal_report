library(SuperLearner)

data <- createDataUnits(sdw,bootstrap=F)
ghat <- with(data=data, expr={ghat <- calcGhat(W,A,Y)})
weightPlot <- ggplot() + geom_density(aes(x=ghat,y=..density..), colour='black',show_guide=F) + geom_histogram(aes(x=ghat,y=..density..),alpha=0.35,binwidth=0.02,show_guide=F)  + xlab(expression(g[n]*"("*1*"|"*w*")")) + ggtitle(expression("SuperLearner estimate of "*g[n]*"("*1*"|"*w*")"))

ggsave("./project_report/density-plot_ghat.pdf", plot=weightPlot)