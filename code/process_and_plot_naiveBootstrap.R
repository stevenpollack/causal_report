require(ggplot2)
require(reshape2)
require(plyr)

load("../causal_data/Rdata/naiveBootstrapData.R")
load("../causal_data/Rdata/weightedBootstrapResults.Rdata")

estimators <- c("G-Comp", "IPTW", "TMLE")

# structure the data (make sure estimates stay numeric)
naiveBootstrapData <- as.data.frame(naiveBootstrapData)
naiveBootstrapData <- cbind(naiveBootstrapData,rep("unweighted",times=nrow(naiveBootstrapData)))
colnames(naiveBootstrapData) <- c(estimators,"type")


# make weighted estimates' structure compatible
weightedPE <- data.frame(t(bootstrapResults$t0),"weighted")
colnames(weightedPE) <- c(estimators,"type")

fullData <- rbind(naiveBootstrapData,weightedPE)

# melt it for use with ggplot
meltedData <- melt(fullData,measure.vars=estimators,variable.name="estimator")
# make summary stats for qqplots using plyr
summaryBSData <- ddply(meltedData,.(estimator,type),summarize,mean=mean(value),sd=sd(value))

# check out densities of bootstrap distributions:
bootstrap.Density.plot <- ggplot(data=meltedData[meltedData$type=="unweighted",]) + geom_density(aes(x=value,y=..density..,fill=estimator),alpha=0.25,adjust=2,trim=F,kernel="epanechnikov") + xlab("Estimate") + ylab("Density of Estimator") + scale_fill_discrete(name="Estimator")

ggsave(filename="./project_report/figures/naiveBootstrapDensities.pdf")

## they look fairly "normal"; check out qq plots
bootstrap.QQ.plot <- ggplot(data=meltedData[meltedData$type=="unweighted",],aes(sample=value)) + stat_qq(alpha=0.75,aes(colour=estimator)) + geom_abline(data=summaryBSData[summaryBSData$type=="unweighted",], aes(intercept=mean, slope=sd)) + facet_wrap(~estimator, scales="free") + guides(colour=F)

ggsave(filename="./project_report/figures/naiveBootstrapQQplots.pdf")

# build summary statistics fxns for ggplot2
# these stats do NOT exclude the original point
# estimate from the bootstrap point estimate data

getPointEstimate <- function(x) {
  out <- head(x,n=1L)
  names(out) <- "y"
  return(out)
}

makeEmpCI <- function(x) {
  out <- c(quantile(x=x,probs=c(0.025,0.975),names=F),x[1])
  names(out) <- c("ymin","ymax","y")
  return(out)
}

makeNormCI <- function(x) {
  bsMean <- mean(x)
  bsSE <- sqrt(var(x))
  out <- c(x[1], x[1] + c(-1.96,1.96)*bsSE)
  names(out) <- c("y","ymin", "ymax")
  return(out)
}


quantileBasedCIPlot <- ggplot() + stat_summary(data=meltedData[meltedData$type=="unweighted",],aes(x=estimator,y=value,shape=type),geom="point", fun.y=getPointEstimate)
quantileBasedCIPlot <- quantileBasedCIPlot + stat_summary(data=meltedData[meltedData$type=="unweighted",],aes(colour=estimator, x=estimator,y=value),geom="errorbar",fun.data=makeEmpCI,width=0.25,size=1,alpha=0.75,show_guide=F)
quantileBasedCIPlot <- quantileBasedCIPlot + stat_summary(data=meltedData[meltedData$type=="weighted",], aes(x=estimator,y=value,shape=type), geom="point", size=4, fun.y=getPointEstimate)
quantileBasedCIPlot <- quantileBasedCIPlot + scale_shape_manual(values=c(19,4),name="Type of \nPoint Estimate") + xlab("Estimator") + ylab("Estimates")

ggsave(filename="./project_report/figures/naiveBootstrapQuantileCI.pdf")

normalBasedCI.plot <- ggplot() + stat_summary(data=meltedData[meltedData$type=="unweighted",],aes(x=estimator,y=value,shape=type),geom="point", fun.y=getPointEstimate) 
normalBasedCI.plot <- normalBasedCI.plot + stat_summary(data=meltedData[meltedData$type=="unweighted",],aes(colour=estimator, x=estimator,y=value),geom="errorbar",fun.data=makeNormCI,width=0.25,size=1,alpha=0.75,show_guide=F)
normalBasedCI.plot <- normalBasedCI.plot + stat_summary(data=meltedData[meltedData$type=="weighted",], aes(x=estimator,y=value,shape=type), geom="point", size=4, fun.y=getPointEstimate)
normalBasedCI.plot <- normalBasedCI.plot + scale_shape_manual(values=c(19,4),name="Type of \nPoint Estimate") + xlab("Estimator") + ylab("Estimates")

ggsave(filename="./project_report/figures/naiveBootstrapNormalCI.pdf")

