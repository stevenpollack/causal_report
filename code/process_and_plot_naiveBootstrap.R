library(ggplot2)
library(reshape2)
library(plyr)
load("../causal_data/Rdata/naiveBootstrapData.R")

# structure the data
colnames(naiveBootstrapData) <- c("G-Comp", "IPTW", "TMLE")
names(dimnames(naiveBootstrapData)) <- c("estimate","estimator")
  
# melt it for use with ggplot
meltedBSData <- melt(naiveBootstrapData,measure.vars=estimator,variable.name="estimator")
# make summary stats for qqplots using plyr
summaryBSData <- ddply(meltedBSData,.(estimator),summarize,mean=mean(value),sd=sd(value))

# check out densities of bootstrap distributions:
bootstrap.Density.plot <- ggplot(data=meltedBSData) + geom_density(aes(x=value,y=..density..,fill=estimator),alpha=0.25,adjust=2,trim=F,kernel="epanechnikov") + xlab("Estimate") + ylab("Density of Estimator") + scale_fill_discrete(name="Estimator")

ggsave(filename="./project_report/figures/naiveBootstrapDensities.pdf")

## they look fairly "normal"; check out qq plots
bootstrap.QQ.plot <- ggplot(data=meltedBSData,aes(sample=value)) + stat_qq(alpha=0.75,aes(colour=estimator)) + geom_abline(data=summaryBSData, aes(intercept=mean, slope=sd)) + facet_wrap(~estimator, scales="free") + guides(colour=F)

ggsave(filename="./project_report/figures/naiveBootstrapQQplots.pdf")

# build summary statistics fxns for ggplot2
# these stats do NOT exclude the original point
# estimate from the bootstrap point estimate data

getPointEstimate <- function(x) {
  out <- x[1]
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

quantileBasedCI.plot <- ggplot(data=meltedBSData, aes(colour=estimator)) + stat_summary(aes(x=estimator,y=value),geom="pointrange",fun.data=makeEmpCI,size=0.75) + xlab("Estimator") + ylab("Estimates") + scale_color_discrete(name="Estimator")

ggsave(filename="./project_report/figures/naiveBootstrapQuantileCI.pdf")

normalBasedCI.plot <- ggplot(data=meltedBSData, aes(colour=estimator)) + stat_summary(aes(x=estimator,y=value),geom="pointrange",fun.data=makeNormCI,size=0.75) + xlab("Estimator") + ylab("Estimates") + scale_color_discrete(name="Estimator")

ggsave(filename="./project_report/figures/naiveBootstrapNormalCI.pdf")


