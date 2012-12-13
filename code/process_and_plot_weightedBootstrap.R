library(ggplot2)
library(reshape2)
load("../causal_data/Rdata/weightedBootstrapResults.Rdata")

bootHists <- with(data=bootstrapResults,expr={data.frame(gComp=t[,1],IPTW=t[,2],TMLE=t[,3])})

bootHists <- melt(bootHists,variable.name="Estimator")

gComp.IPTW.bootstrap.plot <- ggplot(data=bootHists[which(bootHists[["Estimator"]] != "TMLE"),]) + geom_density(aes(x=value,colour=Estimator),alpha=0.5,adjust=2,trim=T) # approximately normal

TMLE.bootstrap.plot <- ggplot(data=bootHists[which(bootHists[["Estimator"]] == "TMLE"),]) + geom_density(aes(x=value,colour=Estimator),alpha=0.5,adjust=2,trim=F) # this is not approximately normal

## calculate CI's

gCompResults <- with(data=bootstrapResults,expr={
  e0 <- t0[1]
  bsMean <- mean(t[,1])
  bsSE <- sqrt(var(t[,1]))
  empQuants <- quantile(probs=c(0.025,0.975),x=t[,1],names=F)
  normQuants <- qnorm(p=c(0.025,0.975),mean=bsMean,sd=bsSE)
  data.frame(original=e0,mean=bsMean,se=bsSE,empQuantsL=empQuants[1],empQuantsU=empQuants[2],normQuantsL=normQuants[1],normQuantsU=normQuants[2])
}) 

IPTWResults <- with(data=bootstrapResults,expr={
  e0 <- t0[2]
  bsMean <- mean(t[,2])
  bsSE <- sqrt(var(t[,2]))
  empQuants <- quantile(probs=c(0.025,0.975),x=t[,2],names=F)
  normQuants <- qnorm(p=c(0.025,0.975),mean=bsMean,sd=bsSE)
  data.frame(original=e0,mean=bsMean,se=bsSE,empQuantsL=empQuants[1],empQuantsU=empQuants[2],normQuantsL=normQuants[1],normQuantsU=normQuants[2])
})

TMLEResults <- with(data=bootstrapResults,expr={
  e0 <- t0[3]
  bsMean <- mean(t[,3])
  bsSE <- sqrt(var(t[,3]))
  empQuants <- quantile(probs=c(0.025,0.975),x=t[,2],names=F)
  #normQuants <- qnorm(p=c(0.025,0.975),mean=bsMean,sd=bsSE)
  data.frame(original=e0,mean=bsMean,se=bsSE,empQuantsL=empQuants[1],empQuantsU=empQuants[2],normQuantsL=NA,normQuantsU=NA)
})

results <- list(gComp=gCompResults,IPTW=IPTWResults,TMLE=TMLEResults)
results <- melt(results)
colnames(results) <- c("value","statistic","estimator")

calcCIquantiles <- function(x) {
  out <- quantile(x=x,probs=c(0.025,0.975))
  names(out) <- c("ymin","ymax")
  return(out)
}

calcCInorm <- function(x) {
  sampleMean <- mean(x)
  sampleSD <- sqrt(var(x))
  out <- qnorm(p=c(0.025,0.5,0.975),mean=sampleMean,sd=sampleSD)
  names(out) <- c("ymin","y","ymax")
  return(out)
}

bootstrapData <- as.data.frame(bootstrapResults$t)
colnames(bootstrapData) <- c("G-Comp","IPTW","TMLE")
bootstrapData <- melt(bootstrapData,variable.name="estimator")

ggplot(data=bootstrapResults$t0, aes(x=estimator,y=value,colour=estimator)) + stat_summary(fun.data=calcCInorm, geom="pointrange")

ggplot() + geom_pointrange(data=results,
                      aes(x=estimator,
                          y=value[c(1,9,15)],
                          ymin=value[c(4,11,18)],
                          ymax=value[c(5,12,19)])
)


ggplot() + geom_pointrange(                           aes(x=c(1,2,3),
                               y=c(1,2,3),
                               ymin=c(1,2,3)-1,
                               ymax=c(1,2,3)+1)
)