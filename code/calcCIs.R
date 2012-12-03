## build CI's for bootstrap
library(ggplot2)
library(xtable)

## g-comp:
findCIs <- function(i) {
  bootstrap <- bootstrap.list[,i]
  bootstrapSD <- sqrt(var(bootstrap[-1]))
  bootstrapCINormal <- bootstrap[1] + c(-1.96,1.96)*bootstrapSD
  
  pvalue <- pnorm(q=bootstrap.list[1,i],
                  mean=mean(bootstrap[-1]),
                  sd=bootstrapSD,lower.tail=FALSE)
  
  tmp <- quantile(bootstrap,probs=c(0.025,0.975))
  bootstrapCIEmp <- c(tmp[[1]],tmp[[2]])
  return(list(Normal=bootstrapCINormal, Emp=bootstrapCIEmp, pvalue=pvalue))
}

bootstrapCIs <- sapply(1:3,findCIs)

bootstrapDataFrame <- data.frame(estimator=c("G-Comp","IPTW","TMLE"),
                                  estimate=bootstrap.list[1,1:3],
                                  normalLower=unlist(bootstrapCIs[1,1:3])[c(1,3,5)],
                                  normalUpper=unlist(bootstrapCIs[1,1:3])[c(2,4,6)],
                                 pvalues=unlist(bootstrapCIs[3,]),
                                  empLower=unlist(bootstrapCIs[2,1:3])[c(1,3,5)],
                                  empUpper=unlist(bootstrapCIs[2,1:3])[c(2,4,6)]
                                 )

# build plots

normalPlot <- ggplot(data=bootstrapDataFrame, aes(x=estimator, y=estimand, colour=estimator)) + geom_point(show_guide=F) + geom_errorbar(aes(ymin=normalLower, ymax=normalUpper), width=.25, show_guide=F) + ggtitle("Normal Based 95% Confidence Intervals") 

empPlot <- ggplot(data=bootstrapDataFrame, aes(x=estimator, y=estimand, colour=estimator) )  + geom_point(show_guide=F) + geom_errorbar(aes(ymin=empLower, ymax=empUpper), width=.25, show_guide=F) + ggtitle("Quantile Based 95% Confidence Intervals")

# build latex table
d <- 5 # num of digits
outputTable <- matrix(nrow=3,ncol=4)

outputTable[,1] <- round(bootstrapDataFrame$estimate,digits=d)
outputTable[,2] <- with(data=bootstrapDataFrame,expr={
  paste("(",round(normalLower,digits=d),", ",round(normalUpper,digits=d),")",sep="")
})
outputTable[,3] <- round(bootstrapDataFrame$pvalues,digits=d)
outputTable[,4] <- with(data=bootstrapDataFrame,expr={
  paste("(",round(empLower,digits=d),", ",round(empUpper,digits=d),")",sep="")
})

rownames(outputTable) <- c("G-Comp","IPTW","TMLE")
colnames(outputTable) <- c("Estimate","95% Normal CI", "p-value", "95% Quantile CI")
tableLatex <- xtable(outputTable,digits=5)
align(tableLatex) <- "rcccc"

