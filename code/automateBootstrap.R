source("./code/bootstrap_all_fxn.R")

B <- 1e3
bootstrapResults <- sapply(X=c(FALSE,rep(TRUE,times=B-1)),
                           FUN=calculateEstimates,dataframe=sdw)

save(bootstrapResults,file="../causal_data/1000sampleBSrun.Rdata")