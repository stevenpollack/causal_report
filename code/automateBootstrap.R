library(boot)

# this function will be called by ran.gen in
# boot::boot when performing "parametric"
# bootstrap so it needs to take (d,p) as
# arguments
createBootstrapData <- function(data,weights) {
  nrows <- nrow(data)
  bootstrapIndices <- sample(x=1:nrows,size=nrows,
                             replace=T,prob=weights)
  return(data[bootstrapIndices,])
}


source("./code/bootstrap_all_fxn.R")
load('../causal_data/Rdata/processed_aggregated_data.Rdata') # load data (uses steven's directory structure)

sdw <- processed.aggregated.data
weights <- sdw[["IntWeight"]]/sum(sdw[["IntWeight"]])

B <- 3
bootstrapResults <- boot(data=sdw, statistic=calculateEstimates, stype="w", R=500, weights=weights, parallel="multicore", ncpus=2)
