library('SuperLearner')
library('tmle')
## estimation functions

calcQbar <- function(X,Y,weights,control,treatment) { 
  source('./code/SLlibrary1.R') # source wrapper functions for Qn's glm's
  SL.library <- c(paste("SL.glm",1:5, sep=""), "SL.mean",
                  "SL.rpartPrune", "SL.ridge", "SL.glmnet")
  
  Qbar.n0 <- SuperLearner(Y=Y, X=X,
                          obsWeights=weights,
                          SL.library=SL.library,
                          cvControl=list(V=10))
  
  Qbar.n0.A0 <- predict(Qbar.n0,newdata=control)$pred
  Qbar.n0.A1 <- predict(Qbar.n0,newdata=treatment)$pred
  return(list(A0=Qbar.n0.A0, A1=Qbar.n0.A1))
}

calcATE <- function(Qbar.n0.A0,Qbar.n0.A1,weights) { 
   sum(weights*(Qbar.n0.A1 - Qbar.n0.A0))
  }

calcGhat <- function(W,A,Y,weights) {
  source('./code/SLlibrary2.R') # source wrapper functions for ghat's glm's
  SL.lib4g <- c('SL.mean','SL.rpartPrune','SL.glmnet',
                paste('SL.glm',1:4,sep=''))
  
  SL.ghat <- SuperLearner(A,W,
                          obsWeights=weights,
                          family='binomial',
                          SL.library=SL.lib4g,
                          cvControl=list(V=10))
  g.est <- predict(SL.ghat)$pred
  
  return(g.est)
}

calcIPTW <- function(g.est,A,Y,weights) {
  sum( weights*((A==1)/g.est - (A==0)/(1-g.est))*Y )
}

calcTMLE <- function(W,A,Y, Qbar.n0.A0, Qbar.n0.A1, ghat) {
  Q <- cbind(Qbar.n0.A0,Qbar.n0.A1)
  tmle.out <- tmle(Y,A,W,Q=Q,g1W=ghat)$estimates$ATE$psi
}

## create data units

load('../causal_data/Rdata/processed_aggregated_data.Rdata') # load data (uses steven's directory structure)
sdw <- processed.aggregated.data

createDataUnits <- function(sdw, bootstrap=FALSE) {
  if (bootstrap) {
    n = nrow(sdw)
    row.inds = sample(1:n,replace=T,prob=sdw[["IntWeight"]])
    sdw.boot = sdw[row.inds,]
    
    X <- sdw.boot[,which(colnames(sdw.boot) != "Y")] # dataframe of predictor variables
    X <- within(data=X,expr={A <- A-1}) # shift treatment back to {0,1}
    Y <- sdw.boot[["Y"]] # vector of outcomes
  } else {
    X <- sdw[,which(colnames(sdw) != "Y")] # dataframe of predictor variables
    X <- within(data=X,expr={A <- A-1}) # shift treatment back to {0,1}
    Y <- sdw[["Y"]] # vector of outcomes
  }

  W = X[,which(colnames(X) != "A")]
  A = X[["A"]]
  
  control <- within(data=X,expr={A<-0})
  treatment <- within(data=X,expr={A<-1})
  weights <- X[["IntWeight"]]/sum(X[["IntWeight"]])
  
  data = list(X=X,Y=Y,W=W,A=A,control=control,treatment=treatment,weights=weights)
  return(data)
  
}

## calculate estimates!

calculateEstimates <- function(dataframe=sdw, bootstrap=FALSE) {
  data <- createDataUnits(dataframe,bootstrap=bootstrap)
  with(data=data, expr={
    Qbar <- calcQbar(X,Y,weights,control,treatment)
    ATE <- calcATE(Qbar$A0,Qbar$A1,weights)  
    print(sprintf("ATE is %f",ATE))
    
    ghat <- calcGhat(W,A,Y,weights)
    IPTW <- calcIPTW(ghat,A,Y,weights)
    print(sprintf("IPTW is %f", IPTW))
    
    TMLE <- calcTMLE(W,A,Y, Qbar$A0, Qbar$A1, ghat)
    print(sprintf("TMLE is %f", TMLE))
    
    c(ATE,IPTW,TMLE)
  })
}




