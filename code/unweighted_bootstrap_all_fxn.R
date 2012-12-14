library('SuperLearner')
library('tmle')
## estimation functions

calcQbar <- function(X,Y,control,treatment) { 
  source('./code/SLlibrary1.R') # source wrapper functions for Qn's glm's
  SL.library <- c(paste("SL.glm", 1:5, sep=""), "SL.mean",
                  "SL.earth", "SL.rpartPrune", "SL.ridge", "SL.glmnet")
  
  Qbar.n0 <- SuperLearner(Y=Y, X=X,
                          SL.library=SL.library,
                          cvControl=list(V=10))
  
  Qbar.n0.A0 <- predict(Qbar.n0,newdata=control)$pred
  Qbar.n0.A1 <- predict(Qbar.n0,newdata=treatment)$pred
  return(list(A0=Qbar.n0.A0, A1=Qbar.n0.A1))
}

calcATE <- function(Qbar.n0.A0,Qbar.n0.A1) { 
  mean(Qbar.n0.A1 - Qbar.n0.A0)
}

calcGhat <- function(W,A,Y) {
  source('./code/SLlibrary2.R') # source wrapper functions for ghat's glm's
  SL.lib4g <- c('SL.mean','SL.earth','SL.rpartPrune',
                'SL.glmnet',paste('SL.glm',1:4,sep=''))
  
  SL.ghat <- SuperLearner(A,W,family='binomial',
                          SL.library=SL.lib4g,
                          cvControl=list(V=10))
  g.est <- predict(SL.ghat)$pred
  
  return(g.est)
}

calcIPTW <- function(g.est,A,Y) {
  mean( ( (A==1)/g.est - (A==0)/(1-g.est) )*Y )
}

calcTMLE <- function(W,A,Y, Qbar.n0.A0, Qbar.n0.A1, ghat) {
  Q <- cbind(Qbar.n0.A0,Qbar.n0.A1)
  tmle.out <- tmle(Y,A,W,Q=Q,g1W=ghat)$estimates$ATE$psi
}

## create data units

createDataUnits <- function(sdw, bootstrap=FALSE) {
  if (bootstrap) {
    n = nrow(sdw)
    row.inds = sample(1:n,replace=T)
    sdw.boot = sdw[row.inds,]
    
    X <- sdw.boot[,-10] # dataframe of predictor variables
    X[,9] <- X[,9]-1 # shift treatment back to {0,1}
    Y <- sdw.boot[,10] # vector of outcomes
  } else {
    X <- sdw[,-10] # dataframe of predictor variables
    X[,9] <- X[,9]-1 # shift treatment back to {0,1}
    Y <- sdw[,10] # vector of outcomes
  }
  
  W = X[,-which(colnames(X)=='A')]
  A = X[['A']]
  
  control <- within(data=X,expr={A<-0})
  treatment <- within(data=X,expr={A<-1})
  
  data = list(X=X,Y=Y,W=W,A=A,control=control,treatment=treatment)
  return(data)
  
}

## calculate estimates!

calculateEstimates <- function(dataframe=sdw, bootstrap=FALSE, verb=F) {
  data <- createDataUnits(dataframe,bootstrap=bootstrap)
  with(data=data, expr={
    Qbar <- calcQbar(X,Y,control,treatment)
    ATE <- calcATE(Qbar$A0,Qbar$A1)  
    if(verb){print(sprintf("ATE is %f",ATE))}
    
    ghat <- calcGhat(W,A,Y)
    IPTW <- calcIPTW(ghat,A,Y)
    if(verb){print(sprintf("IPTW is %f", IPTW))}
    
    TMLE <- calcTMLE(W,A,Y, Qbar$A0, Qbar$A1, ghat)
    if(verb){print(sprintf("TMLE is %f", TMLE))}
    
    c(ATE,IPTW,TMLE)
  })
}