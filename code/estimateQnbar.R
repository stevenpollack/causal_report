## estimate \bar{Q}_{n}^{0} and bootstrap for variance

library('SuperLearner')
load('../causal_data/sdw.Rdata') # load data
source('./code/SLlibrary1.R') # source wrapper functions for glm's

SL.library <- c(paste("SL.glm",1:5, sep=""), "SL.mean", "SL.earth", "SL.rpartPrune", "SL.ridge", "SL.glmnet")

X <- sdw[,-10] # dataframe of predictor variables
X[,9] <- X[,9]-1 # shift treatment back to {0,1}

Y <- sdw[,10] # vector of outcomes

control <- within(data=X,expr={A<-0})
treatment <- within(data=X,expr={A<-1})

SL.ATE <- function(X,Y,SL.library,control,treatment) {
  Qbar.n0 <- SuperLearner(Y=Y, X=X,
                          SL.library=SL.library,
                          cvControl=list(V=10))
  
  Qbar.n0.A0 <- predict(Qbar.n0,newdata=control)$pred
  Qbar.n0.A1 <- predict(Qbar.n0,newdata=treatment)$pred 
  
  ATE <- mean(Qbar.n0.A1 - Qbar.n0.A0)
  return(ATE)
}

ATE.bootstrap <- replicate(n=100,expr={SL.ATE(X,Y,SL.library,control,treatment)})