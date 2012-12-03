library('SuperLearner')
library('tmle')
load('../causal_data/sdw.Rdata') # load data (uses steven's directory structure)

n = nrow(sdw)
bootstrap.list = do.call(rbind,lapply(1:101, function(i){
  print(i)
  
	if(i>1){ # sample from data for bootstrap
		row.inds = sample(1:n,replace=T)
		sdw.boot = sdw[row.inds,]
	} else { # use original data for initial bootstrap
		sdw.boot = sdw
	}

	
	X <- sdw.boot[,-10] # dataframe of predictor variables
	X[,9] <- X[,9]-1 # shift treatment back to {0,1}
	
	control <- within(data=X,expr={A<-0})
	treatment <- within(data=X,expr={A<-1})
	
	W = X[,-which(colnames(X)=='A')]
	A = X[['A']]
	Y <- sdw.boot[,10] # vector of outcomes
	
	## take care of IPTW estimate
  source('./code/SLlibrary2.R') # source wrapper functions for ghat's glm's
  SL.lib4g = c('SL.mean','SL.earth','SL.rpartPrune',
               'SL.glmnet',paste('SL.glm',1:4,sep=''))
  
  SL.ghat <- SuperLearner(A,W,family='binomial',
                          SL.library=SL.lib4g,
                          cvControl=list(V=10))
  g.est <- predict(SL.ghat)$pred

	iptw.est = mean( ( (A==1)/g.est - (A==0)/(1-g.est) )*Y )
  
	print(sprintf("IPTW is %f", iptw.est))
	
	## take care of ATE estimate
	source('./code/SLlibrary1.R') # source wrapper functions for Qn's glm's
  SL.library <- c(paste("SL.glm",1:5, sep=""), "SL.mean",
                  "SL.earth", "SL.rpartPrune", "SL.ridge", "SL.glmnet")
  
	Qbar.n0 = SuperLearner(Y=Y, X=X,
                          SL.library=SL.library,
                          cvControl=list(V=10))
  	
	Qbar.n0.A0 <- predict(Qbar.n0,newdata=control)$pred
	Qbar.n0.A1 <- predict(Qbar.n0,newdata=treatment)$pred
	
	ATE <- mean(Qbar.n0.A1 - Qbar.n0.A0)

	print(sprintf("ATE is %f",ATE))

	## take care of TMLE
	Q = cbind(Qbar.n0.A0,Qbar.n0.A1)
	tmle.out = tmle(Y,A,W,Q=Q,g1W=g.est)$estimates$ATE$psi

	print(sprintf("TMLE is %f", tmle.out))
	  
	return(c(ATE,iptw.est,tmle.out))
}))