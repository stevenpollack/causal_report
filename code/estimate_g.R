## estimate.g
## INPUT
## data : a data file containing all of the elements of sdw
## n.folds : number of folds
## OUTPUT
## 

estimate.g = function(data,n.folds){
	n = nrow(data)
	col.y = which(colnames(data)=='Y')
	if(length(col.y)>0){
		data = data[,-col.y]}
	A = data[['A']]-1
	W = data[,-which(colnames(data)=='A')]
	SL.lib = c('SL.mean','SL.earth','SL.rpartPrune','SL.glmnet',paste('SL.glm',1:4,sep=''))
	sl.out = SuperLearner(A,W,family='binomial',SL.library=SL.lib,cvControl=list(V=10))
	return(predict(sl.out)$pred)
}
