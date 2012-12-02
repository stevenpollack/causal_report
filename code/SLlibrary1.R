SL.glm1 <- function(Y, X, newX, family, obsWeights, ...){
	fit.glm <- glm(Y ~ A*Gender*RaceEth + AgeMonths + AgeMonths:Gender + EduLevel + MarStat*HHInc, data=X, family=family, weights=obsWeights)
	pred <- predict(fit.glm, newdata=newX,type='response')
	fit <- list(object=fit.glm)
	class(fit) <- "SL.glm1"
	out <- list(pred=pred, fit=fit)
	return(out)
}

predict.SL.glm1 <- function(object,newdata,...){
	pred <- predict(object = object$object, newdata = newdata, type ="response")
	pred
}

SL.glm2 <- function(Y, X, newX, family, obsWeights, ...){
	fit.glm <- glm(Y ~ A*Gender*MarStat, data=X, family=family, weights=obsWeights)
	pred <- predict(fit.glm, newdata=newX,type='response')
	fit <- list(object=fit.glm)
	class(fit) <- "SL.glm2"
	out <- list(pred=pred, fit=fit)
	return(out)
}

predict.SL.glm2 <- function(object,newdata,...){
	pred <- predict(object = object$object, newdata = newdata, type ="response")
	pred
}

SL.glm3 <- function(Y, X, newX, family, obsWeights, ...){
	fit.glm <- glm(Y ~ A*Gender*AgeMonths*HHInc, data=X, family=family, weights=obsWeights)
	pred <- predict(fit.glm, newdata=newX,type='response')
	fit <- list(object=fit.glm)
	class(fit) <- "SL.glm3"
	out <- list(pred=pred, fit=fit)
	return(out)
}

predict.SL.glm3 <- function(object,newdata,...){
	pred <- predict(object = object$object, newdata = newdata, type ="response")
	pred
}

SL.glm4 <- function(Y, X, newX, family, obsWeights, ...){
	fit.glm <- glm(Y ~ A*ExamDate*MarStat, data=X, family=family, weights=obsWeights)
	pred <- predict(fit.glm, newdata=newX,type='response')
	fit <- list(object=fit.glm)
	class(fit) <- "SL.glm4"
	out <- list(pred=pred, fit=fit)
	return(out)
}

predict.SL.glm4 <- function(object,newdata,...){
	pred <- predict(object = object$object, newdata = newdata, type ="response")
	pred
}

SL.glm5 <- function(Y, X, newX, family, obsWeights, ...){
	fit.glm <- glm(Y ~ A, data=X, family=family, weights=obsWeights)
	pred <- predict(fit.glm, newdata=newX,type='response')
	fit <- list(object=fit.glm)
	class(fit) <- "SL.glm5"
	out <- list(pred=pred, fit=fit)
	return(out)
}

predict.SL.glm5 <- function(object,newdata,...){
	pred <- predict(object = object$object, newdata = newdata, type ="response")
	pred
}
