#'
#' @title Fits a generalized linear model
#' @description Fits a GLM for one iteration only
#' @details This function cannot be called through 'datashield.aggregate' like other aggregate functions
#' It is only called from within the client function \code{ds.glm} in the package \code{dsmodellingclient}
#' @param formula an object of class \code{formula}
#' @param family a description of the error distribution and link function to
#' used in the model
#' @param beta.vect a string character: the starting values for the parameters in the linear predictor
#' @param offset  null or a numreric vector that can be used to specify an a priori known component to be 
#' included in the linear predictor during fitting.
#' @param weights  an optional vector of 'prior weights' to be used in the fitting process. Should be NULL 
#' or a numeric vector.
#' @param data a character, the name of an optional data frame containing the variables in 
#' in the \code{formula}. 
#' @return a list which contains: the fitted \code{family}, a score vector and an information matrix
#' @author Burton, P.; Gaye, A.; Laflamme, P.
#' @export
#'
glmDS <- function (formula, family, beta.vect=NULL, offset, weights, data) {
  
  # reconstruct the formula that was re-written on the client side to protect some of the symbols
  formula <- gsub( "TILDA", "~", formula, fixed=TRUE)
  formula <- gsub("ASTERIX", "*", formula, fixed=TRUE) 
  
  # if the user did provide an offset vector alter the formula the include the offset vector
  if(is.null(offset)){
    formula <- as.formula(formula)    
  }else{
    offsetVector <- eval(parse(text=offset))
    formulaText <- paste0(formula, "+offset(offsetVector)")
    formula <- as.formula(formulaText)
  }
  
  # get the value of the parameter provided as character in the client side
  ww <- eval(parse(text=weights))
  dt <- eval(parse(text=data))
  mod.glm.ds <- glm(formula, family=family, x=TRUE, control=glm.control(maxit=1), constrast=NULL, weights=ww, data=dt)
  
  X.mat <- as.matrix(mod.glm.ds$x)
  
  if(is.null(beta.vect)) {
    beta.vect <- rep(0,dim(X.mat)[2])
  }else{
    beta.vect <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  }
  
  numsubs<-dim(X.mat)[1]
  
  y.vect<-as.vector(mod.glm.ds$y)
  
  lp.vect <- X.mat%*%beta.vect
  
  f<-mod.glm.ds$family
  
  mu.vect<-f$linkinv(lp.vect)
  mu.eta.val<-f$mu.eta(lp.vect)
  var.vect<-f$variance(mu.vect)
  dev<-sum(f$dev.resids(y.vect, mu.vect, rep(1, length(y.vect))))
  
  W.vect<-as.vector(mu.eta.val^2/var.vect)
  WX.mat<-W.vect*X.mat
  info.matrix<-t(X.mat)%*%WX.mat
  
  u.vect<-(y.vect-mu.vect)*1/var.vect
  W.u.mat<-matrix(W.vect*u.vect)
  score.vect<-t(X.mat)%*%W.u.mat
  
  return(list(family=f, info.matrix=info.matrix, score.vect=score.vect, numsubs=numsubs, dev=dev))
  
}
