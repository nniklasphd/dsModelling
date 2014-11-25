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
#' @return a list which contains: the fitted \code{family}, a score vector and an information matrix
#' @author Burton, P.; Laflamme, P.; Gaye, A.
#' @export
#'
glmDS <- function (formula, family, beta.vect=NULL, offset) {

  mod.glm.ds <- glm(formula, family=family, x=TRUE, control=glm.control(maxit=1), constrast=NULL)

  X.mat <- as.matrix(mod.glm.ds$x)

  if(is.null(beta.vect)) {
    beta.vect <- rep(0,dim(X.mat)[2])
  }else{
    beta.vect <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  }

  numsubs<-dim(X.mat)[1]

  y.vect<-as.vector(mod.glm.ds$y)

  lp.vect.temp <- X.mat%*%beta.vect
  if(is.null(offset)){
    lp.vect <- lp.vect.temp
  }else{
    # check for missing values in the outcome vector 
    # and delete the correpsonding entries in the offset vector
    offsetVector <- eval(parse(text=offset))  
    formulatext <- gsub(" ", "", deparse(formula), fixed=TRUE)
    outcomeName <- strsplit(deparse(formulatext), split='~')[[1]]
    outcomeVector <- eval(parse(text=outcomeName)) 
    indxNA <- which(is.na(outcomeVector))
    if(length(indxNA) > 0){offsetVector <- offsetVector[-indxNA]}
    
    lp.vect <- lp.vect.temp + offsetVector   
  }


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

  list(family=f, info.matrix=info.matrix, score.vect=score.vect, numsubs=numsubs, dev=dev)
}
