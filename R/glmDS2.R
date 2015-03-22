#'
#' @title Fits a generalized linear model
#' @description Fits a GLM for one iteration only
#' @details This function cannot be called through 'datashield.aggregate' like other aggregate functions
#' It is only called from within the client function \code{ds.glm} in the package \code{dsModellingClient}
#' @param formula an object of class \code{formula}.
#' @param family a character, description of the error distribution and link function to used in the model.
#' @param beta.vect a string character: the starting values for the parameters in the linear predictor.
#' @param offset  a character, null or a numeric vector that can be used to specify an a priori known component 
#' to be included in the linear predictor during fitting.
#' @param weights  a character, the name of an optional vector of 'prior weights' to be used in the fitting 
#' process. Should be NULL or a numeric vector.
#' @param data a character, the name of an optional data frame containing the variables in 
#' in the \code{formula}. 
#' @return a list which contains: the fitted \code{family}, a score vector and an information matrix
#' @author Gaye, A.; Burton, P.; Laflamme, P.
#' @export
#'
glmDS2 <- function (formula, family, beta.vect, offset, weights, data) {
  
  # get the value of the 'data' and 'weights' parameters provided as character on the client side
  if(is.null(data)){
    dataTable <- NULL 
  }else{
    dataTable <- eval(parse(text=data))
  }
  
  # rewrite formula extracting variables nested in strutures like data frame or list (e.g. D$A~D$B will be re-written A~B)
  formulatext <- Reduce(paste, deparse(formula))
  originalFormula <- formulatext
  formulatext <- gsub(" ", "", formulatext, fixed=TRUE)
  formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("||", "|", formulatext, fixed=TRUE)
  
  # lpvariables incluse both yvect and linear predictor components 
  lpvariables <- unlist(strsplit(formulatext, split="|", fixed=TRUE))
  
  varnames <- c()
  for(i in 1:length(lpvariables)){
    elt <- unlist(strsplit(lpvariables[i], split="$", fixed=TRUE))
    if(length(elt) > 1){
      assign(elt[length(elt)], eval(parse(text=lpvariables[i])))
      originalFormula <- gsub(lpvariables[i], elt[length(elt)], originalFormula, fixed=TRUE)
      varnames <- append(varnames, elt[length(elt)])
    }else{
      varnames <- append(varnames, elt)
    }
  }
  varnames <- unique(varnames)
  
  formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula)))) # here we need the formula as a 'call' object
  mod.glm.ds <- glm(formula2use, family=family, x=TRUE, control=glm.control(maxit=1), contrasts=NULL, data=dataTable)
  
  # remove rows of offset or weights data with missing Y,X
  # both weights and offset
  if(!(is.null(weights))&&!(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", weights, ",", offset,")")
    dtemp <- eval(parse(text=cbindtext))
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[complete.cases(dtemp),]
    offsetvar.orig <- cmplt[, dim(cmplt)[2]] 
    weightsvar.orig <- cmplt[, (dim(cmplt)[2]-1)]    
  }
  
  # offset no weights 
  if(is.null(weights)&&!(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", offset, ")")
    dtemp <- eval(parse(text=cbindtext))
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[complete.cases(dtemp),]
    offsetvar.orig <- cmplt[, dim(cmplt)[2]]    
  }
  
  # weights no offset
  if(!(is.null(weights))&&(is.null(offset))){
    cbindtext <- paste0("cbind(", paste(varnames, collapse=","), ",", weights, ")")
    dtemp <- eval(parse(text=cbindtext))
    # now get the above table with no missing values (i.e. complete) and grab the offset variable (the last column)
    cmplt <- dtemp[complete.cases(dtemp),]
    weightsvar.orig <- cmplt[, dim(cmplt)[2]]    
  } 
  
  # work with Y vector and X nmatrix from original model (mutual missings removed but not missings in offset or weights)
  X.mat.orig <- as.matrix(mod.glm.ds$x)
  y.vect.orig <- as.vector(mod.glm.ds$y)
  f <- mod.glm.ds$family
  
  # strip additional missings in offset or weights
  # both weights and offset
  if(!(is.null(weights))&&!(is.null(offset))){
    YXWO.orig <- cbind(y.vect.orig,X.mat.orig,weightsvar.orig,offsetvar.orig)
    YXWO.complete <- YXWO.orig[complete.cases(YXWO.orig),]
    numcol.YXWO <- dim(YXWO.orig)[2]
    y.vect <- YXWO.complete[,1]
    X.mat <- YXWO.complete[,(2:(numcol.YXWO-2))]
    weightsvar <- YXWO.complete[,numcol.YXWO-1]
    offsetvar <- YXWO.complete[,numcol.YXWO]
  }
  
  # offset no weights
  if(is.null(weights)&&!(is.null(offset))){
    YXO.orig <- cbind(y.vect.orig,X.mat.orig,offsetvar.orig)
    YXO.complete <- YXO.orig[complete.cases(YXO.orig),]
    numcol.YXO <- dim(YXO.orig)[2]
    y.vect <- YXO.complete[,1]
    X.mat <- YXO.complete[,(2:(numcol.YXO-1))]
    weightsvar <- rep(1,length(y.vect))
    offsetvar <- YXO.complete[,numcol.YXO]
  }
  
  # weights no offset
  if(!(is.null(weights))&&(is.null(offset))){
    YXW.orig <- cbind(y.vect.orig,X.mat.orig,weightsvar.orig)
    YXW.complete <- YXW.orig[complete.cases(YXW.orig),]
    numcol.YXW <- dim(YXW.orig)[2]
    y.vect <- YXW.complete[,1]
    X.mat <- YXW.complete[,(2:(numcol.YXW-1))]
    weightsvar <- YXW.complete[,numcol.YXW]
    offsetvar <- rep(0,length(y.vect))
  }
  
  # no weights or offset
  if(is.null(weights)&&(is.null(offset))){
    y.vect <- y.vect.orig
    X.mat <- X.mat.orig
    weightsvar <- rep(1,length(y.vect))
    offsetvar <- rep(0,length(y.vect))
  }

  numsubs <- length(y.vect)
  
  # convert beta.vect from character to numeric format
  beta.vect.n <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  
  # add offset to LP if offset specified
  if(!is.null(offset)){
    lp.vect <- (X.mat%*%beta.vect.n)+offsetvar
  }else{
    lp.vect <- (X.mat%*%beta.vect.n)   
  }  
  
  mu.vect <- f$linkinv(lp.vect)
  mu.eta.val <- f$mu.eta(lp.vect)
  var.vect <- f$variance(mu.vect)
  dev <- sum(f$dev.resids(y.vect, mu.vect, rep(1, length(y.vect))))
  
  # now multiply working weights by prior weights if prior weights specified
  if(!is.null(weights)){
    W.vect <- as.vector(mu.eta.val^2/var.vect)
    W.vect <- W.vect*weightsvar
  }else{
    W.vect <- as.vector(mu.eta.val^2/var.vect)
  }
  
  WX.mat <- W.vect*X.mat
  info.matrix <- t(X.mat)%*%WX.mat
  
  u.vect <- (y.vect-mu.vect)*1/var.vect
  W.u.mat <- matrix(W.vect*u.vect)
  score.vect <- t(X.mat)%*%W.u.mat
  
  return(list(family=f, info.matrix=info.matrix, score.vect=score.vect, numsubs=numsubs, dev=dev))
  
}
