#'
#' @title Fits a generalized linear model
#' @description Fits a GLM for one iteration only
#' @details This function cannot be called through 'datashield.aggregate' like other aggregate functions
#' It is only called from within the client function \code{ds.glm} in the package \code{dsmodellingclient}
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
glmDS <- function (formula, family, beta.vect, offset, weights, data) {
  
  # get the value of the 'data' and 'weights' parameters provided as character on the client side
  if(is.null(data)){
    dataTable <- NULL 
  }else{
    dataTable <- eval(parse(text=data))
  } 
  
  # this bit deals with cases where 'offset' is a loose vector or a vector within another data frame than 'data' or NULL
  # reconstruct the formula to include an offset variable if the user did specify one
  if(is.null(offset)){
    formulatext <- Reduce(paste, deparse(formula))
  }else{
    myterms <- unlist(strsplit(offset, split='$', fixed=TRUE))
    if(length(myterms) > 1){
      offsetname <- offset
    }else{
      if(!(is.null(data))){
        if(offset %in% colnames(dataTable)){
          offsetname <- paste0(data, "$", offset)           
        }else{
          offsetname <- offset          
        }
      }else{
        offsetname <- offset
      }
    }
    formulatext <- paste0(Reduce(paste, deparse(formula)), paste0(" + offset(", offsetname, ")"))
  }
  
  # rewrite formula extracting variables nested in strutures like data frame or list (e.g. D$A~D$B will be re-written A~B)
  originalFormula <- formulatext
  formulatext <- gsub(" ", "", formulatext, fixed=TRUE)
  formulatext <- gsub("~", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("+", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("*", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("offset(", "|", formulatext, fixed=TRUE)
  formulatext <- gsub(")", "|", formulatext, fixed=TRUE)
  formulatext <- gsub("||", "|", formulatext, fixed=TRUE)
  lpvariables <- unlist(strsplit(formulatext, split="|", fixed=TRUE))
  for(i in 1:length(lpvariables)){
    elt <- unlist(strsplit(lpvariables[i], split="$", fixed=TRUE))
    if(length(elt) > 1){
      assign(elt[length(elt)], eval(parse(text=lpvariables[i])))
      originalFormula <- gsub(lpvariables[i], elt[length(elt)], originalFormula, fixed=TRUE)
    }
  }
  
  # if 'weights' are defined fit model accordingly
  if(is.null(weights)){
    formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula)))) 
    mod.glm.ds <- glm(formula2use, family=family, x=TRUE, control=glm.control(maxit=1), constrast=NULL, data=dataTable)
  }else{
    formula2use <- originalFormula
    mod2eval <- paste0("glm(",formula2use,  ",family=", family, ",x=TRUE,", "control=glm.control(maxit=1),constrast=NULL,data=dataTable,weights=",weights, ")")
    mod.glm.ds <- eval(parse(text=mod2eval))
  }
  
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
