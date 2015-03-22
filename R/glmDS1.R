#'
#' @title get matrix dimensions
#' @description Fits a GLM for one iteration only
#' @details This function cannot be called through 'datashield.aggregate' like other aggregate functions
#' It is only called from within the client function \code{ds.glm} in the package \code{dsModellingClient}
#' @param formula an object of class \code{formula}.
#' @param family a character, description of the error distribution and link function to used in the model.
#' @param beta.vect a string character: the starting values for the parameters in the linear predictor.
#' @param data a character, the name of an optional data frame containing the variables in 
#' in the \code{formula}. 
#' @return a list which contains: the fitted \code{family}, a score vector and an information matrix
#' @author Gaye, A.; Burton, P.; Laflamme, P.
#' @export
#'
glmDS1 <- function (formula, family, beta.vect, data) {
  
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
  
  # evaluate model fit on server side to identify the number of model parameters 
  formula2use <- as.formula(paste0(Reduce(paste, deparse(originalFormula)))) # here we need the formula as a 'call' object
  mod.glm.ds <- glm(formula2use, family=family, x=TRUE, control=glm.control(maxit=1), contrasts=NULL, data=dataTable)
  
  X.mat <- as.matrix(mod.glm.ds$x)
  
  dimX<-dim((X.mat))
  
  return(list(dimX=dimX))
}
