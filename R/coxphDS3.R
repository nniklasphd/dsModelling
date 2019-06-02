#' @title Cox proportional-hazards model.
#'
#' @param survival_time survivial time
#' @param survival_event survivial event
#' @param terms terms for the model
#' @param method method for tie handling
#' @param beta.vect weight parameters
#' @param data a character, the name of an optional data frame containing the variables in 
#' in the \code{formula}. 
#'
#' @return the coxph model.
#' @export
#'
#' @author Inberg, G.
#' 
coxphDS3 <- function (survival_time, survival_event, terms, method, beta.vect, data) {
  
  if (is.null(data)){
    dataset <- NULL 
  } else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  #Convert beta.vect from transmittable (character) format to numeric 
  beta.vect <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  
  # data properties
  n_feat  <- ncol(dataset) - 2
  dataset <- dataset[order(dataset[, n_feat+1]),]
  Zc      <- dataset[, 1:n_feat];

  ZBc        <- exp(Zc %*% beta.vect);
  thetaZtmpc <- Zc * do.call("cbind", rep(list(ZBc), n_feat))
  
  return(list(exp.Zc.beta = ZBc, theta.Ztmpc = thetaZtmpc))
}
#coxphDS3