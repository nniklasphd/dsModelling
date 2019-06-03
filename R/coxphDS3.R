#' @title Cox proportional-hazards model.
#'
#' @param survival_time survivial time
#' @param survival_event survivial event
#' @param terms terms for the model
#' @param beta.vect model weight parameters
#' @param data a character, the name of an optional data frame containing the variables in 
#' in the \code{formula}. 
#'
#' @return a list of aggregated statistics based on local data.
#' @export
#'
#' @author Inberg, G.
#' 
coxphDS3 <- function (survival_time, survival_event, terms, beta.vect, data) {
  # get the value of the 'data' parameter provided as character on the client side
  if (is.null(data)){
    dataset <- NULL 
  } else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  #Convert beta.vect from transmittable (character) format to numeric   
  beta.vect <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  
  # data properties
  n_features <- ncol(dataset) - 2
  dataset    <- dataset[order(dataset[, n_features+1]),]
  Zc         <- dataset[, 1:n_features]

  ZBc        <- exp(Zc %*% beta.vect);
  thetaZtmpc <- Zc * do.call("cbind", rep(list(ZBc), n_features))
  
  return(list(exp.Zc.beta = ZBc, theta.Ztmpc = thetaZtmpc))
}
#coxphDS3
