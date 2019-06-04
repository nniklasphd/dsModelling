#' @title Distributed Cox model learning step3, calculate aggregated statistics..
#'
#' @param data a character, the name of the data frame that holds the data.
#' @param survival_time character, the survivial time variable
#' @param terms comma separated string containing the terms for the model
#' @param beta.vect comma separated string containing the model weight parameters
#'
#' @return a list of aggregated statistics based on local data.
#' @author Inberg, G.
#' @export
#' 
coxphDS3 <- function (data, survival_time, terms, beta.vect) {
  # get the value of the 'data' parameter provided as character on the client side
  if (is.null(data)){
    dataset <- NULL 
  } else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  #Convert parameters from transmittable (character) format to numeric   
  beta.vect     <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  features      <- unlist(strsplit(terms, split=","))
  
  n_features    <- length(features)
  dataset       <- dataset[order(dataset[, survival_time]),]
  data_features <- dataset[, features]
  ZBc           <- exp(data_features %*% beta.vect);
  thetaZtmpc    <- data_features * do.call("cbind", rep(list(ZBc), n_features))
  
  return(list(exp.Zc.beta = ZBc, theta.Ztmpc = thetaZtmpc))
}
#coxphDS3
