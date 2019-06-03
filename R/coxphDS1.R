#' @title Distributed Cox model learning, local initialization.
#'
#' @param survival_time survivial time
#' @param terms terms for the model
#' @param data a character, the name of an optional data frame containing the variables in 
#' in the \code{formula}. 
#'
#' @return a list of time values and aggregated statistics based on local data.
#' @export
#'
#' @author Inberg, G.
#' 
coxphDS1 <- function (survival_time, terms, data) {
  # get the value of the 'data' parameter provided as character on the client side
  if(is.null(data)){
    dataset <- NULL 
  }else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  #Convert parameters from transmittable (character) format to numeric 
  features      <- as.numeric(unlist(strsplit(terms, split=",")))
  
  dataset       <- dataset[order(dataset[, survival_time]),]
  data_features <- dataset[, features]
  time_values   <- dataset[, survival_time]
  ZZvc          <- Conj(t.default(data_features)) %*% data_features
  
  return(list(ZZvc = ZZvc, time.values = time_values))
}
#coxphDS1
