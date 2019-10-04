#' @title Distributed Cox model learning, local initialization.
#'
#' @param data a character, the name of the data frame that holds the data.
#' @param survival_time character, the survivial time variable
#' @param terms comma separated string containing the terms for the model
#'
#' @return a list of time values and aggregated statistics based on local data.
#' @author Niklas, N.
#' @export
#' 
coxphDS1 <- function (data, survival_time) {
  # get the value of the 'data' parameter provided as character on the client side
  if(is.null(data)){
    dataset <- NULL 
  }else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  dataset       <- dataset[order(dataset[, survival_time]),]
  time_values   <- dataset[, survival_time]
  
  return(list(time.values = time_values))
}
#coxphDS1
