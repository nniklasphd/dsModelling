#' @title Cox proportional-hazards model.
#'
#' @param survival_time survivial time
#' @param survival_event survivial event
#' @param terms terms for the model
#' @param method method for tie handling
#' @param data a character, the name of an optional data frame containing the variables in 
#' in the \code{formula}. 
#'
#' @return the coxph model.
#' @export
#'
#' @author Inberg, G.
#' 
coxphDS1 <- function (survival_time, survival_event, terms, method, data) {

  # get the value of the 'data' parameter provided as character on the client side
  if(is.null(data)){
    dataset <- NULL 
  }else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  # data properties
  n_features    <- ncol(dataset) - 2
  data_features <- dataset[, 1:n_features]
  time_values   <- dataset[, n_features + 1]
  ZZvc          <- Conj(t.default(data_features)) %*% data_features
  
  return(list(ZZvc = ZZvc, time.values = time_values))
}
