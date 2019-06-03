#' @title Distributed Cox model learning, local initialization based on global unique time values.
#'
#' @param survival_time survivial time
#' @param survival_event survivial event
#' @param terms terms for the model
#' @param tuniq unique time values
#' @param data a character, the name of an optional data frame containing the variables in 
#' in the \code{formula}. 
#'
#' @return a list of aggregated statistics based on local data.
#' @export
#'
#' @author Inberg, G.
#' 
coxphDS2 <- function (survival_time, survival_event, terms, tuniq, data) {
  # get the value of the 'data' parameter provided as character on the client side
  if (is.null(data)){
    dataset <- NULL 
  } else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  #Convert parameters from transmittable (character) format to numeric 
  tuniq         <- as.numeric(unlist(strsplit(tuniq, split=",")))
  features      <- as.numeric(unlist(strsplit(terms, split=",")))
  
  dataset       <- dataset[order(dataset[, survival_time]),]
  data_features <- dataset[, features]
  time_values   <- dataset[, survival_time]
  delta_values  <- dataset[, survival_event];

  # calculate index, Di, sumZ
  no_t  <- length(tuniq)
  index <- DI <- c()
  sumZ  <- NULL
  for (k in 1:no_t) {
    T_idx      <- (time_values == tuniq[k])
    index      <- c(index, sum(T_idx))
    Delta_idx  <- (delta_values == 1)
    total_idx  <- (T_idx & Delta_idx)
    DI         <- c(DI, sum(total_idx))
    col_sum    <- colSums(data_features[total_idx, ,drop = FALSE])
    if (is.null(sumZ)) {
      sumZ <- col_sum
    } else {
      sumZ <- rbind(sumZ, col_sum)
    }
  }
  return(list(index = index, DI = DI, sum.Z = sumZ))
}
#coxphDS2

