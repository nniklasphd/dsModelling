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
coxphDS2 <- function (survival_time, survival_event, terms, method, tuniq, data) {
  
  if (is.null(data)){
    dataset <- NULL 
  } else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  # data properties
  n_features <- ncol(dataset) - 2
  dataset    <- dataset[order(dataset[, n_features+1]),]
  Zc         <- dataset[, 1:n_features];
  Tc         <- dataset[, n_features + 1]
  Deltac     <- dataset[, n_features + 2];

  # calculate index, Di, sumZ
  no_t    <- length(tuniq)
  index <- DI <- c()
  sumZ <- NULL
  for (k in 1:no_t) {
    T_idx <- (Tc == tuniq[k])
    index <- c(index, sum(T_idx))
    Delta_idx  <- (Deltac == 1)
    total_idx  <- (T_idx & Delta_idx)
    DI <- c(DI, sum(total_idx))
    col_sum <- colSums(Zc[total_idx, ,drop = FALSE])
    if (is.null(sumZ)) {
      sumZ = col_sum
    } else {
      sumZ = rbind(sumZ, col_sum)
    }
  }
  return(list(index = index, DI = DI, sum.Z = sumZ))
}
#coxphDS2