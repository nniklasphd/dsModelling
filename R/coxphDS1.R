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
    dataTable <- NULL 
  }else{
    dataTable <- eval(parse(text=data))
  }
  
  # data features 
  n_rows     <- nrow(dataTable)
  n_features <- ncol(dataTable) - 2

  Zc     <- dataTable[, 1:n_features];
  Tc     <- dataTable[, n_features + 1];
  Deltac <- dataTable[, n_features + 2];
  zzc    <- Conj(t.default(Zc)) %*% Zc
  
  return(list(n.rows = n_rows, n.features = n_features, zzc = zzc))
}
