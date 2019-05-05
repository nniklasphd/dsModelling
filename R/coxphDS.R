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
coxphDS <- function (survival_time, survival_event, terms, method, data) {

  # get the value of the 'data' parameter provided as character on the client side
  if(is.null(data)){
    dataTable <- NULL
  }else{
    dataTable <- eval(parse(text=data))
  }

  terms_str <- paste0("(", paste(terms, collapse = " + "), ")")
  formula   <- as.formula(paste0("Surv(", survival_time, ",", survival_event, ") ~ ", terms_str))
  result    <- survival::coxph(formula, data = dataTable, method = method)
  
  return(result)
}
