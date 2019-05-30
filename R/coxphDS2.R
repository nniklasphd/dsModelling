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
coxphDS2 <- function (survival_time, survival_event, terms, method, beta.vect, data) {
  
  if (is.null(data)){
    dataset <- NULL 
  } else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  #Convert beta.vect from transmittable (character) format to numeric 
  beta.vect <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  
  # data properties
  n_feat <- ncol(dataset) - 2
  Zc     <- dataset[, 1:n_feat];
  Tc     <- dataset[, n_feat + 1]
  Deltac <- dataset[, n_feat + 2];
  Tuniq  <- unique(Tc)
  no_t   <- length(Tuniq)
  
  # calculate Di, sumZ, index
  index <- DI <- c()
  sumZ <- NULL
  for (k in 1:no_t) {
    T_idx <- (Tc == Tuniq[k])
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
  ZBc        <- exp(Zc %*% beta.vect);
  thetaZtmpc <- Zc * do.call("cbind", rep(list(ZBc), n_feat))
  return(list(idx = index, TC = TC, DI = DI, sum.Z = sumZ, exp.Zc.beta = ZBc, theta.Ztmpc = thetaZtmpc))
}
#coxphDS2