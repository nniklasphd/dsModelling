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
    dataTable <- NULL 
  } else{
    dataTable <- eval(parse(text=data))
  }
  
  # number of observations
  Tc = dataTable[, ncol(dataTable) - 1]
  Tuniq = unique(Tc)
  no_t = length(Tuniq)
  
  # calculate Di, sumZ, index
  index <- c()
  sumZ <- DI <- NULL
  for (k in 1:no_t) {
    T_idx <- (Tc == Tuniq[k])
    index <- c(index, sum(T_idx))
    Delta_idx  <- (Deltac[[1]] == 1)
    total_idx  <- (T_idx & Delta_idx)
    DI <- c(DI, sum(total_idx))
    col_sum <- colSums(Zc[[1]][total_idx, ,drop = FALSE])
    if (is.null(sumZ)) {
      sumZ = col_sum
    } else {
      sumZ = rbind(sumZ, col_sum)
    }
  }
  indexc <- cumsum(c(0, index[1:(length(index)-1)])) + 1;
  
  Zc  <- dataTable[, 1:m];
  ZBc <- exp(Zc %*% beta.vect);
  thetac <- rev(t(apply(apply(ZBc, 2, rev), 2, cumsum)))
  thetaZtmpc <- Zc * do.call("cbind", rep(list(ZBc), m))
  thetaZtmpc <- apply(apply(apply(thetaZtmpc, 2, rev), 2, cumsum), 2, rev)
  thetaZtmpc <- thetaZtmpc / do.call("cbind", rep(list(thetac), m))
  thetaZc <- thetaZtmpc[indexc,]
  thetaZc <- thetaZc * do.call("cbind", rep(list(DI), m))
  Gvc <- sumZc - thetaZc;
  col_sums <- colSums(Gvc)
  
  return(list(col.sums = col_sums))
  
}
#coxphDS2