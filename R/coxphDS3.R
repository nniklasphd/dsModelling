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
coxphDS3 <- function (data, survival_time, terms, beta.vect, index_str) {
  # get the value of the 'data' parameter provided as character on the client side
  if (is.null(data)){
    dataset <- NULL 
  } else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  #Convert parameters from transmittable (character) format to numeric   
  beta.vect     <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  index			    <- as.numeric(unlist(strsplit(index_str, split=",")))
  features      <- unlist(strsplit(terms, split=","))
  
  n_features    <- length(features)
  dataset       <- dataset[order(dataset[, survival_time]),]
  data_features <- dataset[, features]
  #ZBc           <- exp(data_features %*% beta.vect);
  #thetaZtmpc    <- data_features * do.call("cbind", rep(list(ZBc), n_features))
  temp1 <- c(exp(data_features%*%beta.vect))
  temp2 <- rev(cumsum(rev(temp1)))
  sum_matrix <- (apply(apply(apply(data_features*temp1,2,rev),2,cumsum),2,rev)/temp2)[index,]
  
  zz <- array(0,c(dim(dataset)[1],n_features,n_features))
  for(i in 1:(dim(dataset)[1])){zz[i,,] <- data_features[i,] %*% t(data_features[i,])}
  sum_array <- (apply(apply(apply(zz*temp1,c(2,3),rev),c(2,3),cumsum),c(2,3),rev)/temp2)[index,,]

  
  #return(list(exp.Zc.beta = ZBc, theta.Ztmpc = thetaZtmpc))
  return(list(ebz = temp2, zebz = sum_matrix, zzebz = sum_array))
}
#coxphDS3
