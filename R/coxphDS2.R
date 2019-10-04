#' @title Distributed Cox model learning, local initialization based on global unique time values.
#'
#' @param data a character, the name of the data frame that holds the data.
#' @param survival_time character, the survivial time variable
#' @param survival_event character, the survivial event variable
#' @param terms comma separated string containing the terms for the model
#' @param tuniq comma separated string containing the unique time values
#'
#' @return a list of aggregated statistics based on local data.
#' @author Niklas, N.
#' @export
#' 
coxphDS2 <- function (data, survival_time, survival_event, terms, tuniq) {
  # get the value of the 'data' parameter provided as character on the client side
  if (is.null(data)){
    dataset <- NULL 
  } else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  #Convert parameters from transmittable (character) format to numeric 
  tuniq         <- as.numeric(unlist(strsplit(tuniq, split=",")))
  features      <- unlist(strsplit(terms, split=","))
  
  dataset       <- dataset[order(dataset[, survival_time]),]
  data_features <- dataset[, features]
  time_values   <- dataset[, survival_time]
  delta_values  <- dataset[, survival_event];
 
  n <- length(tuniq)
  m <- length(features)
  s <- matrix(0,n,m)
  d <- rep(0,n)
 
  for(i in 1:n)
  {
	if (length(time_values[time_values==tuniq[i]&delta_values==1])>1)
		s[i,] <- colSums(data_features[time_values==tuniq[i]&delta_values==1,])
	if (length(time_values[time_values==tuniq[i]&delta_values==1])==1)
		s[i,] <- data_features[time_values==tuniq[i]&delta_values==1,]
	if (length(time_values[time_values==tuniq[i]&delta_values==1])==0)
		s[i,] <- rep(0,m)
	d[i] <- length(time_values[time_values==tuniq[i]&delta_values==1])
  }

 return(list(DI = d, sum.Z = s))
}
#coxphDS2
