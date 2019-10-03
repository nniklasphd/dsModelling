#' @title Distributed Cox model learning step3, calculate aggregated statistics..
#'
#' @param data a character, the name of the data frame that holds the data.
#' @param survival_time character, the survivial time variable
#' @param terms comma separated string containing the terms for the model
#' @param beta.vect comma separated string containing the model weight parameters
#'
#' @return a list of aggregated statistics based on local data.
#' @author Niklas, N.
#' @export
#' 
coxphDS3 <- function (data, survival_time, survival_event, terms, beta.vect, data_times_str) {
  # get the value of the 'data' parameter provided as character on the client side
  if (is.null(data)){
    dataset <- NULL 
  } else{
    dataset <- as.matrix(eval(parse(text=data)))
  }
  
  #Convert parameters from transmittable (character) format to numeric   
  beta.vect     <- as.numeric(unlist(strsplit(beta.vect, split=",")))
  data_times    <- as.numeric(unlist(strsplit(data_times_str, split=",")))
  features      <- unlist(strsplit(terms, split=","))
    
  n_features    <- length(features)
  dataset       <- dataset[order(dataset[, survival_time]),]
  data_features <- dataset[, features]
  time_values   <- dataset[, survival_time]
  delta_values  <- dataset[, survival_event]
  tuniq         <- unique(time_values)
  n <- length(tuniq)
 	
  zz <- array(0,c(dim(dataset)[1],n_features,n_features))
  for(i in 1:n_features)
	{
		for(j in 1:n_features)
			{
				zz[,i,j] <- data_features[,i]*data_features[,j]
			}
	}
	
  #for(i in 1:n){index[i]<- length(time_values[time_values==tuniq[i]&delta_values==1])+1}
  #for(i in 1:n){index[i]<- length(time_values[time_values<tuniq[i]])+1}
  #for(i in 1:length(data_times)){index[[i]] <-  which(!(time_values<data_times[i] & delta_values==0)&!(time_values<=data_times[i] & delta_values==1))}
  
  temp1 <- c(exp(data_features%*%beta.vect))
  ebz <- rep(0,length(data_times))
  zebz <- matrix(0,length(data_times),n_features)
  zzebz <- array(0,c(length(data_times),n_features,n_features))
  for(i in 1:length(data_times))
  {
	  #ind <- !(time_values<data_times[i] & delta_values==0) & !(time_values<=data_times[i] & delta_values==1)
	  ind <- !(time_values<data_times[i])
	  ebz[i] <- sum(temp1[ind])
	  if(sum(ind)==1){
	  zebz[i,] <- (data_features*temp1)[ind,]
	  zzebz[i,,] <- (zz*temp1)[ind,,]
	  } else {
          zebz[i,] <- colSums((data_features*temp1)[ind,])
	  zzebz[i,,] <- colSums((zz*temp1)[ind,,],dims=1)
          }
  }
  #temp2 <- rev(cumsum(rev(temp1)))
  #temp2 <- temp2[index]
  #sum_matrix <- (apply(apply(apply(data_features*temp1,2,rev),2,cumsum),2,rev))[index,]
  #sum_matrix <- apply(apply(apply(data_features*temp1,2,rev),2,cumsum),2,rev)/temp2[index,]
    
  #sum_array <- (apply(apply(apply(zz*temp1,c(2,3),rev),c(2,3),cumsum),c(2,3),rev))[index,,]
  #sum_array <- (apply(apply(apply(zz*temp1,c(2,3),rev),c(2,3),cumsum),c(2,3),rev)/temp2)[index,,]

  #tmp0 <- rep(0,length(data_times))
  #tmp1 <- matrix(0,length(data_times),n_features)
  #tmp2 <- array(0,c(length(data_times),n_features,n_features))
  
  #tmp0[is.element(data_times,tuniq)] <- temp2
  #tmp1[is.element(data_times,tuniq),] <- sum_matrix
  #tmp2[is.element(data_times,tuniq),,] <- sum_array
  
  #temp2 <- tmp0
  #sum_matrix <- tmp1
  #sum_array <- tmp2
  
  #sum_matrix <- matrix(0,length(data_times),n_features)
  #sum_array <- array(0,c(length(data_times),n_features,n_features))
  
  return(list(ebz=ebz, zebz = zebz, zzebz = zzebz))
}
#coxphDS3
