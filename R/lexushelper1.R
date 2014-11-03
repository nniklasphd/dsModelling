#'
#' @title Computes time breaks given the exit time
#' @description This is an internal function required by the client.
#' function \code{ds.lexus} to get the interval break points for each subject.
#' @param stopTime a numeric, the exit time time. 
#' @param interval a numeric vector of one or more elements which give the sought
#' interval width.
#' @keywords internal
#' @return a list which contains the break points for each subject.
#' @author Gaye, A.
#' 
lexushelper1 <- function(stopTime, interval){
  if(length(interval) < 2){
    output <- unique(c(seq(0,stopTime,by=interval),stopTime))
  }else{
    output <- c(0)
    timeleft <- stopTime
    totaltimesurvived <- 0
    count <- 1
    while(interval[count] < timeleft){
      totaltimesurvived <- totaltimesurvived + interval[count]
      if(timeleft > interval[count]){
        output <- append(output, totaltimesurvived)
      }else{
        break
      }
      timeleft <- timeleft - interval[count]
      count <- count + 1
    }
    output <- c(output, stopTime)
  }
  return(output)
}
