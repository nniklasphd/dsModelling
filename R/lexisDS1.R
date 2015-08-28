#'
#' @title
#' TODO: document lexisDS1
#' 
#' @description
#' TODO: document lexisDS1
#'
#' @export
#'

lexisDS1 <- function(exitCol=NULL){

	if(is.character(exitCol)){
		exposure<-eval(parse(text=exitCol))
	}else{
		exposure<-exitCol
	}

max.time<-max(exposure,na.rm=TRUE)
random.multiplier<-runif(1,1.05,1.25)

max.time<-max.time*random.multiplier

out.obj<-list(max.time=max.time)

return(out.obj)

}
