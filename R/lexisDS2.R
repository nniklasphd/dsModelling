#'
#' @title
#' TODO: document lexisDS2
#' 
#' @description
#' TODO: document lexisDS2
#'
#' @export
#'

lexisDS2 <- function(datatext=NULL, intervalWidth, maxmaxtime, idCol, entryCol, exitCol, statusCol, vartext=NULL){
 
	starttime<-eval(parse(text=entryCol))
	endtime<-eval(parse(text=exitCol))
	cens<-eval(parse(text=statusCol))
	id.orig<-eval(parse(text=idCol))

	starttime<-as.numeric(starttime)
	endtime<-as.numeric(endtime)

	if(is.factor(cens)){
	cens<-as.character(cens)
	cens<-as.numeric(cens)
}

	CENS<-as.numeric(cens)
	SURVTIME<-endtime-starttime
	STARTTIME<-starttime
	ENDTIME<-endtime
	ID<-id.orig	
	

	


#CREATE A NEW ORDERED SEQUENTIAL ID - INDEPENDENT FROM ORIGINAL idCol
#THIS WILL ENABLE SORTING REGARDLESS OF THE NATURE OF THE ORIGINAL idCol 
#USE exitCol FOR LENGTH BECAUSE idCol MAY POTENTIALLY BE NULL
   	idSeq<-1:length(SURVTIME)


#IDENTIFY VARIABLES TO BE CARRIED WITH THE EXPANDED SURVIVAL DATA

   if(is.null(vartext)){
   datatext2<-paste0("data.frame(",datatext,")")
   DF <- eval(parse(text=datatext2))
}

   if(!is.null(vartext)){
   vartext2<-paste0("data.frame(",vartext,")")
   DF<-eval(parse(text=vartext2))
}

   if(is.null(datatext)&&is.null(vartext)){
   DF<-data.frame(SURVTIME,CENS)
}



#intervalWidth IS A SINGLE VALUE
if(is.null(intervalWidth)||is.na(intervalWidth)||intervalWidth==0){
	return("A VALID NON-ZERO intervalWidth ARGUMENT MUST BE SPECIFIED") 
	}

   
  # if entry time is not provided set all entry times to 0
  if(is.null(entryCol)){
    entryCol <- "STARTTIME"
    STARTTIME <- rep(0, dim(DF)[1])
    DF <- data.frame(DF, STARTTIME)
  }

   DF.orig<-DF
   DF.add<-data.frame(idSeq,ID,STARTTIME,ENDTIME,SURVTIME,CENS)
   DF<-data.frame(DF.add,DF.orig)
   
   carry.data<-DF



#GENERATE BREAKS FROM intervalWidth

###############################
#NEED FIRST CALL FROM CLIENT SIDE TO GET MAX ENDTIME (+RANDOM MASK)
#Here we set from study alone
##################################
#COERCE CHARACTER VALUES OF intervalWidth INTO NUMERIC
intervalWidth<-as.numeric(intervalWidth)

#USE MAX TOTAL SURVIVAL TIME (WITH RANDOM +VE INCREMENT
#ACROSS ALL STUDIES IN ANALYSIS TO DEFINE END OF FINAL PERIOD 
max.end<-maxmaxtime

#IF intervalWidth IS A SINGLE VALUE 
if(length(intervalWidth)==1){
	numfullintervals<-floor(max.end/intervalWidth)
	start.breaks<-c(0,intervalWidth*(1:numfullintervals))
	end.breaks<-c(intervalWidth*(1:numfullintervals),max.end)
	numends<-numfullintervals
	}

#IF intervalWidth IS A VECTOR
if(length(intervalWidth)>1){
	numends<-length(intervalWidth)

	if(sum(intervalWidth)>=max.end){
		start.breaks<-c(0,cumsum(intervalWidth[1:(numends-1)]))
		end.breaks<-cumsum(intervalWidth)
		}		

	if(sum(intervalWidth)< max.end){
		start.breaks<-c(0,cumsum(intervalWidth))
		end.breaks<-c(cumsum(intervalWidth),max.end)
		}		
	}


#STRIP BREAKS where start.break>=max.end
end.breaks<-end.breaks[start.breaks<max.end]
start.breaks<-start.breaks[start.breaks<max.end]
period.surv<-end.breaks-start.breaks


print(start.breaks)
print(end.breaks)
totints<-length(end.breaks)
totsubs<-dim(DF)[1]

print(totints)
print(totsubs)


survival.matrix<-matrix(data=0,totsubs,totints)
cens.matrix<-matrix(0,totsubs,totints)
idSeq.matrix<-matrix(0,totsubs,totints)


numperiods.exposed<-rep(0,totsubs)


for(j in 1:totints){
	numperiods.exposed<-numperiods.exposed+(start.breaks[j]<SURVTIME)
}



#NEED TO ADD DUMMY COLUMNS TO HARMLESSLY DIRECT NA VALUES IN numperiods.exposed


for(m in 1:totints){

			survival.matrix[,m]<-(m<numperiods.exposed)*period.surv[m]+
						(m>=numperiods.exposed)*(SURVTIME-start.breaks[m])	
			cens.matrix[,m]<-(m<numperiods.exposed)*0+
						(m>=numperiods.exposed)*CENS	
			idSeq.matrix[,m]<- idSeq


}

idSeq.vector<-as.vector(idSeq.matrix)
cens.vector<-as.vector(cens.matrix)
survival.vector<-as.vector(survival.matrix)


cens.vector[survival.vector<=0]<-NA
cens.matrix<-matrix(cens.vector,nrow=totsubs,ncol=totints)

idSeq.vector[survival.vector<=0]<-NA
idSeq.matrix<-matrix(idSeq.vector,nrow=totsubs,ncol=totints)


survival.vector[survival.vector<=0]<-NA
survival.matrix<-matrix(survival.vector,nrow=totsubs,ncol=totints)

idSeq.vector<-as.vector(t(idSeq.matrix))
cens.vector<-as.vector(t(cens.matrix))
survival.vector<-as.vector(t(survival.matrix))


expanded.template<-cbind(idSeq.vector,survival.vector,cens.vector)

expanded.template<-expanded.template[complete.cases(expanded.template),]

expanded.carry.data<-carry.data[expanded.template[,1],]

expanded.ID<-dimnames(expanded.carry.data)[1]

expanded.table<-data.frame(expanded.ID,expanded.template,expanded.carry.data)
names(expanded.table)[1]<-"UID.expanded"


return(list(expanded.table=expanded.table))

}
