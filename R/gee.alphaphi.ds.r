#' GEE helper function
#'
#' @title GEE
#'
#' @param formula
#' @param family
#' @param link 
#' @param data
#' @param cluster
#' @param corstr
#' @param start.betas
#' @export
#'
gee.alphaphi.ds <- function (formula, family, link, data, cluster, corstr, start.betas=NULL){
  
  input.table <- data
  id <- cluster
  
  
  corr.strc <- function (int) {
    if(int == 1){
      "ar1"
    }else if(int == 2){
      "exchangeable"
    } else if(int == 3){
      "independence"
    }else if(int == 4){
      "fixed"
    }else if(int == 5){
      "unstructured"
    }
  }
  corstr <- corr.strc(corstr)
  
  link.func <- function (int) {
    if(int == 1){
      "logit"
    }else if(int == 2){
      "identity"
    } else if(int == 3){
      "inverse"
    }else if(int == 4){
      "log"
    }else if(int == 5){
      "probit"
    }
  }
  
  linkf <- link.func(link)
  
  if(is.function(family)) {
    family <- family(linkf)
  }
  print(family)
  
  # THESE TWO LINES GET THE 'X' and "Y" WE WERE GETTING THROUGH ONE the geeglm FUNCTION OF 'GEEPACK'
  frame <- model.frame(formula, data);
  X.mat <- model.matrix(formula, frame)
  Y.vec <- as.vector(model.response(frame, type="numeric"))
  
  npara <- dim(X.mat)[2]
  N <- length(id)
  clusnew <- c(which(diff(as.numeric(id)) != 0), length(id))
  clusz <- c(clusnew[1], diff(clusnew))
  N.clus <- length(clusz)
  maxclsz <- max(clusz)
  
  # EXTRACTING FAMILY AND SIMILAR INFORMATION
  f <- family 
  LINKS <- c("identity", "logit", "probit", "log", "inverse")
  VARIANCES <- c("gaussian", "binomial", "poisson", "Gamma")
  mean.link <- f$link
  variance <- f$variance
  mean.link.v <- pmatch(mean.link, LINKS, -1, TRUE)
  mu <- quasi(LINKS[mean.link.v])$linkinv(X.mat%*%start.betas)
  
  # EXTRACTING SUMMARY STATISTICS TO CALCULATE PHI
  pr2 <- (Y.vec - mu)^2/f$variance(mu)
  pr <-(Y.vec - mu)/sqrt(f$variance(mu))
  sum_p <- sum(pr2)
  phi <- (N-npara)^{-1}*sum_p						 # PHI ZEGER AND LIANG
  
  # PREPARING TO ESTIMATE ALPHA
  pearson <- matrix(pr, ncol=1)
  mat.clus <- vector("list", N.clus)
  mat.clus[[1]] <- pearson[1:clusnew[1]]%*%t(pearson[1:clusnew[1]])
  for(i in 2:N.clus){
    mat.clus[[i]] <- pearson[(clusnew[i-1]+1):clusnew[i]]%*%t(pearson[(clusnew[i-1]+1):clusnew[i]])
  }
  
  # FOR EXCHANGEABLE COR STRUCTURE
  if(corstr=="exchangeable"){
    clus.sq <- vector("list", N.clus)
    prod.clus <- rep(0,N.clus)
    clus.sq[[1]] <- sum(pearson[1:clusnew[1]]^2)
    prod.clus[[1]] < -sum(mat.clus[[1]])-clus.sq[[1]]
    for(i in 2:N.clus){
      clus.sq[[i]] <- sum(pearson[(clusnew[i-1]+1):clusnew[i]]^2)
      prod.clus[[i]] <- sum(mat.clus[[i]])-clus.sq[[i]]
    }
    temp <- sum(prod.clus)
  }
  
  # FOR UNSTRUCTURED COR STRUCTURE
  if(corstr=="unstructured"){
    temp <- array(rep(0,(N.clus*max(clusz)^2)), dim=c(N.clus,max(clusz),max(clusz)))
    alpha.unstructured <- matrix(0, nrow=max(clusz), ncol=max(clusz))
    pearson.unstr <- vector("list", N.clus)
    pearson.unstr[[1]] <- matrix(c(pr[1:clusnew[1]]), ncol=1)
    for(i in 2:N.clus){
      pearson.unstr[[i]] <- matrix(c(pr[(clusnew[i-1]+1):clusnew[i]]), ncol=1)
    }
    for(i in 1:N.clus){
      for(j in 1:clusz[i]){
        for(m in 1:clusz[i]){
          temp[i,j,m] <- pearson.unstr[[i]][j]
          alpha.unstructured[j,m] <- sum(temp[,j,m])/((N.clus-npara)*phi)
        }
      }
    }
    alpha.unstructured <- alpha.unstructured[col(alpha.unstructured) < row(alpha.unstructured)]
  }
  
  # FOR AR1 COR STRUCTURE
  if(corstr=="ar1"){
    sum.component<-vector("list", N.clus)
    component<-matrix(rep(0,(N.clus*(max(clusz)-1))), nrow=N.clus)
    for(i in 1:N.clus){
      for(j in 1:(clusz[i]-1)){
        component[i,j] <- mat.clus[[i]][j,j+1]
      }
    }
    temp <- sum(component)
  }
  
  # SET ALPHA ACCORDING TO THE SPECIFIED CORRELATION STRUCTURE
  if(corstr=="exchangeable"){
    M <- phi*sum(clusz*(clusz-1))-phi*npara
    alpha <- temp/M 
    M_study <- sum(clusz*(clusz-1))
    alphaM <- M*alpha
  }
  
  if(corstr=="ar1"){
    M <- phi* sum((clusz-1))-phi*npara
    alpha <- temp/M 
    M_study <- sum((clusz-1))
    alphaM <- M*alpha
  }
  
  if(corstr=="unstructured"){
    M_study <- sum((clusz-1))
    alphaM <-(N.clus-npara)*phi*alpha.unstructured
  }
  
  if(corstr=="independence"|corstr=="fixed"){
    M_study <- NULL
    alphaM <- NULL
  }
  
  list(N=N, npara=npara, M_study=M_study, alphaM=alphaM, sum_p=sum_p) 
}
