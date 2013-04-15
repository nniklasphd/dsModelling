#' Generalized linear model.
#'
#' @title aasasas
#'
#' @param formula the formula
#' @param family sdsd
#' @param beta.vect dfsdfsdf
#' @export
#'
glm.ds <- function (formula, family, beta.vect=NULL) {

  mod.glm.ds <- glm(formula, family=family, x=TRUE, control=glm.control(maxit=1), constrast=NULL)

  X.mat <- as.matrix(mod.glm.ds$x)

  if(is.null(beta.vect)) {
    beta.vect <- rep(0,dim(X.mat)[2])
  }

  numsubs<-dim(X.mat)[1]

  y.vect<-as.vector(mod.glm.ds$y)

  lp.vect<-X.mat%*%beta.vect

  f<-mod.glm.ds$family

  mu.vect<-f$linkinv(lp.vect)
  mu.eta.val<-f$mu.eta(lp.vect)
  var.vect<-f$variance(mu.vect)
  dev<-sum(f$dev.resids(y.vect, mu.vect, rep(1, length(y.vect))))

  W.vect<-as.vector(mu.eta.val^2/var.vect)
  WX.mat<-W.vect*X.mat
  info.matrix<-t(X.mat)%*%WX.mat

  u.vect<-(y.vect-mu.vect)*1/var.vect
  W.u.mat<-matrix(W.vect*u.vect)
  score.vect<-t(X.mat)%*%W.u.mat

  list(family=f, info.matrix=info.matrix, score.vect=score.vect, numsubs=numsubs, dev=dev)
}
