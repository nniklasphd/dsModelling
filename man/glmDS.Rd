\name{glmDS}
\alias{glmDS}
\title{Fits a generalized linear model}
\usage{
glmDS(formula, family, beta.vect = NULL)
}
\arguments{
  \item{formula}{an object of class \code{formula}}

  \item{family}{a description of the error distribution and
  link function to used in the model}

  \item{beta.vect}{a string character: the starting values
  for the parameters in the linear predictor}
}
\value{
a list which contains: the fitted \code{family}, a score
vector and an information matrix
}
\description{
Fits a GLM for one iteration only
}
\details{


This function cannot be called through
'datashield.aggregate' like other aggregate functions It is
only called from within the client function \code{ds.glm}
in the package \code{dsmodellingclient}
}
\author{
Burton, P.; Laflamme, P.; Gaye, A.
}

