\name{glmDS1}
\alias{glmDS1}
\title{get matrix dimensions}
\usage{
glmDS1(formula, family, beta.vect, data)
}
\arguments{
  \item{formula}{an object of class \code{formula}.}

  \item{family}{a character, description of the error
  distribution and link function to used in the model.}

  \item{beta.vect}{a string character: the starting values
  for the parameters in the linear predictor.}

  \item{data}{a character, the name of an optional data
  frame containing the variables in in the \code{formula}.}
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
in the package \code{dsModellingClient}
}
\author{
Gaye, A.; Burton, P.; Laflamme, P.
}

