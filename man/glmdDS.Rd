\name{glmdDS}
\alias{glmdDS}
\title{Fits a generalized linear model}
\usage{
glmdDS(formula, family, beta.vect = NULL, offset, weights, data)
}
\arguments{
  \item{formula}{an object of class \code{formula}}

  \item{family}{a description of the error distribution and
  link function to used in the model}

  \item{beta.vect}{a string character: the starting values
  for the parameters in the linear predictor}

  \item{offset}{null or a numreric vector that can be used
  to specify an a priori known component to be included in
  the linear predictor during fitting.}

  \item{weights}{an optional vector of 'prior weights' to
  be used in the fitting process. Should be NULL or a
  numeric vector.}

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
in the package \code{dsmodellingclient}
}
\author{
Burton, P.; Gaye, A.; Laflamme, P.
}

