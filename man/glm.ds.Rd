\name{glm.ds}
\alias{glm.ds}
\title{Fits a generalized linear model}
\usage{
  glm.ds(formula, family, beta.vect = NULL)
}
\arguments{
  \item{formula}{an object of class \code{formula}}

  \item{family}{a description of the error distribution and
  link function to used in the model}

  \item{beta.vect}{a string character: the starting values
  for the parameters in the linear predictor. THE VALUES
  CONCATENATED INTO A STRING WITH ',' AS SPLITTING
  CHARACTER; the string is split within the function and
  the values turned into numeric for use.}
}
\value{
  a list which contains: the fitted \code{family}, a score
  vector and an information matrix
}
\description{
  THIS FUNCTION IS NOT USED ALONE, IT IS CALLED BY THE
  FUNCTION \code{ds.glm} FROM THE PACKAGE
  \code{dsmodellingclient}.
}
\details{
  Generalized linear modellling
}
\examples{
{
# Please see the examples in the documentation of the function 'ds.glm' from the package 'dsmodellingclient'
}
}
\author{
  Burton, P.; Laflamme, P.; Gaye, A.
}

