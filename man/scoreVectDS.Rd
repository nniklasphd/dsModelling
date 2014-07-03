\name{scoreVectDS}
\alias{scoreVectDS}
\title{Generates the score vector and information matrix}
\usage{
scoreVectDS(data, formula, family, clusterID, corstr, alpha, phi,
  startBetas = NULL, zcor = NULL)
}
\arguments{
  \item{data}{the input dataframe which contains the
  variable specified in the formula.}

  \item{formula}{a regression formula.}

  \item{family}{the link function for the regression.}

  \item{clusterID}{the name of the column that holds the
  cluster IDs.}

  \item{corstr}{the correlation structure.}

  \item{alpha}{the parameter alpha}

  \item{phi}{the parameter alpha}

  \item{startBetas}{starting values for the beta
  estimates.}

  \item{zcor}{the user defined matrix user if the
  correlation structure is 'fixed'.}
}
\value{
a list
}
\description{
This function is called by the client function 'ds.gee' to
produced the score vector and information matrix.
}
\details{


the score vector and information matrix are calculated
according to the correlation structure.
}
\author{
Gaye, A.; Jones EM.
}

