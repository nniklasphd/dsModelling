\name{lexushelper1}
\alias{lexushelper1}
\title{Computes time breaks given the exit time}
\usage{
lexushelper1(stopTime, interval)
}
\arguments{
  \item{stopTime}{a numeric, the exit time time.}

  \item{interval}{a numeric vector of one or more elements
  which give the sought interval width.}
}
\value{
a list which contains the break points for each subject.
}
\description{
This is an internal function required by the client.
function \code{ds.lexus} to get the interval break points
for each subject.
}
\details{

}
\author{
Gaye, A.
}
\keyword{internal}

