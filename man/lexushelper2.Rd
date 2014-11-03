\name{lexushelper2}
\alias{lexushelper2}
\title{Computes time periods of each subject}
\usage{
lexushelper2(dt, ids, exits, interval)
}
\arguments{
  \item{dt}{the dataset that contains the original data.}

  \item{ids}{a string character, the name of column that
  holds the individuals IDs.}

  \item{exists,}{a string character, the name of column
  that holds the exit times.}

  \item{interval}{a numeric vector, the sought interval
  widths.}
}
\value{
a list which contains the individuals time periods.
}
\description{
This is an internal function required by the client.
function \code{ds.lexus} to get the time periods for each
subject.
}
\details{

}
\author{
Gaye, A.
}
\keyword{internal}

