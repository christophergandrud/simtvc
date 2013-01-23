\name{tvc}
\alias{tvc}
\title{Create a time interaction variable}
\usage{
  tvc(data, b, tvar, tfunc = "linear", pow = NULL)
}
\arguments{
  \item{data}{a data frame}

  \item{b}{the non-time interacted variable's name}

  \item{tvar}{the time variable's name}

  \item{tfunc}{function of time that btvc was multiplied
  by. Default is "linear". Can also be "log" (natural log)
  and "power". If tfunc = "power" then the pow argument
  needs to be specified also.}

  \item{pow}{if tfunc = "power", then use pow to specify
  what power to raise the time interaction to.}
}
\value{
  a vector
}
\description{
  \code{tvc} creates a time interaction varible that can be
  used in a coxph model (or any other model with time
  interactions)
}
\seealso{
  \code{\link{ggtvc}}, \code{coxsimtvc},
  \code{\link{survival}}, and \code{\link{coxph}}
}

