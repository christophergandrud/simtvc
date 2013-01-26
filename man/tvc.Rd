\name{tvc}
\alias{tvc}
\title{Create a time interaction variable}
\usage{
  tvc(data, b, tvar, tfun = "linear", pow = NULL)
}
\arguments{
  \item{data}{a data frame}

  \item{b}{the non-time interacted variable's name}

  \item{tvar}{the time variable's name}

  \item{tfun}{function of time that btvc was multiplied by.
  Default is "linear". Can also be "log" (natural log) and
  "power". If tfun = "power" then the pow argument needs to
  be specified also.}

  \item{pow}{if tfun = "power", then use pow to specify
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
\details{
  Interacts a variable with a specified function of time.
  Possible functions of time include 'linear', natural
  'log', and exponentiated ('power').
}
\examples{
# Load Golub & Steunenberg (2007) Data
data("GolubEUPData")

# Create natural log time interaction with the qmv variable
GolubEUPData$Lqmv <- tvc(GolubEUPData, b = "qmv", tvar = "end", tfun = "log")
}
\seealso{
  \code{\link{ggtvc}}, \code{coxsimtvc},
  \code{\link{survival}}, and \code{\link{coxph}}
}

