\name{coxsimtvc}
\alias{coxsimtvc}
\title{Simulate time-varying hazard ratios for coxph fitted model objects}
\usage{
  coxsimtvc(obj, b, btvc, nsim = 1000, from, to, by, ...)
}
\arguments{
  \item{obj}{a coxph fitted model object with a time
  interaction. NOTE: currently only supports log time
  interactions.}

  \item{b}{the non-time interacted variable name}

  \item{btvc}{the time interacted variable name}

  \item{nsim}{the number of simulations to run per point in
  time. Default is nsim = 1000}

  \item{from}{point in time from when to begin simulating
  coefficient values}

  \item{to}{point in time to stop simulating coefficient
  values}

  \item{by}{intervals by which to simulate coefficient
  values}
}
\value{
  a simtvc object
}
\description{
  \code{coxsimtvc} simulates a time-varying hazard ratios
  from coxph fitted model objects
}
\details{
  Simulates time-varying hazard ratios using estimates from
  a \code{coxph} proportional hazards model. The resulting
  simulation values can be plotted using \code{ggtvc}.
}
\references{
  Licht, Amanda A. 2011. “Change Comes with Time:
  Substantive Interpretation of Nonproportional Hazards in
  Event History Analysis.” Political Analysis 19: 227–43.
}
\seealso{
  \code{\link{ggtvc}}, \code{\link{survival}}, and
  \code{\link{coxph}}
}

