\name{coxsimtvc}
\alias{coxsimtvc}
\title{Simulate time-varying hazard ratios for coxph fitted model objects}
\usage{
  coxsimtvc(obj, b, btvc, tfunc = "linear", pow = NULL,
    nsim = 1000, from, to, by, ci = "95", ...)
}
\arguments{
  \item{obj}{a coxph fitted model object with a time
  interaction.}

  \item{b}{the non-time interacted variable name}

  \item{btvc}{the time interacted variable name}

  \item{nsim}{the number of simulations to run per point in
  time. Default is nsim = 1000.}

  \item{tfunc}{function of time that btvc was multiplied
  by. Default is "linear". Can also be "log" (natural log)
  and "power". If tfunc = "power" then the pow argument
  needs to be specified also.}

  \item{pow}{if tfunc = "power", then use pow to specify
  what power the time interaction was raised to.}

  \item{from}{point in time from when to begin simulating
  coefficient values}

  \item{to}{point in time to stop simulating coefficient
  values}

  \item{by}{time intervals by which to simulate coefficient
  values}

  \item{ci}{the proportion of middle simulations to keep.
  The default is "95", i.e. keep the middle 95 percent.
  Other possibilities include: "90", "99", "all".}
}
\value{
  a simtvc object
}
\description{
  \code{coxsimtvc} simulates a time-varying hazard ratios
  from coxph fitted model objects using the normal
  distribution.
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

