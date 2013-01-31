\name{coxsimtvc}
\alias{coxsimtvc}
\title{Simulate time-varying hazard ratios from coxph fitted model objects}
\usage{
  coxsimtvc(obj, b, btvc, tfun = "linear", pow = NULL,
    nsim = 1000, from, to, by, ci = "95", strata = FALSE)
}
\arguments{
  \item{obj}{a coxph fitted model object with a time
  interaction.}

  \item{b}{the non-time interacted variable's name}

  \item{btvc}{the time interacted variable's name}

  \item{nsim}{the number of simulations to run per point in
  time. Default is \code{nsim = 1000}.}

  \item{tfun}{function of time that btvc was multiplied by.
  Default is "linear". Can also be "log" (natural log) and
  "power". If \code{tfun = "power"} then the pow argument
  needs to be specified also.}

  \item{pow}{if \code{tfun = "power"}, then use pow to
  specify what power the time interaction was raised to.}

  \item{from}{point in time from when to begin simulating
  coefficient values}

  \item{to}{point in time to stop simulating coefficient
  values}

  \item{by}{time intervals by which to simulate coefficient
  values}

  \item{ci}{the proportion of middle simulations to keep.
  The default is \code{ci = "95"}, i.e. keep the middle 95
  percent. Other possibilities include: \code{"90"},
  \code{"99"}, \code{"all"}.}

  \item{strata}{logical for whether or not the coxph model
  used stratification and you would like to simulate the
  hazard rates for each strata}
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
  a \code{coxph} proportional hazards model. If strata =
  TRUE then the hazard rate for each strata is calculated
  assuming that b is set to 1 and all other variables are
  set to zero. The resulting simulation values can be
  plotted using \code{ggtvc}. Note: the stratified
  simulations only incorporate estimation uncertainty for
  the coefficient, not the baseline hazards.
}
\examples{
# Load Golub & Steunenberg (2007) Data
data("GolubEUPData")

# Load survival package
library(survival)

# Create natural log time interactions
GolubEUPData$Lqmv <- tvc(GolubEUPData, b = "qmv", tvar = "end", tfun = "log")

# Run Cox PH Model
M1 <- coxph(Surv(begin, end, event) ~  qmv + Lqmv,
           data = GolubEUPData,
           ties = "efron")

# Create simtvc object
simM1 <- coxsimtvc(obj = M1, b = "qmv", btvc = "Lqmv",
                 tfun = "log", from = 80, to = 2000,
                 by = 15, ci = "99")
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

