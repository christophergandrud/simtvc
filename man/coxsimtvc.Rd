\name{coxsimtvc}
\alias{coxsimtvc}
\title{Simulate time-varying hazard ratios from coxph fitted model objects}
\usage{
  coxsimtvc(obj, b, btvc, tfun = "linear", pow = NULL,
    nsim = 1000, from, to, by, ci = "95")
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

  \item{by}{time intervals at which to simulate coefficient
  values}

  \item{ci}{the proportion of middle simulations to keep.
  The default is \code{ci = "95"}, i.e. keep the middle 95
  percent. Other possibilities include: \code{"90"},
  \code{"99"}, \code{"all"}.}
}
\value{
  a simtvc object
}
\description{
  \code{coxsimtvc} simulates a time-varying hazard ratios
  from \code{coxph} fitted model objects using the
  multivariate normal distribution.
}
\details{
  Simulates time-varying hazard ratios using estimates from
  a \code{coxph} proportional hazards model.

  The function uses the point estimates for a given
  coefficient \eqn{\hat{\beta}_{x}} and its time
  interaction \eqn{\hat{\beta}_{xt}} along with the
  variance matrix (\eqn{\hat{V}(\hat{\beta})}) estimated
  from a \code{coxph} model. These are used to draw values
  of \eqn{\beta_{x}} and \eqn{\beta_{xt}} from the
  multivariate normal distribution \eqn{N(\hat{\beta},\:
  \hat{V}(\hat{beta}))}.

  Using these simulated values combined coefficients
  \eqn{\beta_{xc}} for each point in time \eqn{t} are
  estimated using: \deqn{HR_{xc} = \exp(\beta_{x} +
  \beta_{xt}(t)}) The resulting simulation values can be
  plotted using \code{\link{ggtvc}}.
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

  King, Gary, Michael Tomz, and Jason Wittenberg. 2000.
  “Making the Most of Statistical Analyses: Improving
  Interpretation and Presentation.” American Journal of
  Political Science 44(2): 347–61.
}
\seealso{
  \code{\link{ggtvc}}, \code{\link{rmultinorm}},
  \code{\link{survival}}, and \code{\link{coxph}}
}

