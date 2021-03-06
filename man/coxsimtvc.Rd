\name{coxsimtvc}
\alias{coxsimtvc}
\title{Simulate time-varying hazards from coxph fitted model objects}
\usage{
  coxsimtvc(obj, b, btvc, qi = "Relative Hazard", Xj = 1,
    Xl = 0, tfun = "linear", pow = NULL, nsim = 1000, from,
    to, by, ci = "95", strata = FALSE)
}
\arguments{
  \item{obj}{a coxph fitted model object with a time
  interaction.}

  \item{b}{the non-time interacted variable's name.}

  \item{btvc}{the time interacted variable's name.}

  \item{qi}{character string indicating what quantity of
  interest you would like to calculate. Can be
  \code{'Relative Hazard'}, \code{'First Difference'}, or
  \code{'Hazard Ratio'}. Default is \code{qi = 'Relative
  Hazard'}. If \code{qi = 'First Difference'} or \code{qi =
  'Hazard Ratio'} then you can set \code{Xj} and \code{Xl}.
  If \code{qi = 'First Difference'} then \code{strata} must
  be \code{FALSE}.}

  \item{Xj}{numeric vector of fitted values for Xj. Must be
  the same length as Xl. Default is \code{Xj = 1} Only
  applies if \code{qi = 'First Difference'} or \code{qi =
  'Hazard Ratio'}.}

  \item{Xl}{numeric vector of fitted values for Xl. Must be
  the same length as Xj. Default is \code{Xl = 0}. Only
  applies if \code{qi = 'First Difference'} or \code{qi =
  'Hazard Ratio'}.}

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
  \code{coxsimtvc} simulates time-varying hazards from
  coxph fitted model objects using the normal distribution.
}
\details{
  Simulates time-varying relative hazards, first
  differences, and hazard ratios using parameter estimates
  from \code{coxph} models. Can also simulate hazard rates
  for multiple strata.

  Relative hazards are found using: \deqn{RH = e^{\beta_{1}
  + \beta_{2}f(t)}} where \eqn{f(t)} is the function of
  time.

  First differences are found using: \deqn{FD = (e^{(X_{j}
  - X_{l}) (\beta_{1} + \beta_{2}f(t))} - 1) * 100} where
  \eqn{X_{j}} and \eqn{X_{l}} are some values of \eqn{X} to
  contrast.

  Hazard ratios are calculated using: \deqn{FD = e^{(X_{j}
  - X_{l}) (\beta_{1} + \beta_{2}f(t))}} When simulating
  non-stratifed time-varying harzards \code{coxsimtvc} uses
  the point estimates for a given coefficient
  \eqn{\hat{\beta}_{x}} and its time interaction
  \eqn{\hat{\beta}_{xt}} along with the variance matrix
  (\eqn{\hat{V}(\hat{\beta})}) estimated from a
  \code{coxph} model. These are used to draw values of
  \eqn{\beta_{x}} and \eqn{\beta_{xt}} from the
  multivariate normal distribution \eqn{N(\hat{\beta},\:
  \hat{V}(\hat{beta}))}.

  When simulating stratified time-varying hazard rates
  \eqn{H} for a given strata \eqn{k}, \code{coxsimtvc}
  uses: \deqn{H_{kx} = \hat{\beta_{k0}}\exp{\hat{\beta_{x}}
  + \beta_{xt}(t)}} The resulting simulation values can be
  plotted using \code{\link{ggtvc}}.
}
\examples{
# Load Golub & Steunenberg (2007) Data
data("GolubEUPData")

# Load survival package
library(survival)

# Create natural log time interactions
Golubtvc <- function(x){
  assign(paste0("l", x), tvc(GolubEUPData, b = x, tvar = "end", tfun = "log"))
}

GolubEUPData$Lcoop <-Golubtvc("coop")
GolubEUPData$Lqmv <- Golubtvc("qmv")
GolubEUPData$Lbacklog <- Golubtvc("backlog")
GolubEUPData$Lcodec <- Golubtvc("codec")
GolubEUPData$Lqmvpostsea <- Golubtvc("qmvpostsea")
GolubEUPData$Lthatcher <- Golubtvc("thatcher")

# Run Cox PH Model
M1 <- coxph(Surv(begin, end, event) ~
            qmv + qmvpostsea + qmvpostteu +
            coop + codec + eu9 + eu10 + eu12 +
            eu15 + thatcher + agenda + backlog +
            Lqmv + Lqmvpostsea + Lcoop + Lcodec +
            Lthatcher + Lbacklog,
         data = GolubEUPData,
         ties = "efron")

# Create simtvc object for Relative Hazard
Sim1 <- coxsimtvc(obj = M1, b = "qmv", btvc = "Lqmv",
                   tfun = "log", from = 80, to = 2000,
                   by = 15, ci = "99")

# Create simtvc object for First Difference
Sim2 <- coxsimtvc(obj = M1, b = "backlog", btvc = "Lbacklog",
                  qi = "First Difference",
                  tfun = "log", from = 80, to = 2000,
                  by = 15, ci = "99")

# Create simtvc object for Hazard Ratio
Sim3 <- coxsimtvc(obj = M1, b = "backlog", btvc = "Lbacklog",
                  qi = "Hazard Ratio", Xj = c(191, 229),
                  Xl = c(0, 0),
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
  \code{\link{survival}}, \code{\link{strata}}, and
  \code{\link{coxph}}
}

