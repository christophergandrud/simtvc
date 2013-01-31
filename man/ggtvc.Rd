\name{ggtvc}
\alias{ggtvc}
\title{Create a plot of simulated time-varying hazard ratios or stratified time-varying hazard rates from a simtvc class object using ggplot2}
\usage{
  ggtvc(obj, strata = FALSE, xlab = NULL, ylab = NULL,
    title = NULL, xbreaks = NULL, xlabels = NULL,
    smoother = "auto", colour = "#A6CEE3",
    spalette = "Set1", leg.name = "", lsize = 2, psize = 1,
    palpha = 0.1, ...)
}
\arguments{
  \item{obj}{a simtvc class object}

  \item{strata}{logical whether or not you would like to
  plot the hazard rate for the separate strata}

  \item{xlab}{a label for the plot's x-axis}

  \item{ylab}{a label of the plot's y-axis}

  \item{title}{the plot's main title}

  \item{xbreaks}{breaks for x axis tick marks. These will
  be on the scale you used for the transformed function of
  time.}

  \item{xlabels}{labels for the x axis tick marks. These
  should be on the real time scale.}

  \item{smoother}{what type of smoothing line to use to
  summarize the plotted coefficient}

  \item{colour}{colour of the simulated points. Default is
  hexadecimal colour A6CEE3.}

  \item{spalette}{colour palette for stratified hazard
  rates. Only works if \code{strata = TRUE}. Default
  palette is \code{"Set1"}. See
  \code{\link{scale_colour_brewer}}.}

  \item{leg.name}{name of the stratified hazard rates
  legend. Only works if \code{strata = TRUE}.}

  \item{lsize}{size of the smoothing line. Default is 2.
  See \code{\link{ggplot2}}.}

  \item{psize}{size of the plotted simulation points.
  Default is \code{psize = 1}. See \code{\link{ggplot2}}.}

  \item{palpha}{point alpha (e.g. transparency). Default is
  \code{palpha = 0.05}. See \code{\link{ggplot2}}.}

  \item{...}{other arguments passed to specific methods}
}
\value{
  a ggplot object
}
\description{
  \code{ggtvc} uses ggplot2 to plot the simulated hazard
  ratios from a simtvc class object using ggplot2. Note: A
  dotted line is created at y = 1, i.e. no effect, for
  time-varying hazard ratio graphs.
}
\details{
  Plots either a time varying hazard ratio or the hazard
  rates for multiple strata. Currently to change the strata
  legend labels you need to do this manually (see
  \code{\link{revalue}}) in the simtvc object with the
  strata component. Also, currently the x-axis tick marks
  and break labels must be adjusted manually for non-linear
  functions of time.
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

# Graph simulated time-variying hazard ratios from simtvc object
ggtvc(simM1)
}
\seealso{
  \code{\link{coxsimtvc}} and \code{\link{ggplot2}}
}

