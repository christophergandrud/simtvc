\name{ggtvc}
\alias{ggtvc}
\title{Create a plot of simulated time-varying hazard ratios from a simtvc class object using ggplot2}
\usage{
  ggtvc(obj, strata = FALSE, xlab = NULL, ylab = NULL,
    title = NULL, xbreaks = NULL, xlabels = NULL,
    smoother = "auto", colour = "#AGCEE3",
    spalette = "Set1", leg.name = "", lsize = 2, psize = 1,
    palpha = 0.1, ...)
}
\arguments{
  \item{obj}{a simtvc class object}

  \item{strata}{logical whether or not you would like to
  plot the hazard rate for the separate strata}

  \item{xlab}{a label for the plot's x-axis}

  \item{ylab}{a lable of the plot's y-axis}

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
  rates. Only works if strata = TRUE. Default palette is
  "Set1". See \code{\link{scale_colour_brewer}}.}

  \item{leg.name}{name of the stratified hazard rates
  legend. Only works if strata = TRUE.}

  \item{lsize}{size of the smoothing line. Default is 2.
  See \code{\link{ggplot2}}.}

  \item{psize}{size of the plotted simulation points.
  Default is 1. See \code{\link{ggplot2}}.}

  \item{palpha}{point alpha (e.g. transparency). Default is
  0.05. See \code{\link{ggplot2}}.}

  \item{...}{other arguments passed to specific methods}
}
\value{
  a ggplot object
}
\description{
  \code{ggtvc} uses ggplot2 to plot the simulated hazard
  ratios from a simtvc class object using ggplot2. Note: A
  dotted line is created at y = 1, i.e. no effect.
}
\details{
  Plots either a time variying hazard ratio or the hazard
  rates for multiple strata. Currently to change the strata
  legend labels you need to do this manually (see
  \code{\link{revalue}} in the simtvc object with the
  strata component.
}
\seealso{
  \code{\link{coxsimtvc}} and \code{\link{ggplot2}}
}

