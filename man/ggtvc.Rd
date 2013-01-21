\name{ggtvc}
\alias{ggtvc}
\title{Create a plot of simulated time-varying coefficients from a simtvc class object using ggplot2}
\usage{
  ggtvc(obj, xlab = NULL, ylab = NULL, title = NULL,
    xbreaks = NULL, xlabels = NULL, ybreaks = NULL,
    ylabels = NULL, smoother = "auto", colour = "#A6CEE3",
    ...)
}
\arguments{
  \item{obj}{a simtvc class object}

  \item{xlab}{a label for the plot's x-axis}

  \item{ylab}{a lable of the plot's y-axis}

  \item{title}{the plot's main title}

  \item{xbreaks}{breaks for x axis tick marks}

  \item{xlabels}{labels for the x axis tick marks}

  \item{ybreaks}{breaks for y axis tick marks}

  \item{ylabels}{labels for the y axis tick marks}

  \item{smoother}{what type of smoothing line to use to
  summarize the plotted coefficient}

  \item{colour}{color of the simulated points. Default is
  hexadecimal colour A6CEE3.}

  \item{...}{other arguments passed to specific methods}
}
\value{
  a ggplot object
}
\description{
  \code{ggtvc} uses ggplot2 to plot the simulated hazard
  ratios from a simtvc class object using ggplot2.
  \textbf{Note:} A dotted line is created at y = 1, i.e. no
  effect.
}
\seealso{
  \code{\link{coxsimtvc}} and \code{\link{ggplot2}}
}

