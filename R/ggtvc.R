#' Create a plot of simulated time-varying hazard ratios or stratified time-varying hazard rates from a simtvc class object using ggplot2
#' 
#' \code{ggtvc} uses ggplot2 to plot the simulated hazard ratios from a simtvc class object using ggplot2. 
#' Note: A dotted line is created at y = 1, i.e. no effect.
#' @param obj a simtvc class object
#' @param strata logical whether or not you would like to plot the hazard rate for the separate strata
#' @param xlab a label for the plot's x-axis
#' @param ylab a lable of the plot's y-axis
#' @param title the plot's main title
#' @param xbreaks breaks for x axis tick marks. These will be on the scale you used for the transformed function of time.
#' @param xlabels labels for the x axis tick marks. These should be on the real time scale. 
#' @param smoother what type of smoothing line to use to summarize the plotted coefficient
#' @param colour colour of the simulated points. Default is hexadecimal colour A6CEE3.
#' @param spalette colour palette for stratified hazard rates. Only works if strata = TRUE. Default palette is "Set1". See \code{\link{scale_colour_brewer}}.
#' @param leg.name name of the stratified hazard rates legend. Only works if strata = TRUE.
#' @param lsize size of the smoothing line. Default is 2. See \code{\link{ggplot2}}.
#' @param psize size of the plotted simulation points. Default is 1. See \code{\link{ggplot2}}.
#' @param palpha point alpha (e.g. transparency). Default is 0.05. See \code{\link{ggplot2}}.
#' @param ... other arguments passed to specific methods
#' @return a ggplot object
#' @details Plots either a time variying hazard ratio or the hazard rates for multiple strata. Currently to change the strata legend labels you need to do this manually (see \code{\link{revalue}}) in the simtvc object with the strata component. Also, currently the x-axis tick marks and break labels must be adjusted manually for non-linear functions of time.
#' @examples
#' # Load Golub & Steunenberg (2007) Data
#' data("GolubEUPData")
#' 
#' # Load survival package
#' library(survival)
#' 
#' # Create natural log time interactions
#' GolubEUPData$Lqmv <- tvc(GolubEUPData, b = "qmv", tvar = "end", tfun = "log")
#' 
#' # Run Cox PH Model
#' M1 <- coxph(Surv(begin, end, event) ~  qmv + Lqmv, 
#'            data = GolubEUPData,
#'            ties = "efron")
#'
#' # Create simtvc object
#' simM1 <- coxsimtvc(obj = M1, b = "qmv", btvc = "Lqmv", 
#'                  tfun = "log", from = 80, to = 2000, 
#'                  by = 15, ci = "99")
#'                  
#' # Graph simulated time-variying hazard ratios from simtvc object
#' ggtvc(simM1)
#' @seealso \code{\link{coxsimtvc}} and \code{\link{ggplot2}}
#' @import ggplot2
#' @export

ggtvc <- function(obj, strata = FALSE, xlab = NULL, ylab = NULL, title = NULL, xbreaks = NULL, xlabels = NULL, smoother = "auto", colour = "#A6CEE3", spalette = "Set1", leg.name = "", lsize = 2, psize = 1, palpha = 0.1, ...)
{
  if (!inherits(obj, "simtvc")) 
    stop("must be a simtvc object")
  
  if (strata == TRUE){
    colour <- NULL
    objdf <- data.frame(obj$time, obj$HRate, obj$strata)
    names(objdf) <- c("Time", "HRate", "Strata")

    ggplot(objdf, aes(Time, HRate, colour = factor(Strata))) +
      geom_point(alpha = I(palpha), size = psize) +
      geom_smooth(method = smoother, size = lsize, se = FALSE) +
      scale_y_continuous()+
      scale_x_continuous() +
      xlab(xlab) + ylab(ylab) +
      scale_colour_brewer(palette = spalette, name = leg.name) +
      ggtitle(title) +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      theme_bw(base_size = 15)

  } else if (strata == FALSE){
    spalette <- NULL
    objdf <- data.frame(obj$time, obj$HR)
    names(objdf) <- c("Time", "HR")
    ggplot(objdf, aes(Time, HR)) +
      geom_point(shape = 21, alpha = I(palpha), size = psize, colour = colour) +
      geom_smooth(method = smoother, size = lsize, se = FALSE) +
      geom_hline(aes(yintercept = 1), linetype = "dotted") +
      scale_y_continuous()+
      scale_x_continuous() +
      xlab(xlab) + ylab(ylab) +
      ggtitle(title) +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) +  
      theme_bw(base_size = 15)
  }
}