#' Create a plot of simulated time-varying coefficients from a simtvc class object using ggplot2
#' 
#' \code{ggtvc} uses ggplot2 to plot the simulated hazard ratios from a simtvc class object using ggplot2. 
#' Note: A dotted line is created at y = 1, i.e. no effect.
#' @param obj a simtvc class object
#' @param xlab a label for the plot's x-axis
#' @param ylab a lable of the plot's y-axis
#' @param title the plot's main title
#' @param xbreaks breaks for x axis tick marks. These will be on the scale you used for the transformed function of time.
#' @param xlabels labels for the x axis tick marks. These should be on the real time scale. 
#' @param smoother what type of smoothing line to use to summarize the plotted coefficient
#' @param colour color of the simulated points. Default is hexadecimal colour A6CEE3.
#' @param ... other arguments passed to specific methods 
#' @return a ggplot object
#' @seealso \code{\link{coxsimtvc}} and \code{\link{ggplot2}}
#' @import ggplot2
#' @export

ggtvc <- function(obj, xlab = NULL, ylab = NULL, title = NULL, xbreaks = NULL, xlabels = NULL, smoother = "auto", colour = "#A6CEE3", ...)
{
  if (!inherits(obj, "simtvc")) 
    stop("must be a simtvc object")
  objdf <- data.frame(obj$Time, obj$HR)
  names(objdf) <- c("Time", "HR")
  ggplot(objdf, aes(Time, HR)) +
          geom_point(shape = 21, alpha = I(0.01), colour = colour, size = 5) +
          geom_smooth(method = smoother) +
          geom_hline(aes(yintercept = 1), linetype = "dotted") +
          scale_y_continuous()+
          scale_x_continuous() +
          xlab(xlab) + ylab(ylab) +
          ggtitle(title) +
          theme_bw(base_size = 15)
}