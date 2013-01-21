#' Create a plot of simulated time-varying coefficients from a simtvc class object using ggplot2
#' 
#' \code{ggtvc} uses ggplot2 to plot the simulated hazard ratios from a simtvc class object using ggplot2. 
#' @param obj a simtvc class object
#' @param xlab a label for the plot's x-axis
#' @param ylab a lable of the plot's y-axis
#' @param title the plot's main title
#' @param xbreaks breaks for x axis tick marks
#' @param xlabels labels for the x axis tick marks 
#' @param ybreaks breaks for y axis tick marks
#' @param ylabels labels for the y axis tick marks
#' @param colour color of the simulated points. Default is hexadecimal colour A6CEE3.
#' @param ... other arguments passed to specific methods 
#' @return a ggplot object
#' @seealso \code{\link{coxsimtvc}} and \code{\link{ggplot2}}
#' @import ggplot2
#' @export

ggtvc <- function(obj, xlab = NULL, ylab = NULL, title = NULL, xbreaks = NULL, xlabels = NULL, ybreaks = NULL, ylabels = NULL, colour = "#A6CEE3", ...)
{
  if (class(obj) != "simtvc"){
    cat(paste("---------", obj, " is not a simtvc class object ----------", sep = " "))
    stop()
  } else {
  objdf <- data.frame(obj$Time, obj$HR)
  names(objdf) <- c("Time", "HR")
  ggplot(objdf, aes(Time, HR)) +
          geom_point(shape = 21, alpha = I(0.01), colour = colour, size = 5) +
          geom_smooth() +
          geom_hline(aes(yintercept = 1), linetype = "dotted") +
          scale_y_continuous(breaks = ybreaks, labels = ylabels )+
          scale_x_continuous(breaks = xbreaks, labels = xlabels) +
          xlab(xlab) + ylab(ylab) +
          ggtitle(title) +
          theme_bw(base_size = 15)
  }
}