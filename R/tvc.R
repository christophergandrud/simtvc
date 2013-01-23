#' Create a time interaction variable
#' 
#' \code{tvc} creates a time interaction varible that can be used in a coxph model (or any other model with time interactions)
#' @param data a data frame
#' @param b the non-time interacted variable's name
#' @param tvar the time variable's name
#' @param tfunc function of time that btvc was multiplied by. Default is "linear". Can also be "log" (natural log) and "power". If tfunc = "power" then the pow argument needs to be specified also.
#' @param pow if tfunc = "power", then use pow to specify what power to raise the time interaction to.
#' @return a vector
#' @seealso \code{\link{ggtvc}}, \code{coxsimtvc}, \code{\link{survival}}, and \code{\link{coxph}}
#' @export


tvc <- function(data, b, tvar, tfunc = "linear", pow = NULL)
{
  dfn <- names(data)
  bpos <- match(b, dfn)
  tvarpos <- match(tvar, dfn)
  
  if (tfunc == "linear"){
    data[[bpos]] * data[[tvarpos]]
  } else if (tfunc == "log"){
    data[[b]] * log(data[[tvarpos]])
  } else if (tfunc == "pow") {
    data[[b]] * (data[[tvarpos]])^pow
  }
}