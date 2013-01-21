#' Simulate time-varying hazard ratios for coxph fitted model objects
#' 
#' \code{coxsimtvc} simulates a time-varying hazard ratios from coxph fitted model objects
#' @param obj a coxph fitted model object with a time interaction. NOTE: currently only supports log time interactions.
#' @param b the non-time interacted variable name
#' @param btvc the time interacted variable name
#' @param nsim the number of simulations to run per point in time. Default is nsim = 1000
#' @param from point in time from when to begin simulating coefficient values
#' @param to point in time to stop simulating coefficient values
#' @param by intervals by which to simulate coefficient values
#' @details Simulates time-varying hazard ratios using estimates from a \code{coxph} proportional hazards model. The resulting simulation values can be plotted using \code{ggtvc}.
#' @return a simtvc object
#' @seealso \code{\link{ggtvc}}, \code{\link{survival}}, and \code{\link{coxph}}
#' @import MSBVAR plyr reshape2
#' @export
#' @references Licht, Amanda A. 2011. “Change Comes with Time: Substantive Interpretation of Nonproportional Hazards in Event History Analysis.” Political Analysis 19: 227–43.

coxsimtvc <- function(obj, b, btvc, nsim = 1000, from, to, by, ci = 95, ...)
{
  Coef <- matrix(obj$coefficients)
  VC <- vcov(obj)
    
  Drawn <- rmultnorm(n = nsim, mu = Coef, vmat = VC)
  DrawnDF <- data.frame(Drawn)
 
  dfn <- names(DrawnDF)
  bpos <- match(b, dfn)
  btvcpos <- match(btvc, dfn)
  
  Drawn <- data.frame(Drawn[, c(bpos, btvcpos)])
  Drawn$ID <- 1:nsim
  
  # add ability to change time function
  Range <- log(seq(from = from, to = to, by = by))
  
  TVSim <- outer(Drawn[,2], Range)
  TVSim <- data.frame(melt(TVSim))
  TVSim <- rename(TVSim, c(Var1 = "ID", Var2 = "Time", value = "TVC"))
  TVSim <- merge(Drawn, TVSim, by = "ID")

  TVSim$CombCoef <- TVSim[[2]] + TVSim$TVC
  TVSim$HR <- exp(TVSim$CombCoef)

  TVSim <- TVSim[order(TVSim$Time),]
  
  if 
  
  TVSimPerc <- ddply(TVSim, .(Time), transform, Lower = HR < quantile(HR, c(0.025)))
  TVSimPerc <- ddply(TVSimPerc, .(Time), transform, Upper = HR > quantile(HR, c(0.975)))
  TVSimPerc <- subset(TVSimPerc, Lower == FALSE & Upper == FALSE)
  
  class(TVSimPerc) <- "simtvc"
  TVSimPerc
}