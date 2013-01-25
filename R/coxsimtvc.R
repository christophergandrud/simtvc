#' Simulate time-varying hazard ratios for coxph fitted model objects
#' 
#' \code{coxsimtvc} simulates a time-varying hazard ratios from coxph fitted model objects using the normal distribution.
#' @param obj a coxph fitted model object with a time interaction. 
#' @param b the non-time interacted variable's name
#' @param btvc the time interacted variable's name
#' @param nsim the number of simulations to run per point in time. Default is nsim = 1000.
#' @param tfunc function of time that btvc was multiplied by. Default is "linear". Can also be "log" (natural log) and "power". If tfunc = "power" then the pow argument needs to be specified also.
#' @param pow if tfunc = "power", then use pow to specify what power the time interaction was raised to.
#' @param from point in time from when to begin simulating coefficient values
#' @param to point in time to stop simulating coefficient values
#' @param by time intervals by which to simulate coefficient values
#' @param ci the proportion of middle simulations to keep. The default is "95", i.e. keep the middle 95 percent. Other possibilities include: "90", "99", "all".
#' @details Simulates time-varying hazard ratios using estimates from a \code{coxph} proportional hazards model. The resulting simulation values can be plotted using \code{ggtvc}.
#' @return a simtvc object
#' @seealso \code{\link{ggtvc}}, \code{\link{survival}}, and \code{\link{coxph}}
#' @import MSBVAR plyr reshape2
#' @export
#' @references Licht, Amanda A. 2011. “Change Comes with Time: Substantive Interpretation of Nonproportional Hazards in Event History Analysis.” Political Analysis 19: 227–43.

coxsimtvc <- function(obj, b, btvc, tfunc = "linear", pow = NULL, nsim = 1000, from, to, by, ci = "95")
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
  if (tfunc == "linear"){
    Range <- seq(from = from, to = to, by = by)
  } else if (tfunc == "log"){
    Range <- log(seq(from = from, to = to, by = by))
  } else if (tfunc == "power"){
    Range <- (seq(from = from, to = to, by = by))^pow
  }
  
  TVSim <- outer(Drawn[,2], Range)
  TVSim <- data.frame(melt(TVSim))
  TVSim <- rename(TVSim, c(Var1 = "ID", Var2 = "Time", value = "TVC"))
  TVSim <- merge(Drawn, TVSim, by = "ID")

  TVSim$CombCoef <- TVSim[[2]] + TVSim$TVC
  TVSim$HR <- exp(TVSim$CombCoef)

  TVSim <- TVSim[order(TVSim$Time),]
  
  if (ci == "all"){
    TVSimPerc <- TVSim 
  } else if (ci == "95"){
    TVSimPerc <- ddply(TVSim, .(Time), transform, Lower = HR < quantile(HR, c(0.025)))
    TVSimPerc <- ddply(TVSimPerc, .(Time), transform, Upper = HR > quantile(HR, 0.975))
    TVSimPerc <- subset(TVSimPerc, Lower == FALSE & Upper == FALSE)
  } else if (ci == "90"){
    TVSimPerc <- ddply(TVSim, .(Time), transform, Lower = HR < quantile(HR, c(0.05)))
    TVSimPerc <- ddply(TVSimPerc, .(Time), transform, Upper = HR > quantile(HR, 0.95))
    TVSimPerc <- subset(TVSimPerc, Lower == FALSE & Upper == FALSE)
  } else if (ci == "99"){
    TVSimPerc <- ddply(TVSim, .(Time), transform, Lower = HR < quantile(HR, c(0.005)))
    TVSimPerc <- ddply(TVSimPerc, .(Time), transform, Upper = HR > quantile(HR, 0.995))
    TVSimPerc <- subset(TVSimPerc, Lower == FALSE & Upper == FALSE)
  }
  
  class(TVSimPerc) <- "simtvc"
  TVSimPerc
}