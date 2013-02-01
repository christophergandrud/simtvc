#' Simulate time-varying hazard ratios from coxph fitted model objects
#' 
#' \code{coxsimtvc} simulates a time-varying hazard ratios from coxph fitted model objects using the normal distribution.
#' @param obj a coxph fitted model object with a time interaction. 
#' @param b the non-time interacted variable's name
#' @param btvc the time interacted variable's name
#' @param nsim the number of simulations to run per point in time. Default is \code{nsim = 1000}.
#' @param tfun function of time that btvc was multiplied by. Default is "linear". Can also be "log" (natural log) and "power". If \code{tfun = "power"} then the pow argument needs to be specified also.
#' @param pow if \code{tfun = "power"}, then use pow to specify what power the time interaction was raised to.
#' @param from point in time from when to begin simulating coefficient values
#' @param to point in time to stop simulating coefficient values
#' @param by time intervals by which to simulate coefficient values
#' @param ci the proportion of middle simulations to keep. The default is \code{ci = "95"}, i.e. keep the middle 95 percent. Other possibilities include: \code{"90"}, \code{"99"}, \code{"all"}.
#' @param strata logical for whether or not the coxph model used stratification and you would like to simulate the hazard rates for each strata
#' @return a simtvc object
#' @details Simulates time-varying hazard ratios using estimates from a \code{coxph} proportional hazards model. 
#'
#' When simulating non-stratifed time-varying harzard ratios \code{coxsimtvc} uses the point estimates for a given coefficient \eqn{\hat{\beta}_{x}} and its time interaction \eqn{\hat{\beta}_{xt}} along with the variance matrix (\eqn{\hat{V}(\hat{\beta})}) estimated from a \code{coxph} model. These are used to draw values of \eqn{\beta_{x}} and \eqn{\beta_{xt}} from the multivariate normal distribution \eqn{N(\hat{\beta},\: \hat{V}(\hat{beta}))}.
#'
#' Using these simulated values combined coefficients \eqn{\beta_{xc}} for each point in time \eqn{t} are estimated using:
#' \deqn{HR_{xc} = \exp(\beta_{x} + \beta_{xt}(t))} 
#' When simulating stratified time-varying hazard rates \eqn{H} for a given strata \eqn{k}, \code{coxsimtvc} uses:
#' \deqn{H_{kx} = \hat{\beta_{k0}}\exp{\hat{\beta_{x}} + \beta_{xt}(t)}}
#' The resulting simulation values can be plotted using \code{\link{ggtvc}}. 
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
#' @seealso \code{\link{ggtvc}}, \code{\link{rmultinorm}}, \code{\link{survival}}, \code{\link{strata}}, and \code{\link{coxph}}
#' @import MSBVAR plyr reshape2 survival
#' @export
#' @references Licht, Amanda A. 2011. “Change Comes with Time: Substantive Interpretation of Nonproportional Hazards in Event History Analysis.” Political Analysis 19: 227–43.
#'
#' King, Gary, Michael Tomz, and Jason Wittenberg. 2000. “Making the Most of Statistical Analyses: Improving Interpretation and Presentation.” American Journal of Political Science 44(2): 347–61.

coxsimtvc <- function(obj, b, btvc, tfun = "linear", pow = NULL, nsim = 1000, from, to, by, ci = "95", strata = FALSE)
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
  
  tfunOpts <- c("linear", "log", "power")
  TestforTOpts <- tfun %in% tfunOpts
  if (TestforTOpts == FALSE){
    stop("Must specify tfun as 'linear', 'log', or 'power'")
  }
    
  if (tfun == "linear"){
    Range <- seq(from = from, to = to, by = by)
  } else if (tfun == "log"){
    Range <- log(seq(from = from, to = to, by = by))
  } else if (tfun == "power"){
    Range <- (seq(from = from, to = to, by = by))^pow
  }
  
  TVSim <- outer(Drawn[,2], Range)
  TVSim <- data.frame(melt(TVSim))
  TVSim <- rename(TVSim, c(Var1 = "ID", Var2 = "time", value = "TVC"))
  TVSim <- merge(Drawn, TVSim, by = "ID")

  TVSim$CombCoef <- TVSim[[2]] + TVSim$TVC
  TVSim$HR <- exp(TVSim$CombCoef)
  
  if (strata == TRUE){
    bfit <- basehaz(obj)
    TVSim <- merge(bfit, TVSim, by = "time")
    TVSim$HRate <- TVSim$hazard * TVSim$HR
  }

  TVSim <- TVSim[order(TVSim$time),]
  
  if (ci == "all"){
    TVSimPerc <- TVSim 
  } else if (ci == "95"){
    TVSimPerc <- ddply(TVSim, .(time), transform, Lower = HR < quantile(HR, c(0.025)))
    TVSimPerc <- ddply(TVSimPerc, .(time), transform, Upper = HR > quantile(HR, 0.975))
    TVSimPerc <- subset(TVSimPerc, Lower == FALSE & Upper == FALSE)
  } else if (ci == "90"){
    TVSimPerc <- ddply(TVSim, .(time), transform, Lower = HR < quantile(HR, c(0.05)))
    TVSimPerc <- ddply(TVSimPerc, .(time), transform, Upper = HR > quantile(HR, 0.95))
    TVSimPerc <- subset(TVSimPerc, Lower == FALSE & Upper == FALSE)
  } else if (ci == "99"){
    TVSimPerc <- ddply(TVSim, .(time), transform, Lower = HR < quantile(HR, c(0.005)))
    TVSimPerc <- ddply(TVSimPerc, .(time), transform, Upper = HR > quantile(HR, 0.995))
    TVSimPerc <- subset(TVSimPerc, Lower == FALSE & Upper == FALSE)
  }
  
  class(TVSimPerc) <- "simtvc"
  TVSimPerc
}