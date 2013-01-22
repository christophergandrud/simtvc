##############
# Competing risks test
# Christopher Gandrud
# 22 January 2013
##############

# Load package
library(cmprsk)

# Example from cmprsk manual file
# simulated data to test
set.seed(10)
ftime <- rexp(200)
fstatus <- sample(0:2,200,replace =TRUE)
cov <- matrix(runif(600),nrow = 200)
dimnames(cov)[[2]] <- c("Var1", "Var2", "Var3")
# quadratic in time for first cov
Test <- crr(ftime,fstatus,cov,cbind(cov[,1],cov[,1]),function(Uft) cbind(Uft,Uft^2))

Test$coef
Test$var

Test2 <- crrsimtvct(Test, b = "Var1", btvc = "cbind(cov[, 1], cov[, 1])2*tf2", tfunc = "power", pow = 2, from = 1, to = 200, by = 5)

crrsimtvct <- function(obj, b, btvc, tfunc = "linear", pow = NULL, nsim = 1000, from, to, by, ci = "95", ...)
{
  Coef <- obj$coef
  VC <- obj$var
  
  Drawn <- rmultnorm(n = nsim, mu = Coef, vmat = VC)
  DrawnDF <- data.frame(Drawn)
  
  dfn <- names(DrawnDF)
  bpos <- match(b, dfn)
  btvcpos <- match(btvc, dfn)
  
  Drawn <- data.frame(Drawn[, c(bpos, btvcpos)])
  Drawn$ID <- 1:nsim
  
  if (tfunc == "linear"){
    Range <- seq(from = from, to = to, by = by)
  } else if (tfunc == "log"){
    Range <- log(seq(from = from, to = to, by = by))
  } else if (tfunc == "power"){
    Range <- (seq(from = from, to = to, by = by))^pow
  }
  
  TVSim <- outer(Drawn[,2], Range)
  TVSim <- data.frame(melt(TVSim))
  TVSim <- rename(TVSim, c(X1 = "ID", X2 = "Time", value = "TVC"))
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
