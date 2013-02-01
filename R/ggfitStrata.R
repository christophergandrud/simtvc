ggfitStrata <- function(obj, byStrata = FALSE, xlab = "", ylab = "", lcolour = "#2C7FB8", rcolour = "#2C7FB8")
{
  require(ggplot2)
  require(gridExtra)
  
  lcolour <- lcolour
  rcolour <- rcolour

  sFit <- obj
  time <- sFit$time
  lower <- sFit$lower
  upper <- sFit$upper
  S <- sFit$surv
  strata <- sFit$strata
  strata <- factor(rep(names(strata), strata), levels = names(strata))
  TempData <- data.frame(Time = time, Lower = lower, 
                         Upper = upper, Survival = S, 
                         Strata = strata)   
  
  if (byStrata == FALSE){
    ggplot(data = TempData, aes(x = Time, 
                         y = Survival,
                         color = Strata,
                         fill = Strata)) +
      geom_line() +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = I(0.1)) +
      xlab(xlab) + ylab(ylab) +
      theme_bw()
      
  } else if (byStrata == TRUE){
    TempData$StrataC <- gsub("=", "", TempData$Strata)
    TempData$StrataC <- gsub(" ", "", TempData$StrataC)
    eachStrata <- unique(TempData$StrataC)
    
    plots <- lapply(eachStrata, function(i, lcolour = lcolour, rcolour = rcolour) { 
      SubData <- subset(TempData, StrataC == i)
      assign(paste0("Plot." i), ggplot(data = SubData, aes(x = Time, 
                                                            y = Survival)) +
                                   geom_line(colour = lcolour) +
                                   geom_ribbon(aes(ymin = Lower, 
                                                ymax = Upper), 
                                                alpha = I(0.1),
                                                colour = NA,
                                                fill = rcolour) +
                                   xlab(xlab) + ylab(ylab) +
                                   ggtitle(paste(i, "\n")) +
                                   theme_bw()
                                   )   
    })
    plots
  #do.call(grid.arrange, plots)
  }
}