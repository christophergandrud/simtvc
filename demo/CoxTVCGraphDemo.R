################
# Cox TVC demo
# Christopher Gandrud
# 25 January 2013
# Based on Licth 2011
################

# Load survival package
library(survival)

# Load Golub & Steunenberg (2007) Data
## Originally downloaded from http://hdl.handle.net/1902.1/15633
GS <- read.table("demo/GolubEUPdata.tab", sep = "\t", header = TRUE)

# Create backlogstrata
GS$backlogstrata <- 0
GS$backlogstrata[GS$backlog > 191] <- 1
GS$backlogstrata[GS$backlog > 202] <- 2

# Create natural log time interactions
GS$Lqmv <- tvc(GS, b = "qmv", tvar = "end", tfun = "log")

#### Run Cox PH Model ####
M1 <- coxph(Surv(begin, end, event) ~ 
              qmv + Lqmv,
            data = GS,
            ties = "efron")

#### Create simtvc object
Test3 <- coxsimtvc(obj = M1, b = "qmv", btvc = "Lqmv", tfun = "log", from = 80, to = 2000, by = 10, ci = "99", strata = FALSE)

#### Graph simulated combined hazard ratios ####
ggtvc(Test3)