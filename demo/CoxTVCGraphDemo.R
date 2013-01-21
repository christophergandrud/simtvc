################
# Cox TVC demo
# Christopher Gandrud
# 20 January 2013
# Based on Licth 2011
################

# Load survival package
library(survival)

# Load Golub & Steunenberg (2007) Data
## Originally downloaded from http://hdl.handle.net/1902.1/15633
GS <- read.table("demo/GolubEUPdata.tab", sep = "\t", header = TRUE)

# Create log time variable
GS$LT <- log(GS$end)

# Create natural log time interactions
attach(GS)
GS$Lqmv <- LT * qmv
GS$Lqmvpostsea <- LT * qmvpostsea
GS$Lcoop <- LT * coop
GS$Lcodec <- LT * codec
GS$Lthatcher <- LT * thatcher
GS$Lbacklog <- LT * backlog
detach(GS)

#### Run Cox PH Model ####
# Note, this model does not exactly match Licht (2011), but is pretty close
M1 <- coxph(Surv(begin, end, event) ~ 
              qmv + qmvpostsea + qmvpostteu + 
              coop + codec + eu9 + eu10 + eu12 +
              eu15 + thatcher + agenda + backlog +
              Lqmv + Lqmvpostsea + Lcoop + Lcodec +
              Lthatcher + Lbacklog, 
            data = GS,
            ties = "efron")

#### Create simtvc object
Test <- coxsimtvc(obj = M1, b = "qmv", btvc = "Lqmv", tfunc = "linear", from = 80, to = 2000, by = 15, ci = "99")

#### Graph simulated combined hazard ratios ####
ggtvc(Test)