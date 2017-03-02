# Setup
library("MergerEffects")


# Clear workspace, set working directory
rm(list = ls())
setwd("[your path]")


# Load data
data1 <- read.csv("MarketData.csv")
data2 <- read.csv("OwnershipPre.csv")
data3 <- read.csv("OwnershipPost.csv")
data4 <- read.csv("ControlPre.csv")
data5 <- read.csv("ControlPost.csv")
data6 <- read.csv("ProportionsOfDiversion1.csv")


# Data (invariant)
revenues <- data1[,2]
prices <- data1[,3]
quantities <- revenues/prices
margins <- data1[,4]
costs <- costFun(margins,
                 prices)
ownershipMatrixPre <- data2[,-1]
ownershipMatrixPost <- data3[,-1]
controlMatrixPre <- data4[,-1]
controlMatrixPost <- data5[,-1]
proportionsDiversion <- data6[,-1]
rm(data1,data2,data3,data4,data5,data6,revenues)


# Parameter values
efficiency1 <- 0.2
efficiency2 <- 0.2
efficiency3 <- 0
marketElasticity <- -0.65
shape <- 1
eta <- 2


# Additional parameters (override default settings: 0/0/1e-6/2)
diagnosticInfo <- 1


# Sequential code
results <- mergerRun(eta=eta,
                     shape=shape,
                     q0=quantities,
                     p0=prices,
                     c0=costs,
                     efficiencies=c(efficiency1,efficiency2,efficiency3),
                     PoD=proportionsDiversion,
                     conPre=controlMatrixPre,
                     ownPre=ownershipMatrixPre,
                     conPost=controlMatrixPost,
                     ownPost=ownershipMatrixPost,
                     marketElasticity=marketElasticity,
                     diagnostics=diagnosticInfo)
print(results)
