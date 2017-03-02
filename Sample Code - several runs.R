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
data7 <- read.csv("ProportionsOfDiversion2.csv")


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
proportionsDiversion1 <- data6[,-1]
proportionsDiversion2 <- data7[,-1]
rm(data1,data2,data3,data4,data5,data6,data7,revenues)


# Data (changing)
proportionsDiversion <- list(proportionsDiversion1, proportionsDiversion2,
                             "quantity", "nests")


# Parameter values
gamma <- 2 # single values only
efficiency1 <- c(0, 0.02)
efficiency2 <- c(0, 0.02)
efficiency3 <- c(0, 0.02)
marketElasticity <- c(-0.5, -0.75, -1)
shape <- c(1, 2, 3)
eta <- c(1, 2, 3)


# Additional parameters (override default settings: 0/0/1e-6/2)
coresUsed <- 4
diagnosticInfo <- 0
maxTol <- 1e-6
maxRetries <- 5


# Parallelized code
start <- Sys.time()
resultsPar <- mergerSimulate(q0=quantities,
                             p0=prices,
                             c0=costs,
                             ownPre=ownershipMatrixPre,
                             ownPost=ownershipMatrixPost,
                             conPre=controlMatrixPre,
                             conPost=controlMatrixPost,
                             PoD=proportionsDiversion,
                             eta=eta,
                             shape=shape,
                             marketElasticity=marketElasticity,
                             gamma=gamma,
                             eff1=efficiency1,
                             eff2=efficiency2,
                             eff3=efficiency3,
                             cores=coresUsed,
                             diagnostics=diagnosticInfo,
                             tol=maxTol,
                             retries=maxRetries)
timePar <- Sys.time()-start
writeToFile(fileName="resultsPar.csv", dataframe=resultsPar)


# Sequential code
start <- Sys.time()
resultsSeq <- mergerSimulate(q0=quantities,
                             p0=prices,
                             c0=costs,
                             ownPre=ownershipMatrixPre,
                             ownPost=ownershipMatrixPost,
                             conPre=controlMatrixPre,
                             conPost=controlMatrixPost,
                             PoD=proportionsDiversion,
                             eta=eta,
                             shape=shape,
                             marketElasticity=marketElasticity,
                             gamma=gamma,
                             eff1=efficiency1,
                             eff2=efficiency2,
                             eff3=efficiency3,
                             cores=0,
                             diagnostics=diagnosticInfo,
                             tol=maxTol,
                             retries=maxRetries)
timeSeq <- Sys.time()-start
writeToFile(fileName="resultsSeq.csv", dataframe=resultsSeq)

print(timePar)
print(timeSeq)