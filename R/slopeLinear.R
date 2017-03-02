###################
#
# Demand Calibration: slope parameters
#
# "Linear" demand
#
###################


slopeLinear <- function(eta,
                        q0,
                        p0,
                        c0,
                        PoD,
                        conPre,
                        ownPre,
                        marketElasticity,
                        n,
                        diagnostics,
                        tol,
                        retries)
{
  # vector of own-price effects
  ownpriceFun <- function(ownslope) {
    ownprice <- c() # initialize vector
    for (i in 1:n) {
      ownprice[i] <- eta*q0[i]^((eta-1)/eta)*ownslope[i]
    }
    return(ownprice)
  }
  
  # vector of FOCs (functions of own-slope parameters) and the market elasticity equation (function of R)
  LHSofFOCandRetentionFun <- function(ownslopeR,length) {
    R <- ownslopeR[length]
    ownslope <- ownslopeR[1:(length-1)]
    ownprice <- ownpriceFun(ownslope)
    
    # FOCs (number of FOCs = number of products)
    # demandEffect = n-by-n matrix, rows correspond to quantities (e.g. row 1 = partial dee's of q_1 wrt all n prices)
    demandEffect <- matrix(ncol=n, nrow=n) # derivative of "quantity part of profit function" wrt price (1st part of derivative)
    for (i in 1:n) {
      for (j in 1:n) {
        demandEffect[i,j] <- ifelse(i==j, ownprice[i]*(p0[i]-c0[i]), -R*ownprice[j]*(p0[i]-c0[i])*PoD[j,i]) # effect of p_j on q_i for all i,j
      }
    }
    
    priceEffect <- c()
    cumDemandEffect <- c()
    for (i in 1:n) {
      owner <- which(conPre[i,]==1) # identify the manager authorized to set the price of that product
      priceEffect[i] <- q0[i]*ownPre[i,owner] # profit/ownership shares also affect the contributions of price effects (2nd part of derivative)
      cumDemandEffect[i] <- sum(demandEffect[,i]*ownPre[,owner]) # add up demand effects (relative to profit shares), account for 1+ products owned
    }
    totalEffect <- priceEffect + cumDemandEffect
    
    # market elasticity equation (1)
    ElessRHS <- marketElasticity - (1-R)*(1/sum(q0))*sum(ownprice*p0)
    
    # combine equations
    LHSofFOCandR <- append(totalEffect, ElessRHS)
    
    return(LHSofFOCandR)
  }
  
  # solve for the own-slope paramaters and R using nleqslv's default settings and random initial guess
  for (i in 1:retries) {
    ownslopeR <- append(-abs(rnorm(n, 0, 1)), runif (1)) # n elements ~ N(0,1) [own-slope parameters] + 1 element ~ U(0,1) [R]
    ownslopeR <- nleqslv::nleqslv(ownslopeR,LHSofFOCandRetentionFun,length=length(ownslopeR)) # solnB + R ("totalElasticity")
    
    # if unique solution found
    if (ownslopeR$termcd == 1) {
      print("Slope parameters: Convergence achieved.")
      ownslope <- ownslopeR$x[1:(length(ownslopeR$x)-1)] # solnB
      R <- ownslopeR$x[length(ownslopeR$x)] # R ("totalElasticity")
      break
    } else if (i == retries) { # unique solution not found after max retries
      print("Slope parameters: Did not converge, check output and try again. Aborting iteration.")
      return(list("FAILED",ownslopeR$message))
    }
  }
  
  # calculate own-price effects given the resulting own-slope parameters
  ownprice <- ownpriceFun(ownslope)
  
  # construct slope matrix
  slope <- matrix(ncol=n, nrow=n) #much faster than data.frame() when using numDeriv!
  for (i in 1:n) {
    for (j in 1:n) {
      slope[i,j] <- ifelse(i==j, ownslope[i], -R*(ownslope[i]/ownprice[i]*ownprice[j])*PoD[j,i])
    }
  }
  
  if (diagnostics==0) {
    return(list(slope=slope,
                R=R))
  } else if (diagnostics==1) {
    return(list(slope=slope,
                R=R,
                checkElasticity=(1-R)*(1/sum(q0))*sum(ownprice*p0),
                checkOwnslope=(ownslope < 0),
                checkFOCsSlope=(abs(LHSofFOCandRetentionFun(ownslopeR$x,length(ownslopeR$x))) < tol)[1:(length(ownslopeR$x)-1)]))
  }
}
