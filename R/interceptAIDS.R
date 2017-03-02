###################
#
# Demand Calibration: intercepts
#
# AIDS
#
###################


interceptAIDS <- function(slope,
                          p0,
                          q0,
                          R,
                          PoD,
                          c0,
                          ownPre,
                          n,
                          diagnostics,
                          tol,
                          retries)
{
  # equilibrium condition: equate actual quantities and calculated quantities
  demandEqnsFun <- function(intercept) {
    demandEqns <- demandAIDS(intercept=intercept,
                             price=p0,
                             slope=slope,
                             n=n) - q0
    return(demandEqns)
  }
  
  # solve for the intercepts that guarantee that eq condition is satisfied
  for (i in 1:retries) {
    intercept <- rnorm(n, 0, 1) # n elements ~ N(0,1)
    intercept <- nleqslv::nleqslv(intercept,demandEqnsFun)
    
    # if unique solution found
    if (intercept$termcd == 1) {
      print("Intercepts: Convergence achieved.")
      intercept <- intercept$x
      break
    } else if (i == retries) { # unique solution not found after max retries
      print("Intercepts: Did not converge, check output and try again. Aborting iteration.")
      return(list("FAILED",intercept$message))
    }
  }
  
  if (diagnostics==0) {
    return(intercept=intercept)
    
  } else if (diagnostics==1) {
    # check diversion ratio matrix and R
    divJacobian <- numDeriv::jacobian(MergerEffects::demandAIDS,p0,intercept=intercept,
                                      slope=slope,n=n) # matrix of 1st numerical derivatives wrt p0 given intercept
    divRatios <- matrix(ncol=n, nrow=n)
    for (i in 1:n) {
      for (j in 1:n) {
        divRatios[i,j] <- ifelse(i==j,0,-100*divJacobian[j,i]/divJacobian[i,i])
      }
    }
    checkDivRatios <- (abs(divRatios - R*100*PoD)<tol) # diversion ratio matrix consistent with the PoD matrix
    checkR <- (abs(rowSums(divRatios) - rep(R*100,n))<tol) # each row of diversion matrix adds up to R
    
    # check if calculated demand == actual demand
    checkDemand <- (abs(q0 - MergerEffects::demandAIDS(intercept=intercept,
                                                          price=p0,
                                                          slope=slope,
                                                          n=n)) < tol)
    
    # helper profit function
    profitAIDS <- function(price) {
      demand <- MergerEffects::demandAIDS(intercept=intercept,
                                             price=price,
                                             slope=slope,
                                             n=n)
      profitProduct <- (price - c0)*demand # profit for each product i
      profitCompany <- colSums(profitProduct*ownPre) # profit of each company
      return(profitCompany)
    }
    
    # numerically calculate diagonal elements of Jacobian (i.e. own-price derivatives of profit function)
    FOCsPreFun <- function(price) {
      FOCs <- diag(numDeriv::jacobian(profitAIDS,price))
      return(FOCs)
    }
    
    # check that companies' FOCs==0 at optimal price p0
    checkFOCsProfit <- (abs(FOCsPreFun(p0)) < tol)
    
    # check that optima==maxima
    checkSOCsProfit <- (diag(numDeriv::jacobian(FOCsPreFun,p0)) < 0)
    
    return(list(intercept=intercept,
                checkDivRatios=checkDivRatios,
                checkR=checkR,
                checkDemand=checkDemand,
                checkFOCsProfit=checkFOCsProfit,
                checkSOCsProfit=checkSOCsProfit))
  }
}