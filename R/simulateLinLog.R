###################
#
# Post-Merger Simulation of New Prices
#
# LinLog demand
#
###################


simulateLinLog <- function(slope,
                           p0,
                           c1,
                           intercept,
                           conPost,
                           ownPost,
                           n,
                           diagnostics,
                           tol,
                           retries)
{
  # analytical derivatives of the new profit functions (1 FOC per product)
  PostFOCsFun <- function(price) {
    priceEffect <- MergerEffects::demandLinLog(intercept=intercept,
                                                  price=price,
                                                  slope=slope,
                                                  n=n) # derivative of "(price-cost) part of profit function" wrt price
    
    # demandEffect = n-by-n matrix, rows correspond to quantities (e.g. row 1 = partial dee's of q_1 wrt all n prices)
    demandEffect <- matrix(ncol=n, nrow=n) # derivative of "quantity part of profit function" wrt price
    for (i in 1:n) {
      for (j in 1:n) {
        demandEffect[i,j] <- (price[i]-c1[i])*slope[i,j]/price[j] # effect of p_j on q_i for all i,j
      }
    }
    
    cumDemandEffect <- c()
    for (i in 1:n) {
      owner <- which(conPost[i,]==1) # identify the manager authorized to set the price of that product
      priceEffect[i] <- priceEffect[i]*ownPost[i,owner] # profit/ownership shares also affect the contributions of price effects
      cumDemandEffect[i] <- sum(demandEffect[,i]*ownPost[,owner]) # add up demand effects (relative to profit shares), account for 1+ products owned
    }
    
    totalEffect <- priceEffect + cumDemandEffect
    return(totalEffect)
  }
  
  # post-merger prices
  for (i in 1:retries) {
    p1 <- nleqslv::nleqslv((p0+abs(rnorm(length(p0),0,1))),PostFOCsFun) # add random noise to our starting point (p0)
    
    # if unique solution found
    if (p1$termcd == 1) {
      print("Post-merger prices: Convergence achieved.")
      p1 <- p1$x
      break
    } else if (i == retries) { # unique solution not found after max retries
      print("Post-merger prices: Did not converge, check output and try again. Aborting iteration.")
      return(list("FAILED",p1$message))
    }
  }
  
  if (diagnostics==0) {
    return(p1=p1)
    
  } else if (diagnostics==1) {
    return(list(p1=p1,
                checkFOCsNew=(abs(PostFOCsFun(p1)) < tol),
                checkSOCsNew=(diag(numDeriv::jacobian(PostFOCsFun,p1)) < 0))) # Jacobian of FOC = Hessian of profit function
  }
}
