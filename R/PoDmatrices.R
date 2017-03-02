# create "quantity" POD matrix
quantityPODmatrix <- function(q0,
                              n)
{
  s0 <- q0/sum(q0)
  quantityPoD <- matrix(ncol=n, nrow=n) #faster than data.frame()
  for (i in 1:n) {
    for (j in 1:n) {
      quantityPoD[i,j] <- ifelse(i==j, 0, s0[j]/(1-s0[i]))
    }
  }
  return(quantityPoD)
}



# create "nests" PoD matrix
nestsPODmatrix <- function(q0,
                           gamma,
                           n)
{
  s0 <- q0/sum(q0)
  
  SFDfun <- function(beta) {
    SFD <- matrix(ncol=n, nrow=n)
    for (i in 1:n) {
      for (j in 2:n) {
        SFD[i,j] <- ifelse(i==j, s0[i], s0[j]*beta[i])
      }
    }
    SFD[,1] <- s0[1]*c(1, rep(gamma,n-1))
    return(SFD)
  }
  
  betaFun <- function(beta) {
    SFD <- SFDfun(beta)
    conditions <- rowSums(SFD) - rep(1,n)
    return(conditions)
  }
  
  beta <- rnorm(n, 0, 1)
  beta <- nleqslv::nleqslv(beta,betaFun)
  beta <- beta$x # simple system of linear eqs, no problems re: convergence
  SFD <- SFDfun(beta)
  
  nestsPoD <- matrix(ncol=n, nrow=n)
  for (i in 1:n) {
    for (j in 1:n) {
      nestsPoD[i,j] <- ifelse(i==j, 0, SFD[i,j]/(1-s0[i]))
    }
  }
  return(nestsPoD)
}