###################
#
# Demand Function
#
# "Linear" demand
#
###################


demandLinear <- function(intercept,
                         price,
                         slope,
                         eta,
                         n)
{
  demand <- c()
  for (i in 1:n) {
    demand[i] <- (intercept[i] + sum(slope[i,]*price))^eta
  }
  return(demand)
}