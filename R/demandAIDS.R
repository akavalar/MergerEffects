###################
#
# Demand Function
#
# AIDS
#
###################


demandAIDS <- function(intercept,
                       price,
                       slope,
                       n)
{
  demand <- c()
  for (i in 1:n) {
    demand[i] <- (intercept[i] + sum(slope[i,]*log(price)))/price[i]
  }
  return(demand)
}