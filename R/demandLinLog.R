###################
#
# Demand Function
#
# LinLog demand
#
###################


demandLinLog <- function(intercept,
                         price,
                         slope,
                         n)
{
  demand <- c()
  for (i in 1:n) {
    demand[i] <- intercept[i] + sum(slope[i,]*log(price))
  }
  return(demand)
}