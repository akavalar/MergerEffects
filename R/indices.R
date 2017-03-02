###################
#
# Indices
#
###################

indices <- function(p1,
                    p0,
                    q1,
                    q0)
{
  # Laspeyres index
  Lindex <- sum(p1*q0)/sum(p0*q0)
  
  # Paasche index
  Pindex <- sum(p1*q1)/sum(p0*q1)
  
  # Fisher index
  Findex <- 100*(Lindex*Pindex)^(1/2)
  
  # Marshall-Edgeworth index
  MEindex <- 100*sum(p1*(q0 + q1))/sum(p0*(q0 + q1))
  
  # Tornqvist index
  share1 <- p1*q1/sum(p1*q1)
  share0 <- p0*q0/sum(p0*q0)
  Tindex <- 100*prod((p1/p0)^((share1+share0)/2))
  
  return(list(Findex=Findex,
              MEindex=MEindex,
              Tindex=Tindex))  
}