###################
#
# Merger Simulation
#
# "Linear" demand
#
###################


mergerLinear <- function(eta,
                         q0,
                         p0,
                         c0,
                         c1,
                         PoD,
                         conPre,
                         ownPre,
                         conPost,
                         ownPost,
                         marketElasticity,
                         n,
                         productNames,
                         companyNames,
                         diagnostics,
                         tol,
                         retries)
{
  # if no diagnostic info requested
  if (diagnostics==0) {
    # find slope parameters of demand functions
    slopeAndR <- MergerEffects::slopeLinear(eta=eta,
                                               q0=q0,
                                               p0=p0,
                                               c0=c0,
                                               PoD=PoD,
                                               conPre=conPre,
                                               ownPre=ownPre,
                                               marketElasticity=marketElasticity,
                                               n=n,
                                               diagnostics=diagnostics,
                                               tol=tol,
                                               retries=retries)
    
    # if unique solution not found, return description of the problem and stop
    if (slopeAndR[1]=="FAILED") {
      return(list(status="FAILED: slope",
                  error=unlist(slopeAndR[2]),
                  slope=NA,
                  R=NA,
                  intercept=NA,
                  p1=NA,
                  q1=NA,
                  Findex=NA,
                  MEindex=NA,
                  Tindex=NA))
    } else {
      slope <- slopeAndR$slope
      R <- slopeAndR$R
    }
    
    # find intercepts of demand functions
    intercept <- MergerEffects::interceptLinear(slope=slope,
                                                   p0=p0,
                                                   q0=q0,
                                                   eta=eta,
                                                   R=R,
                                                   PoD=PoD,
                                                   c0=c0,
                                                   ownPre=ownPre,
                                                   n=n,
                                                   diagnostics=diagnostics,
                                                   tol=tol,
                                                   retries=retries)
    
    # if unique solution not found, return description of the problem and stop
    if (intercept[1]=="FAILED") {
      return(list(status="FAILED: intercept",
                  error=unlist(intercept[2]),
                  slope=slope,
                  R=R,
                  intercept=NA,
                  p1=NA,
                  q1=NA,
                  Findex=NA,
                  MEindex=NA,
                  Tindex=NA))
    }
    
    # find new prices under the new ownership & management structure
    p1 <- MergerEffects::simulateLinear(eta=eta,
                                           slope=slope,
                                           p0=p0,
                                           c1=c1,
                                           intercept=intercept,
                                           conPost=conPost,
                                           ownPost=ownPost,
                                           n=n,
                                           diagnostics=diagnostics,
                                           tol=tol,
                                           retries=retries)
    
    # if unique solution not found, return description of the problem and stop
    if (p1[1]=="FAILED") {
      return(list(status="FAILED: new price",
                  error=unlist(p1[2]),
                  slope=slope,
                  R=R,
                  intercept=intercept,
                  p1=NA,
                  q1=NA,
                  Findex=NA,
                  MEindex=NA,
                  Tindex=NA))
    }
    
    # find new quantities (given new prices)
    q1 <- MergerEffects::demandLinear(intercept=intercept,
                                         price=p1,
                                         slope=slope,
                                         eta=eta,
                                         n=n)
    
    # obtain values of price indices
    indices <- MergerEffects::indices(p1=p1,
                                         p0=p0,
                                         q1=q1,
                                         q0=q0)
    
    # assign column and row names
    dimnames(slope) <- list(productNames,productNames)
    names(intercept) <- productNames
    names(p1) <- productNames
    names(q1) <- productNames

    # return results
    return(list(status="OK",
                error="",
                slope=slope,
                R=R,
                intercept=intercept,
                p1=p1,
                q1=q1,
                Findex=indices$Findex,
                MEindex=indices$MEindex,
                Tindex=indices$Tindex))
    
  } else if (diagnostics==1) {  
    # find slope parameters of demand functions
    slopeAndR <- MergerEffects::slopeLinear(eta=eta,
                                               q0=q0,
                                               p0=p0,
                                               c0=c0,
                                               PoD=PoD,
                                               conPre=conPre,
                                               ownPre=ownPre,
                                               marketElasticity=marketElasticity,
                                               n=n,
                                               diagnostics=diagnostics,
                                               tol=tol,
                                               retries=retries)
    
    # if unique solution not found, return description of the problem and stop
    if (slopeAndR[1]=="FAILED") {
      return(list(status="FAILED: slope",
                  error=unlist(slopeAndR[2]),
                  slope=NA,
                  R=NA,
                  intercept=NA,
                  p1=NA,
                  q1=NA,
                  Findex=NA,
                  MEindex=NA,
                  Tindex=NA,
                  checkElasticity=NA,
                  checkOwnslope=NA,
                  checkFOCsSlope=NA,
                  checkDivRatios=NA,
                  checkR=NA,
                  checkDemand=NA,
                  checkFOCsProfit=NA,
                  checkSOCsProfit=NA,
                  checkFOCsNew=NA,
                  checkSOCsNew=NA))
    } else {
      slope <- slopeAndR$slope
      R <- slopeAndR$R
    }
    
    # find intercepts of demand functions
    interceptList <- MergerEffects::interceptLinear(slope=slope,
                                                       p0=p0,
                                                       q0=q0,
                                                       eta=eta,
                                                       R=R,
                                                       PoD=PoD,
                                                       c0=c0,
                                                       ownPre=ownPre,
                                                       n=n,
                                                       diagnostics=diagnostics,
                                                       tol=tol,
                                                       retries=retries)
    
    # if unique solution not found, return description of the problem and stop
    if (interceptList[1]=="FAILED") {
      return(list(status="FAILED: intercept",
                  error=unlist(interceptList[2]),
                  slope=slope,
                  R=R,
                  intercept=NA,
                  p1=NA,
                  q1=NA,
                  Findex=NA,
                  MEindex=NA,
                  Tindex=NA,
                  checkElasticity=slopeAndR$checkElasticity,
                  checkOwnslope=slopeAndR$checkOwnslope,
                  checkFOCsSlope=slopeAndR$checkFOCsSlope,
                  checkDivRatios=NA,
                  checkR=NA,
                  checkDemand=NA,
                  checkFOCsProfit=NA,
                  checkSOCsProfit=NA,
                  checkFOCsNew=NA,
                  checkSOCsNew=NA))
    } else {
      intercept <- interceptList$intercept
    }
    
    # find new prices under the new ownership & management structure
    p1List <- MergerEffects::simulateLinear(eta=eta,
                                               slope=slope,
                                               p0=p0,
                                               c1=c1,
                                               intercept=intercept,
                                               conPost=conPost,
                                               ownPost=ownPost,
                                               n=n,
                                               diagnostics=diagnostics,
                                               tol=tol,
                                               retries=retries)
    
    # if unique solution not found, return description of the problem and stop
    if (p1List[1]=="FAILED") {
      return(list(status="FAILED: new price",
                  error=unlist(p1List[2]),
                  slope=slope,
                  R=R,
                  intercept=intercept,
                  p1=NA,
                  q1=NA,
                  Findex=NA,
                  MEindex=NA,
                  Tindex=NA,
                  checkElasticity=slopeAndR$checkElasticity,
                  checkOwnslope=slopeAndR$checkOwnslope,
                  checkFOCsSlope=slopeAndR$checkFOCsSlope,
                  checkDivRatios=interceptList$checkDivRatios,
                  checkR=interceptList$checkR,
                  checkDemand=interceptList$checkDemand,
                  checkFOCsProfit=interceptList$checkFOCsProfit,
                  checkSOCsProfit=interceptList$checkSOCsProfit,
                  checkFOCsNew=NA,
                  checkSOCsNew=NA))
    } else {
      p1 <- p1List$p1
    }

    # find new quantities (given new prices)
    q1 <- MergerEffects::demandLinear(intercept=intercept,
                                         price=p1,
                                         slope=slope,
                                         eta=eta,
                                         n=n)
    
    # obtain values of price indices
    indices <- MergerEffects::indices(p1=p1,
                                         p0=p0,
                                         q1=q1,
                                         q0=q0)
    
    # assign column and row names
    varlist1d <- c("intercept","p1","q1","slopeAndR$checkOwnslope","slopeAndR$checkFOCsSlope",
                   "interceptList$checkR","interceptList$checkDemand","interceptList$checkFOCsProfit",
                   "interceptList$checkSOCsProfit","p1List$checkFOCsNew","p1List$checkSOCsNew")
    varlist2d <- c("slope", "interceptList$checkDivRatios")
    for (j in varlist1d) {
      eval(parse(text=paste("names(",j,") <- productNames")))
    }
    for (j in varlist2d) { #here, used slope as p0 is not a matrix
      eval(parse(text=paste("dimnames(",j,") <- list(productNames,productNames)")))
    }

    # return results
    return(list(status="OK",
                error="",
                slope=slope,
                R=R,
                intercept=intercept,
                p1=p1,
                q1=q1,
                Findex=indices$Findex,
                MEindex=indices$MEindex,
                Tindex=indices$Tindex,
                checkElasticity=slopeAndR$checkElasticity,
                checkOwnslope=slopeAndR$checkOwnslope,
                checkFOCsSlope=slopeAndR$checkFOCsSlope,
                checkDivRatios=interceptList$checkDivRatios,
                checkR=interceptList$checkR,
                checkDemand=interceptList$checkDemand,
                checkFOCsProfit=interceptList$checkFOCsProfit,
                checkSOCsProfit=interceptList$checkSOCsProfit,
                checkFOCsNew=p1List$checkFOCsNew,
                checkSOCsNew=p1List$checkSOCsNew))
  }
}