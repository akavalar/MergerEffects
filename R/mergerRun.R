mergerRun <- function(eta,
                      shape,
                      q0,
                      p0,
                      c0,
                      efficiencies,
                      PoD,
                      PoDposition=NA,
                      conPre,
                      ownPre,
                      conPost,
                      ownPost,
                      postPosition=NA,
                      marketElasticity,
                      n=nrow(ownPre),
                      productNames=rownames(rep(1,nrow(ownPre)), do.NULL=FALSE, prefix="Product"),
                      companyNames=rownames(rep(1,ncol(ownPre)), do.NULL=FALSE, prefix="Company"),
                      diagnostics=0,
                      tol=1e-6,
                      retries=2)
{
  # calculate new costs
  c1 <- (1 - efficiencies)*c0
  
  # run simulation
  if (shape==1) {
    results <- MergerEffects::mergerLinear(eta=eta,
                                              q0=q0,
                                              p0=p0,
                                              c0=c0,
                                              c1=c1,
                                              PoD=PoD,
                                              conPre=conPre,
                                              ownPre=ownPre,
                                              conPost=conPost,
                                              ownPost=ownPost,
                                              marketElasticity=marketElasticity,
                                              n=n,
                                              productNames=productNames,
                                              companyNames=companyNames,
                                              diagnostics=diagnostics,
                                              tol=tol,
                                              retries=retries)
  } else if (shape==2) {
    results <- MergerEffects::mergerAIDS(q0=q0,
                                            p0=p0,
                                            c0=c0,
                                            c1=c1,
                                            PoD=PoD,
                                            conPre=conPre,
                                            ownPre=ownPre,
                                            conPost=conPost,
                                            ownPost=ownPost,
                                            marketElasticity=marketElasticity,
                                            n=n,
                                            productNames=productNames,
                                            companyNames=companyNames,
                                            diagnostics=diagnostics,
                                            tol=tol,
                                            retries=retries)
  } else if (shape==3) {
    results <- MergerEffects::mergerLinLog(q0=q0,
                                              p0=p0,
                                              c0=c0,
                                              c1=c1,
                                              PoD=PoD,
                                              conPre=conPre,
                                              ownPre=ownPre,
                                              conPost=conPost,
                                              ownPost=ownPost,
                                              marketElasticity=marketElasticity,
                                              n=n,
                                              productNames=productNames,
                                              companyNames=companyNames,
                                              diagnostics=diagnostics,
                                              tol=tol,
                                              retries=retries)
  }
  
  # clear existing column/label names
  #dimnames(PoD) <- list(NULL, NULL)
  #dimnames(conPost) <- list(NULL, NULL)
  #dimnames(ownPost) <- list(NULL, NULL)  
  
  # save column/row names
  names(efficiencies) <- productNames
  dimnames(PoD) <- list(productNames, productNames)
  dimnames(conPost) <- list(productNames, companyNames)
  dimnames(ownPost) <- list(productNames, companyNames)
  
  results$data.postPosition <- postPosition
  results$data.conPost <- conPost
  results$data.ownPost <- ownPost
  results$data.shape <- shape
  results$data.eta <- eta
  results$data.marketElasticity <- marketElasticity
  results$data.efficiencies <- efficiencies
  results$data.PoDposition <- PoDposition
  results$data.PoD <- PoD
  
  return(results)
}