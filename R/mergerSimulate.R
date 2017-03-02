mergerSimulate <- function(q0,
                           p0,
                           c0,
                           ownPre,
                           ownPost,
                           conPre,
                           conPost,
                           PoD,
                           eta,
                           shape,
                           marketElasticity,
                           gamma=1,
                           productNames=NULL,
                           companyNames=NULL,
                           cores=0,
                           diagnostics=0,
                           tol=1e-6,
                           retries=2,
                           ...)
{
  # setup  
  library("nleqslv") # non-linear equation solver
  library("numDeriv") # numerical derivatives (i.e. Jacobian matrix)
  
  # number of products (n) and companies (m)
  n <- length(q0) #nrow(ownPre)
  m <- ncol(ownPre)
  
  # check if market elasticies are all <= 0
  if (sum(marketElasticity > 0) > 0) {
    stop("Market elasticities cannot be positive. Aborting.")
  }
  
  # consistency checks (ownPost/conPost)
  if (class(ownPost)!=class(conPost)) {
    stop("ownPost and conPost objects are of different type. Aborting.")
  } else if (length(ownPost)!=length(conPost)) {
    stop("ownPost and conPost lists have an unequal number of elements. Aborting.")
  }
  if (length(ownPost)>1 && class(ownPost)=="list") {
    for (i in 1:(length(ownPost)-1)) {
      if (ncol(ownPost[[i]])!=ncol(ownPost[[i+1]])) {
        stop("Number of columns (companies) in ownPost should not vary. Aborting.")
      } else if (ncol(conPost[[i]])!=ncol(conPost[[i+1]])) {
        stop("Number of columns (companies) in conPost should not vary. Aborting.")
      }
    }
  }
  
  # convert objects to lists of matrices (expand.grip does not work with dfs/matrices)
  if (class(ownPost) != "list") {
    ownPost <- list(as.matrix(ownPost))
  } else {
    ownPost <- lapply(ownPost, as.matrix)
  }
  
  if (class(conPost) != "list") {
    conPost <- list(as.matrix(conPost))
  } else {
    conPost <- lapply(conPost, as.matrix)
  }
  
  if (class(PoD) != "list") {
    stop("PoD can contain heterogeneous objects and thus needs to be a list. Aborting.")
  } else {
    PoD <- lapply(PoD, as.matrix)
  }
  
  # parameters: create "quantity" PoD matrix
  if (is.element("quantity", PoD)) {
    PoD[[which(PoD=="quantity")]] <- MergerEffects::quantityPODmatrix(q0=q0,
                                                                         n=n)
  }
  
  # parameters: create "nests" PoD matrix
  if (is.element("nests", PoD)) {
    PoD[[which(PoD=="nests")]] <- MergerEffects::nestsPODmatrix(q0=q0,
                                                                   gamma=gamma,
                                                                   n=n)
  }
  
  # parameters - efficiencies: find efficiencies set by user
  efficienciesPartial <- list(...)
  names <- names(efficienciesPartial)
  if (length(efficienciesPartial)>0) {
    for (i in 1:length(efficienciesPartial)) {
      assign(names[i], efficienciesPartial[[i]])
    }
  }
  
  # parameters - efficiencies: set values of unknown efficiencies
  haves <- as.numeric(gsub("eff","",names))
  donthaves <- setdiff(seq(from=1, to=n, by=1), haves)
  for (i in 1:length(donthaves)) {
    assign(paste("eff", donthaves[i], sep=""),0)
  }
  
  effString <- "eff1=eff1"
  for (i in 2:n) {
    effString <- paste(effString,paste("eff",i,"=eff",i, sep=""),sep=",")
  }
  efficiencies <- eval(parse(text=paste("expand.grid(",effString,")",sep=""))) # df
  efficiencies <- split(efficiencies, seq(nrow(efficiencies))) # list of dfs
  
  # parameters: add PoDposition
  PoDlist <- Map(list,PoD,1:length(PoD))
  
  # parameters: link own and control lists
  ownconPostlist <- Map(list,ownPost,conPost)
  
  # parameters: add postPosition
  ownconPostlist <- Map(append,ownconPostlist,1:length(ownPost))
  
  # combine parameters into df (expand.grid works with scalars, vectors and lists only)
  parameters <- expand.grid(ownconPostlist=ownconPostlist,
                            PoDlist=PoDlist,
                            efficiencies=efficiencies,
                            eta=eta,
                            shape=shape,
                            marketElasticity=marketElasticity,
                            gamma=gamma)
  
  #convert eta to NA if shape!=1 (not linear), then remove duplicates
  parameters$eta[which(parameters$shape>1)] <- NA
  parameters <- parameters[!duplicated(parameters),]
  
  # check if supplied name vectors are correct, if not recreate them
  if (class(productNames) != "character" || class(companyNames) != "character" ||
        length(productNames) != n || length(companyNames) != m) {
    productNames <- rownames(rep(1,n), do.NULL=FALSE, prefix="Product")
    companyNames <- rownames(rep(1,m), do.NULL=FALSE, prefix="Company")
  }
  
  # data: turn ownPre and conPre into matrices (~10x speed-up!) 
  ownPre <- as.matrix(ownPre)
  conPre <- as.matrix(conPre)
  
  
  # run all simulations
  if (cores > 0) {
    library("foreach")
    library("doParallel")
    cluster <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cluster)
    
    results <- foreach::foreach(i=1:nrow(parameters),
                                .combine=rbind,
                                .packages=c("MergerEffects","nleqslv","numDeriv")) %dopar% {
                                  
                                  # run a single iteration
                                  MergerEffects::mergerRun(eta=parameters$eta[[i]],
                                                              shape=parameters$shape[[i]],
                                                              q0=q0,
                                                              p0=p0,
                                                              c0=c0,
                                                              efficiencies=as.numeric(parameters$efficiencies[[i]]),
                                                              PoD=as.matrix(parameters$PoDlist[[i]][[1]]),
                                                              PoDposition=parameters$PoDlist[[i]][[2]],
                                                              conPre=conPre,
                                                              ownPre=ownPre,
                                                              conPost=as.matrix(parameters$ownconPostlist[[i]][[1]]),
                                                              ownPost=as.matrix(parameters$ownconPostlist[[i]][[2]]),
                                                              postPosition=parameters$ownconPostlist[[i]][[3]],
                                                              marketElasticity=parameters$marketElasticity[[i]],
                                                              n=n,
                                                              productNames=productNames,
                                                              companyNames=companyNames,
                                                              diagnostics=diagnostics,
                                                              tol=tol,
                                                              retries=retries)
                                }
    parallel::stopCluster(cluster)
  }
  else {
    namesPar <- c("marketElasticity", "shape", "eta",
                  names(parameters$efficiencies[[1]]), "PoDposition", "postPosition")
    for (i in 1:nrow(parameters)) {
      # print-out for easier debugging
      cat("\nIteration #: ", i, " (out of ", nrow(parameters),")\n", sep = '')
      printout <- data.frame(t(c(parameters$marketElasticity[[i]], parameters$shape[[i]],
                                 parameters$eta[[i]], as.numeric(parameters$efficiencies[[i]]),
                                 parameters$PoDlist[[i]][[2]], parameters$ownconPostlist[[i]][[3]])),
                             row.names = i)
      names(printout) <- namesPar
      print(printout)
      
      # run a single iteration
      seqRun <- MergerEffects::mergerRun(eta=parameters$eta[[i]],
                                            shape=parameters$shape[[i]],
                                            q0=q0,
                                            p0=p0,
                                            c0=c0,
                                            efficiencies=as.numeric(parameters$efficiencies[[i]]),
                                            PoD=as.matrix(parameters$PoDlist[[i]][[1]]),
                                            PoDposition=parameters$PoDlist[[i]][[2]],
                                            conPre=conPre,
                                            ownPre=ownPre,
                                            conPost=as.matrix(parameters$ownconPostlist[[i]][[1]]),
                                            ownPost=as.matrix(parameters$ownconPostlist[[i]][[2]]),
                                            postPosition=parameters$ownconPostlist[[i]][[3]],
                                            marketElasticity=parameters$marketElasticity[[i]],
                                            n=n,
                                            productNames=productNames,
                                            companyNames=companyNames,
                                            diagnostics=diagnostics,
                                            tol=tol,
                                            retries=retries)
      if (i==1) {
        results <- seqRun        
      }
      else {
        results <- rbind(results, seqRun)
      }
    }
  }
  rownames(results) <- NULL
  results <- as.data.frame(results)
  
  print("Done!")
  
  return(results)
}