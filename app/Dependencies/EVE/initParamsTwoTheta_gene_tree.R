

# applied necessary changes to the two theta test functions for
# when different gene trees are used for each row (orthogroup) in the gene.data df 


initParamsTwoTheta_gene_tree <- function(gene.data, colSpecies, shiftSpecies)
{
  colSpeciesIndices <- split(seq_along(colSpecies), f = colSpecies)
  
  gene.data[2,] <- NA # ADDED THIS to prevent issues with mean calculation when only one row is in the df
  
  species.mean <- sapply(colSpeciesIndices, function(i){ rowMeans(gene.data[,i, drop=F]) })
  species.var <- sapply(colSpeciesIndices, function(i){ apply(gene.data[,i, drop=F],1,var) })
  
  
  ### ADDED THIS to overwrite original definition of shift species (which was incorrect)
  shiftSpecies <- tree$tip.label[is_theta2_edge]
  ########################

  
  nonShiftSpecies <- setdiff(colnames(species.mean),shiftSpecies)
  
  theta1 <- rowMeans(species.mean[ ,nonShiftSpecies, drop=F],na.rm=T)
  if( length(shiftSpecies) == 0 )
    theta2 = theta1
  else
    theta2 <- rowMeans(species.mean[ ,shiftSpecies, drop=F],na.rm=T)
  sigma2 <- apply(species.mean,1,var,na.rm=T)
  alpha <- .5
  beta <- rowMeans(species.var,na.rm=T) / sigma2
  
  return(cbind(theta1,theta2,sigma2,alpha,beta))
}

fitTwoTheta_gene_tree <- function( tree, gene.data, isTheta2edge, colSpecies = colnames(gene.data), 
                         extra.var = NULL, initPar = NULL,
                         lowerBound = c(theta1 = -Inf, theta2 = -Inf, sigma2 = 0, alpha = 0, beta = 1e-10),
                         upperBound = c(theta1 =  Inf, theta2 =  Inf, sigma2 =   Inf, alpha = Inf, beta = 1e10),
                         logTransPars = c("alpha","sigma2","beta"),
                         cores = 1, fork=F)
{
  # get shift species from shift edges
  shiftSpecies = tree$tip.label[tree$edge[isTheta2edge & tree$edge[,2] <= Ntip(tree),2]]
  
  if(is.null(initPar)){
    # estimate initial parameters
    initPar <- initParamsTwoTheta_gene_tree(gene.data, colSpecies, shiftSpecies = shiftSpecies)
  } else {
    # check that given initial parameters have correct format
    stopifnot(identical(colnames(initPar),c("theta1","theta2","sigma2","alpha","beta")))
    stopifnot(nrow(initPar)==nrow(gene.data))
  }
  paramNames <- colnames(initPar)
  
  
  # log transform initial parameters and bounds
  doTransPar <- paramNames %in% logTransPars # which params to log transform
  initPar[,doTransPar] <- log(initPar[,doTransPar])
  lowerBound[logTransPars] <- log(lowerBound[logTransPars])
  upperBound[logTransPars] <- log(upperBound[logTransPars])
  
  # match the column species with the phylogeny tip labels
  index.expand <- match(colSpecies, tree$tip.label)
  
  localEVEmodel <- prepEVEmodel(tree = tree,index.expand = index.expand,
                                thetaIdx = ifelse(isTheta2edge, match("theta2",paramNames),
                                                  match("theta1",paramNames)),
                                alphaIdx = rep(match("alpha",paramNames),Nedge(tree)),
                                sigma2Idx = rep(match("sigma2",paramNames),Nedge(tree)),
                                betaIdx = match("beta",paramNames))
  
  fitOneGene <- function(row){
    # Error handling to catch infinte optim or function values that arise when data with NaN paramters is optimized
    res <- tryCatch({
      stats::optim(par = initPar[row, ], method = "L-BFGS-B", 
                   lower = lowerBound[paramNames], upper = upperBound[paramNames],
                   gene.data.row = gene.data[row, ],
                   extra.var.row = if(is.null(extra.var)) NULL else extra.var[row, ],
                   fn = function(par, gene.data.row, extra.var.row){
                     # reverse log transform parameters
                     par[doTransPar] <- exp(par[doTransPar])
                     
                     mvnormParams <- localEVEmodel(par)
                     
                     # Add extra variance (if given)
                     if( !is.null(extra.var) )
                       diag(mvnormParams$sigma) <- diag(mvnormParams$sigma) +  extra.var.row
                     
                     # ignore species with NA in the expression matrix
                     notNA <- !is.na(gene.data.row)
                     return(-dmvnorm_nocheck(gene.data.row[notNA], sigma = mvnormParams$sigma[notNA,notNA], 
                                             mean=mvnormParams$mean[notNA]))
                   })
    }, error = function(e) {
      msg <- paste0(e$message,". Unable to fit model to gene.data row ", row)
      warning( msg, immediate. = T)
      # return NA and error message in same format as returned by optim()
      return( list(par = setNames(rep(NA,length(paramNames)),paramNames), 
                   value=NA, counts=NA, convergence=52, message=msg)
      )
    })
    # reverse log transform estimated parameters
    res$par[doTransPar] <- exp(res$par[doTransPar])
    return(res)
  }
  
  myFunc <- function(row) {localEVEmodel(par = initPar[row,])}
  
  
  if(cores==1){
    res <- lapply(X = 1:nrow(gene.data), FUN = fitOneGene)
  } else if(fork){
    res <- mclapply(mc.cores = cores,X = 1:nrow(gene.data), FUN = fitOneGene)
  }else{
    cl <- makeCluster(cores)
    # Export local environment to worker processes
    clusterExport(cl, varlist = c(ls(envir = environment()),"dmvnorm_nocheck"),envir = environment())
    # clusterExport(cl, varlist = c("initPar","gene.data","lowerBound","upperBound","doTransPar","localEVEmodel","dmvnorm_nocheck"),envir = environment())
    clusterEvalQ(cl, expr = library(ape))
    res <- parLapply(cl = cl,X = 1:nrow(gene.data), fun = fitOneGene)
    stopCluster(cl)
  }
  
  # Simplify the results
  list( par = t(sapply(res,function(x) x$par)), 
        ll = -sapply(res,function(x) x$value),
        iterations = setNames(sapply(res,function(x) x$counts[1]),NULL),
        convergence = sapply(res,function(x) x$convergence),
        message = sapply(res,function(x) x$message))
}


initParamsOneTheta_gene_tree <- function(gene.data, colSpecies)
{
  colSpeciesIndices <- split(seq_along(colSpecies), f = colSpecies)
  
  gene.data[2,] <- NA # ADDED THIS to prevent issues with mean calculation when only one row is in the df
  
  
  species.mean <- sapply(colSpeciesIndices, function(i){ rowMeans(gene.data[ ,i, drop=F]) })
  species.var <- sapply(colSpeciesIndices, function(i){ apply(gene.data[ ,i, drop=F],1,var) })
  
  theta <- rowMeans(species.mean,na.rm = T)
  sigma2 <- apply(species.mean,1,var,na.rm = T)
  alpha <- .5
  beta <- rowMeans(species.var,na.rm = T) / sigma2
  
  return(cbind(theta,sigma2,alpha,beta))
}


fitOneTheta_gene_tree <- function( tree, gene.data, colSpecies = colnames(gene.data), 
                         extra.var = NULL, initPar = NULL,
                         lowerBound = c(theta = -Inf, sigma2 = 0, alpha = 0, beta = 1e-10),
                         upperBound = c(theta =  Inf, sigma2 = Inf, alpha = Inf, beta = 1e10),
                         logTransPars = c("alpha","sigma2","beta"),
                         cores = 1, fork=F)
{
  #Calculate the per gene parameter matrix based on the gene data
  
  if(is.null(initPar)){
    # estimate initial parameters
    initPar <- initParamsOneTheta_gene_tree(gene.data, colSpecies)
  } else {
    # check that given initial parameters have correct format
    stopifnot(identical(colnames(initPar),c("theta","sigma2","alpha","beta")))
    stopifnot(nrow(initPar)==nrow(gene.data))
  }
  paramNames <- colnames(initPar)
  paramNames <- colnames(initPar)
  
  
  # log transform initial parameters and bounds
  doTransPar <- paramNames %in% logTransPars # which params to log transform
  initPar[,doTransPar] <- log(initPar[,doTransPar])
  lowerBound[logTransPars] <- log(lowerBound[logTransPars])
  upperBound[logTransPars] <- log(upperBound[logTransPars])
  
  # match the column species with the phylogeny tip labels
  index.expand <- match(colSpecies, tree$tip.label)
  
  localEVEmodel <- prepEVEmodel(tree = tree,index.expand = index.expand,
                                thetaIdx = rep(match("theta",paramNames),Nedge(tree)),
                                alphaIdx = rep(match("alpha",paramNames),Nedge(tree)),
                                sigma2Idx = rep(match("sigma2",paramNames),Nedge(tree)),
                                betaIdx = match("beta",paramNames))
  
  fitOneGene <- function(row){
    # Error handling to catch infinite optim or function values that arise when data with NaN paramters is optimized
    res <- tryCatch({
      stats::optim(par = initPar[row, ], method = "L-BFGS-B", 
                   lower = lowerBound[paramNames], upper = upperBound[paramNames],
                   gene.data.row = gene.data[row, ],
                   extra.var.row = if(is.null(extra.var)) NULL else extra.var[row, ],
                   fn = function(par, gene.data.row, extra.var.row){
                     # reverse log transform parameters
                     par[doTransPar] <- exp(par[doTransPar])
                     
                     mvnormParams <- localEVEmodel(par)
                     
                     # Add extra variance (if given)
                     if( !is.null(extra.var) )
                       diag(mvnormParams$sigma) <- diag(mvnormParams$sigma) +  extra.var.row
                     
                     # ignore species with NA in the expression matrix
                     notNA <- !is.na(gene.data.row)
                     return(-dmvnorm_nocheck(gene.data.row[notNA], sigma = mvnormParams$sigma[notNA,notNA], 
                                             mean=mvnormParams$mean[notNA]))
                   })
    }, error = function(e) {
      msg <- paste0(e$message,". Unable to fit model to gene.data row ", row)
      warning( msg, immediate. = T)
      # return NA and error message in same format as returned by optim()
      return( list(par = setNames(rep(NA,length(paramNames)),paramNames), 
                   value=NA, counts=NA, convergence=52, message=msg)
      )
    })
    # reverse log transform estimated parameters
    res$par[doTransPar] <- exp(res$par[doTransPar])
    return(res)
  }
  
  myFunc <- function(row) {localEVEmodel(par = initPar[row,])}
  
  
  if(cores==1){
    res <- lapply(X = 1:nrow(gene.data), FUN = fitOneGene)
  } else if(fork){
    res <- mclapply(mc.cores = cores,X = 1:nrow(gene.data), FUN = fitOneGene)
  }else{
    cl <- makeCluster(cores)
    # Export local environment to worker processes
    clusterExport(cl, varlist = c(ls(envir = environment()),"dmvnorm_nocheck"),envir = environment())
    # clusterExport(cl, varlist = c("initPar","gene.data","lowerBound","upperBound","doTransPar","localEVEmodel","dmvnorm_nocheck"),envir = environment())
    clusterEvalQ(cl, expr = library(ape))
    res <- parLapply(cl = cl,X = 1:nrow(gene.data), fun = fitOneGene)
    stopCluster(cl)
  }
  
  # Simplify the results
  list( par = t(sapply(res,function(x) x$par)), 
        ll = -sapply(res,function(x) x$value),
        iterations = setNames(sapply(res,function(x) x$counts[1]),NULL),
        convergence = sapply(res,function(x) x$convergence),
        message = sapply(res,function(x) x$message))
}

                      
twoThetaTest_gene_tree <- function(tree, gene.data, isTheta2edge, colSpecies = colnames(gene.data), ...){
  
  cat("Fit with single theta...\n") 
  oneThetaRes <- fitOneTheta_gene_tree(tree, gene.data, colSpecies = colSpecies)

  # use one-theta fit as initial parameters for two-theta model
  twoThetaInitPar <- oneThetaRes$par[,c(1,1,2,3,4)]
  
  if(is.null(nrow(twoThetaInitPar))) {twoThetaInitPar <- t(as.data.frame(twoThetaInitPar))} # ADDED THIS
  twoThetaInitPar <- as.data.frame(twoThetaInitPar)# ADDED THIS
  colnames(twoThetaInitPar) <- c("theta1","theta2", 'sigma2', 'alpha', 'beta') # ADDED THIS
  #colnames(twoThetaInitPar)[1:2]<-c("theta1","theta2") # REMOVED THIS 
  
  cat("Fit with two thetas...\n")
  twoThetaRes <- fitTwoTheta_gene_tree(tree, gene.data,isTheta2edge = isTheta2edge, colSpecies = colSpecies, 
                             initPar=twoThetaInitPar)
  
  LRT <- 2 * (twoThetaRes$ll - oneThetaRes$ll)
  
  return( list(oneThetaRes = oneThetaRes,
               twoThetaRes = twoThetaRes,
               LRT = LRT) )
}






