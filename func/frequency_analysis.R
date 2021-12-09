#########################################################################
# R-script for flood frequency analysis (general function call)         #
#	based on:                                                             #
#  http://www.headwateranalytics.com/blog/flood-frequency-analysis-in-r #
#	2016-12-31                                                            #
#########################################################################

FrequencyAnalysis <- function(series, distribution, nep = nonexceeds()) {
  
  distribution <- tolower(distribution) # dist=c("gev", "wei", "gum", "ln3", "lp3")
  transformed <- FALSE
  
  # add log Pearson Type 3 to list of distributions supported
  # by lmomco package
  base.dist <- c('lp3', dist.list())
  
  if( any(distribution %in% base.dist) ) {
    
    # log transform series 
    if( distribution == 'lp3' ) {
      series <- log10(series)
      transformed <- TRUE
      distribution <- 'pe3'
    }
    
    # compute L-moments
    samLmom <- lmom.ub(series)
    
    # estimate distribution parameters
    distPar <- lmom2par(samLmom, type = distribution)
    if (is.null(distPar)) return(NULL)
      
    # compute quantiles for nonexceedances
    quant <- par2qua(f = nep, para = distPar)
    
    if( distribution == 'pe3' & transformed ) {
      distribution <- 'lp3'
      quant <- 10^quant
    }
    
    # return result as list object
    return(
      list(
        distribution = list(
          name = distribution,
          logTransformed = transformed,
          parameters = distPar),
        output = data.frame(nep = nep, rp = prob2T(nep), estimate = quant)
      ) )
    
  } else {
    stop(
      sprintf('Distribution \'%s\' not recognized!', distribution))
  }
}


#########################################################################

BootstrapCI <- function(series, distribution, n.resamples=1E3, nep=nonexceeds(), ci=0.90) {

  # compute frequency analysis
  fa <- FrequencyAnalysis(series=series, distribution=distribution, nep=nep)
  if (is.null(fa)) return(fa)
  
  # extract fitted model parameters and flag as to whether the 
  # distribution is based on log transformed data
  base.params <- fa$distribution$parameters
  isTransformed <- fa$distribution$logTransformed
  
  # create output matrices to store parameter sets and quantile estimates
  param.sets <- matrix(NA, nrow = n.resamples, ncol = length(base.params$para))
  quantile.estimates <- matrix(NA, nrow = n.resamples, ncol = length(nep), 
                               dimnames = list(NULL, nep) ) 
  
  # begin bootstrapping procedure
  for(i in 1:n.resamples) {
    
    valid.moments <- FALSE
    j <- 0
    
    # allow up to 20 re-tries to re-sample 
    while(!valid.moments & j < 20) {  
      
      # sample 'n' random variates from base distribution
      data <- rlmomco(n=length(series), base.params)
      
      # compute sample l-moments
      sample.moms = lmom.ub(data)
      
      valid.moments <- are.lmom.valid(sample.moms)
      j <- j + 1
    }
    
    # error handling
    if(!valid.moments) {
      stop("Bootstrapping failed to sample valid l-moments")
    } else {
      # estimate distribution parameters
      dist.par <- lmom2par(sample.moms, base.params$type)
      
      # store the distribution parameters
      param.sets[i,] <- dist.par$para
      
      # estimate quantiles at NEP
      estimated <- qlmomco(nep, dist.par)
      
      # convert quantile estimates to real values if
      # distribution was transformed
      if(isTransformed) estimated <- 10^estimated
      
      # store the quantiles at the desired AEP values
      quantile.estimates[i,] <- estimated
    } 
    
  }
  
  # now calculate confidence limits for quantiles
  p <- c((1-ci)/2, (1+ci)/2)
  ci <- sapply(colnames(quantile.estimates), 
               FUN=function(x){
                 quantile(quantile.estimates[,x], probs=p, na.rm=TRUE)})
  
  # now return list object containing output
  return(
    list(
      ci = data.frame(
        nonexceed_prob = nep,
        lower = as.vector(ci[1,]),
        true = fa$output$estimate,
        upper = as.vector(ci[2,]) ),
      parameters = param.sets,
      quantiles = quantile.estimates)
  )
  
}
