#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

closed.form.estimators <- function(distr,x){
  switch(distr,
         "pnorm" = {
           params <- c(mean(x,na.rm=TRUE),sd(x,na.rm=TRUE))
         },
         "pexp" = {
           params <- 1/mean(x,na.rm=TRUE)
         },
         "pgamma" = {
           # We do not use MLE for computational efficiency
           n <- length(x)
           mean.x <- mean(x,na.rm=TRUE)
           mean.logx <- mean(log(x),na.rm=TRUE)
           mean.xlogx <- mean(x*log(x),na.rm=TRUE)
           theta.init <-  mean.xlogx - mean.logx*mean.x
           k.init <- mean.x / theta.init
           theta.unbiased <- theta.init * n/(n-1)
           k.unbiased <- k.init - (3*k.init - 2/3*k.init/(1+k.init) - 4/5*k.init/(1+k.init)^2)/n
           params <- c(k.unbiased,1/theta.unbiased)
         },
         "plnorm" = {
           params <- c(mean(log(x),na.rm=TRUE),sd(log(x),na.rm=TRUE))
         },
         "punif" = {
           params <- range(x,na.rm=TRUE)
         })
}

ad.test.one <- function(x,distr,fit){
  switch(distr,
         "pnorm" = {
           res <- ad.test(x, "pnorm", mean = fit[1], sd = fit[2])
         },
         "pexp" = {
           res <- ad.test(x, "pexp", rate = fit[1])
         },
         "pgamma" = {
           res <- ad.test(x, "pgamma", shape = fit[1], rate = fit[2])
         },
         "plnorm" = {
           res <- ad.test(x, "plnorm", meanlog = fit[1], sdlog = fit[2])
         },
         "punif" = {
           res <- ad.test(setdiff(x,c(fit[1],fit[2])), "punif", min = fit[1], max = fit[2])
         })
  return(res)
}

ad.test.rep <- function(x,distr,fit,nrep=4999){
  n <- length(x)
  switch(distr,
         "pnorm" = {
           statistic.obs <- ad.test(x, "pnorm", mean = fit[1], sd = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rnorm(n, mean = fit[1], sd = fit[2])
             params.estimated <- closed.form.estimators("pnorm",simulated.sample)
             return(ad.test(simulated.sample, "pnorm", mean = params.estimated[1], sd = params.estimated[2])$statistic)
           })
         },
         "pexp" = {
           statistic.obs <- ad.test(x, "pexp", rate = fit[1])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rexp(n, rate = fit[1])
             params.estimated <- closed.form.estimators("pexp",simulated.sample)
             return(ad.test(simulated.sample, "pexp", rate = params.estimated[1])$statistic)
           })
         },
         "pgamma" = {
           statistic.obs <- ad.test(x, "pgamma", shape = fit[1], rate = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rgamma(n, shape = fit[1], rate = fit[2])
             params.estimated <- closed.form.estimators("pgamma",simulated.sample)
             return(ad.test(simulated.sample, "pgamma", shape = params.estimated[1], rate = params.estimated[2])$statistic)
           })
         },
         "plnorm" = {
           statistic.obs <- ad.test(x, "plnorm", meanlog = fit[1], sdlog = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rlnorm(n, meanlog = fit[1], sdlog = fit[2])
             params.estimated <- closed.form.estimators("plnorm",simulated.sample)
             return(ad.test(simulated.sample, "plnorm", meanlog = params.estimated[1], sdlog = params.estimated[2])$statistic)
           })
         },
         "punif" = {
           statistic.obs <- ad.test(setdiff(x,c(fit[1],fit[2])), "punif", min = fit[1], max = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- runif(n, min = fit[1], max = fit[2])
             params.estimated <- closed.form.estimators("punif",simulated.sample)
             return(ad.test(setdiff(simulated.sample,c(params.estimated[1],params.estimated[2])), "punif", min = params.estimated[1], max = params.estimated[2])$statistic)
           })
         })
  p.value <- (sum(statistic.sim > statistic.obs) + 1) / (nrep + 1)
  return(list(statistic=mean(statistic.sim), p.value = p.value))
}

cvm.test.one <- function(x,distr,fit){
  switch(distr,
         "pnorm" = {
           res <- cvm.test(x, "pnorm", mean = fit[1], sd = fit[2])
         },
         "pexp" = {
           res <- cvm.test(x, "pexp", rate = fit[1])
         },
         "pgamma" = {
           res <- cvm.test(x, "pgamma", shape = fit[1], rate = fit[2])
         },
         "plnorm" = {
           res <- cvm.test(x, "plnorm", meanlog = fit[1], sdlog = fit[2])
         },
         "punif" = {
           res <- cvm.test(x, "punif", min = fit[1], max = fit[2])
         })
  return(res)
}

cvm.test.rep <- function(x,distr,fit,nrep=4999){
  n <- length(x)
  switch(distr,
         "pnorm" = {
           statistic.obs <- cvm.test(x, "pnorm", mean = fit[1], sd = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rnorm(n, mean = fit[1], sd = fit[2])
             params.estimated <- closed.form.estimators("pnorm",simulated.sample)
             return(cvm.test(simulated.sample, "pnorm", mean = params.estimated[1], sd = params.estimated[2])$statistic)
           })
         },
         "pexp" = {
           statistic.obs <- cvm.test(x, "pexp", rate = fit[1])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rexp(n, rate = fit[1])
             params.estimated <- closed.form.estimators("pexp",simulated.sample)
             return(cvm.test(simulated.sample, "pexp", rate = params.estimated[1])$statistic)
           })
         },
         "pgamma" = {
           statistic.obs <- cvm.test(x, "pgamma", shape = fit[1], rate = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rgamma(n, shape = fit[1], rate = fit[2])
             params.estimated <- closed.form.estimators("pgamma",simulated.sample)
             return(cvm.test(simulated.sample, "pgamma", shape = params.estimated[1], rate = params.estimated[2])$statistic)
           })
         },
         "plnorm" = {
           statistic.obs <- cvm.test(x, "plnorm", meanlog = fit[1], sdlog = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rlnorm(n, meanlog = fit[1], sdlog = fit[2])
             params.estimated <- closed.form.estimators("plnorm",simulated.sample)
             return(cvm.test(simulated.sample, "plnorm", meanlog = params.estimated[1], sdlog = params.estimated[2])$statistic)
           })
         },
         "punif" = {
           statistic.obs <- cvm.test(setdiff(x,c(fit[1],fit[2])), "punif", min = fit[1], max = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- runif(n, min = fit[1], max = fit[2])
             params.estimated <- closed.form.estimators("punif",simulated.sample)
             return(cvm.test(setdiff(simulated.sample,c(params.estimated[1],params.estimated[2])), "punif", min = params.estimated[1], max = params.estimated[2])$statistic)
           })
         })
  p.value <- (sum(statistic.sim > statistic.obs) + 1) / (nrep + 1)
  return(list(statistic=mean(statistic.sim), p.value = p.value))
}

ks.test.one <- function(x,distr,fit){
  switch(distr,
         "pnorm" = {
           res <- ks.test(x, "pnorm", mean = fit[1], sd = fit[2])
         },
         "pexp" = {
           res <- ks.test(x, "pexp", rate = fit[1])
         },
         "pgamma" = {
           res <- ks.test(x, "pgamma", shape = fit[1], rate = fit[2])
         },
         "plnorm" = {
           res <- ks.test(x, "plnorm", meanlog = fit[1], sdlog = fit[2])
         },
         "punif" = {
           res <- ks.test(x, "punif", min = fit[1], max = fit[2])
         })
  return(res)
}

ks.test.rep <- function(x,distr,fit,nrep=4999){
  n <- length(x)
  switch(distr,
         "pnorm" = {
           statistic.obs <- ks.test(x, "pnorm", mean = fit[1], sd = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rnorm(n, mean = fit[1], sd = fit[2])
             params.estimated <- closed.form.estimators("pnorm",simulated.sample)
             return(ks.test(simulated.sample, "pnorm", mean = params.estimated[1], sd = params.estimated[2])$statistic)
           })
         },
         "pexp" = {
           statistic.obs <- ks.test(x, "pexp", rate = fit[1])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rexp(n, rate = fit[1])
             params.estimated <- closed.form.estimators("pexp",simulated.sample)
             return(ks.test(simulated.sample, "pexp", rate = params.estimated[1])$statistic)
           })
         },
         "pgamma" = {
           statistic.obs <- ks.test(x, "pgamma", shape = fit[1], rate = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rgamma(n, shape = fit[1], rate = fit[2])
             params.estimated <- closed.form.estimators("pgamma",simulated.sample)
             return(ks.test(simulated.sample, "pgamma", shape = params.estimated[1], rate = params.estimated[2])$statistic)
           })
         },
         "plnorm" = {
           statistic.obs <- ks.test(x, "plnorm", meanlog = fit[1], sdlog = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- rlnorm(n, meanlog = fit[1], sdlog = fit[2])
             params.estimated <- closed.form.estimators("plnorm",simulated.sample)
             return(ks.test(simulated.sample, "plnorm", meanlog = params.estimated[1], sdlog = params.estimated[2])$statistic)
           })
         },
         "punif" = {
           statistic.obs <- ks.test(setdiff(x,c(fit[1],fit[2])), "punif", min = fit[1], max = fit[2])$statistic
           statistic.sim <- sapply(1:nrep,function(i){
             simulated.sample <- runif(n, min = fit[1], max = fit[2])
             params.estimated <- closed.form.estimators("punif",simulated.sample)
             return(ks.test(setdiff(simulated.sample,c(params.estimated[1],params.estimated[2])), "punif", min = params.estimated[1], max = params.estimated[2])$statistic)
           })
         })
  p.value <- (sum(statistic.sim > statistic.obs) + 1) / (nrep + 1)
  return(list(statistic=mean(statistic.sim), p.value = p.value))
}


