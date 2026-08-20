#
# This source file is available at https://www4.stat.ncsu.edu/~hdbondel/software/acosso.R, implementing the method developed by:
# Storlie, C. B., Bondell, H. D., Reich, B. J., & Zhang, H. H. (2011). Surface estimation, variable selection, and the nonparametric oracle property. Statistica Sinica, 21, 679.
#



##########################################################
############### ACOSSO Function ##########################
##########################################################


acosso <- function(X, y, order=1, wt.pow=1, cv='loo', w, lambda.0, gcv.pen=0, categorical="auto", min.distinct=7){
  
  ########## INPUTS ############################################################
  ## X        - a matrix of predictors
  ## y        - a vector of responses
  ## order    - the order of interactions to consider (1 or 2)
  ## wt.pow   - the weights used in the adaptive penalty are ||P^j||^{-wt.pow} 
  ##            wt.pow=0 is the COSSO
  ## cv       - the method used to select the tuning parameter M from the ACOSSO
  ##            paper. (the tuning paramter is actually called K in this code).
  ##            Options are '5cv', 'gcv', and 'bic', or a numeric value to use 
  ##            for M
  ## w        - optional vector to use for w (only used if 'cv' is numeric)
  ## lambda.0 - optional value to use for lambda.0 (only used if 'cv' is numeric)
  ## categorical - vector containg the columns to be treated as categorical
  ## min.distinct - minimum number of distinct values to treat a predictor as
  ##                continuous (instead of categorical).  Only used if
  ##                categorical="auto".
  ##############################################################################
  
  
  ########## OUTPUTS ###########################################################
  ## c.hat   - coefficients of the Kernel representation f(x)=mu+sum K(x_i,x)*c
  ## mu.hat  - estimated constant in above representation
  ## y.hat   - vector of the predicted y's
  ## res     - vector of the residuals
  ## dfmod   - approximate degrees of freedom of the fit
  ## w       - adaptive weights used in the estimation
  ## gcv     - gcv score
  ## bic     - bic score
  ## theta   - estimated theta vector
  ## Rsq     - R^2 = 1-SSE/SSTOT
  ## Gram    - Gram matrix.  The (i,j)th element is K(x_i,x_j)
  ## y.mat   - Matrix of fitted y's for each functional component 
  ##           (i.e. y.hat = mu.hat + sum(y.mat))
  ## X       - the original matrix of predictors
  ## y	   - the original vector of inputs
  ## rescale - Did the data need to be rescaled to (0,1).  Used for prediction.
  ## order   - the order of interactions specified
  ## K	   - the value of K chosen by cv
  ## lambda.0- the value used for lambda.0
  ##############################################################################
  
  if(!is.numeric(cv)){
    ans.cv <- venus.cv(X, y, order=order, cv=cv, wt.pow=wt.pow, w='L2', f.est='trad', seed=110, K.lim='data', nK=5, gcv.pen=gcv.pen, rel.K=5E-2, nlambda=5, lambda.lim=c(1E-10, 1E3), rel.lambda=5E-2, rel.theta=1E-2, maxit=2, init.cv='gcv', cat.pos=categorical, min.distinct=min.distinct)
    
    lambda.0 <- ans.cv$lambda.0
    K <- ans.cv$K
    fit.acosso <- venus(X, y, order=order, K=ans.cv$K, gcv.pen=gcv.pen, maxit=5, rel.tol=1E-2, cv='gcv', lambda.0=ans.cv$lambda.0, w=ans.cv$w, seed=110, cat.pos=ans.cv$cat.pos, min.distinct=min.distinct)
  }
  
  
  else{  ## use a prespecified numerical value for K
    K <- cv
    if(missing(w)||missing(lambda.0)){
      ans.cv <- venus.cv(X, y, order=order, cv='gcv', wt.pow=wt.pow, w='L2', f.est='trad', seed=110, K.lim='data', nK=5, gcv.pen=gcv.pen, rel.K=5E-2, nlambda=5, lambda.lim=c(1E-10, 1E3), rel.lambda=5E-2, rel.theta=1E-2, maxit=2, init.cv='gcv', cat.pos=categorical, min.distinct=min.distinct)
      w <- ans.cv$w
      lambda.0 <- ans.cv$lambda.0
    }
    fit.acosso <- venus(X, y, order=order, K=K, gcv.pen=gcv.pen, maxit=5, rel.tol=1E-2, cv='gcv', lambda.0=lambda.0, w=w, seed=110, cat.pos=categorical, min.distinct=min.distinct)
  }
  
  fit.acosso$order <- order
  fit.acosso$K <- K
  fit.acosso$lambda.0 <- lambda.0
  fit.acosso$Gram <- NULL
  
  return(fit.acosso)
  
}



##########################################################
############### ACOSSO Prediction ########################
##########################################################


predict.acosso <- function(X.new, obj){
  
  ###################### INPUTS ################################
  ## X.new - a matrix of new values for the predictors
  ## obj - a fitted acosso object
  ##############################################################
  
  
  ###################### OUTPUT #################################
  ## a vector of the predicted y's
  ###############################################################
  
  return(predict.venus(X.new, obj, order=obj$order))
}












##############################################################################
##############################################################################
##############################################################################










##############################################################################
##############################################################################
##############################################################################









##############################################################################
############# Other Functions Used in the Creation of ACOSSO #################
##############################################################################


index <- function(m,n){
  if(m<=n) return(m:n)
  else return(numeric(0))
}

which.equal <- function(x, y){
  
  n <- length(x)
  ans <- rep(0,n)
  for(i in 1:n){
    ans[i] <- any(approx.equal(y,x[i]))
  }
  return(as.logical(ans))
}

approx.equal <- function(x, y, tol=1E-9){
  
  return(abs(x - y) < tol)
}


## Generates a matrix whose columns are random samples from 1:n
get.obs.ind <- function(n, nfolds=5, seed=220){
  
  replace.seed <- T
  if(missing(seed))
    replace.seed <- F
  
  if(replace.seed){
    ## set seed to specified value
    if(!any(ls(name='.GlobalEnv', all.names=T)=='.Random.seed')){
      set.seed(1)
    }
    save.seed <- .Random.seed
    set.seed(seed)  
  }
  
  perm <- sample(1:n, n)
  n.cv <- rep(floor(n/nfolds),nfolds)
  rem <- n - n.cv[1]*nfolds
  n.cv[index(1,rem)] <- n.cv[index(1,rem)]+1
  obs.ind <- list()
  ind2 <- 0
  
  for(i in 1:nfolds){
    ind1 <- ind2+1
    ind2 <- ind2+n.cv[i]
    obs.ind[[i]] <- perm[ind1:ind2]
  }
  if(replace.seed){
    ## restore random seed to previous value
    .Random.seed <<- save.seed
  }
  return(obs.ind)
}



ginv2 <- function(X, eps=1E-12){
  
  eig.X <- eigen(X, symmetric=T)
  P <- eig.X[[2]]
  lambda <- eig.X[[1]]
  ind <- lambda>eps
  lambda[ind] <- 1/lambda[ind]
  lambda[!ind] <- 0
  ans <- P%*%diag(lambda,nrow=length(lambda))%*%t(P)
  return(ans)
}

sym.sqrt <- function(X){
  
  eig.X <- eigen(X)
  P <- eig.X[[2]]
  lambda <- eig.X[[1]]
  lambda <- sqrt(lambda)
  ans <- P%*%diag(lambda)%*%t(P)
  return(ans)
}


sym.sqrt.inv <- function(X, eps=1E-12){
  
  eig.X <- eigen(X)
  P <- eig.X[[2]]
  lambda <- eig.X[[1]]
  ind <- lambda>eps
  lambda[ind] <- 1/sqrt(lambda[ind])
  lambda[!ind] <- 0
  ans <- P%*%diag(lambda)%*%t(P)
  return(ans)
}



################################
##### Sobolev RK function ######
################################


k1 <- function(t){
  return(t-.5)
}
k2 <- function(t){
  return( (k1(t)^2-1/12)/2 )
}
k4 <- function(t){
  return( (k1(t)^4-k1(t)^2/2+7/240)/24 )
}
K.sob <- function(s,t){
  
  ans <- k1(s)*k1(t) + k2(s)*k2(t) - k4(abs(s-t))
  return(ans)
}

K.sob <- function(s,t){
  
  ans <- k1(s)*k1(t) + k2(s)*k2(t) - k4(abs(s-t))
  return(ans)
}


################################
##### Sobolev RK function ######
################################

K.cat <- function(s,t,G){
  
  ans <- (G-1)/G*(s==t) - 1/G*(s!=t) 
  return(ans)
}




###### Get MC integral for d2y

est.d2y <- function(y, x){
  
  n <- length(y)
  ord.x <- order(x)
  x <- x[ord.x]
  y <- y[ord.x]
  dy <- (y[-1]-y[-n])/(x[-1]-x[-n])
  d2y <- (dy[-1]-dy[-(n-1)])/((x[-c(1,2)]-x[-c(n-1,n)])/2)
  
  #           (((x[-c(1,n)]-x[-c(n-1,n)])+(x[-c(1,2)]-x[-c(1,n)]))/2)
  return(d2y)
}


#########################################################################
##### Create cross reference for kth component & ij th interaction ######
#########################################################################

get.int2ind <- function(p){
  
  ## create int2ind and ind2int to be able to switch back and forth between
  ##  i,j th interaction and kth component
  P <- p + choose(p,2)
  int2ind <- matrix(0,p,p)
  ind2int <- matrix(NA,P,2)
  diag(int2ind) <- index(1,p)
  ind2int[index(1,p),] <- cbind(index(1,p), index(1,p))
  next.ind <- p+1
  for(i in index(1,p-1)){
    for(j in index(i+1,p)){
      int2ind[i,j] <- int2ind[j,i] <- next.ind
      ind2int[next.ind,] <- c(i,j)
      next.ind <- next.ind+1
    }
  }
  return(list(int2ind=int2ind, ind2int=ind2int))
}


#########################################################################
############ Creates p Gram matrices, 1 for each predictor  #############
#########################################################################

get.gram <- function(X1, X2, order, cat.pos){
  
  ## Calculates K(X1[i,j]
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  p <- ncol(X1)
  gram <- list()
  if(length(cat.pos)>0)
    cont.pos <- (1:p)[-cat.pos]
  else
    cont.pos <- (1:p)
  
  for(i in cont.pos){
    x1 <- rep(X1[,i], times=n2)
    x2 <- rep(X2[,i], each=n1)
    ans <- K.sob(x1,x2)
    gram[[i]] <- matrix(ans, n1, n2)
  }
  for(i in cat.pos){
    x1 <- rep(X1[,i], times=n2)
    x2 <- rep(X2[,i], each=n1)
    G <- length(unique(x1))
    ans <- K.cat(x1,x2,G)
    gram[[i]] <- matrix(ans, n1, n2)
  }
  if(order==2){
    next.ind <- p+1
    for(i in index(1,p-1)){
      for(j in index(i+1,p)){
        gram[[next.ind]] <- gram[[i]]*gram[[j]]
        next.ind <- next.ind+1
      }
    }
  }
  return(gram)
}


#########################################################################
######### Creates a single Gram matrix for Prediction of new obs ########
#########################################################################


get.gram.predict <- function(X1, X2, order, theta, w, cat.pos){
  
  n1 <- nrow(X1)
  n2 <- nrow(X2)
  p <- ncol(X1)
  gram.mat <- matrix(0, n1, n2)
  gram <- list()
  if(length(cat.pos)>0)
    cont.pos <- (1:p)[-cat.pos]
  else
    cont.pos <- (1:p)
  
  for(i in cont.pos){
    x1 <- rep(X1[,i], times=n2)
    x2 <- rep(X2[,i], each=n1)
    ans <- K.sob(x1,x2)
    gram[[i]] <- matrix(ans, n1, n2)
    if(theta[i] > 1E-6)
      gram.mat <- gram.mat + theta[i]*w[i]^2*gram[[i]]
  }
  for(i in cat.pos){
    x1 <- rep(X1[,i], times=n2)
    x2 <- rep(X2[,i], each=n1)
    G <- length(unique(x1))
    ans <- K.cat(x1,x2,G)
    gram[[i]] <- matrix(ans, n1, n2)
    if(theta[i] > 1E-6)
      gram.mat <- gram.mat + theta[i]*w[i]^2*gram[[i]]
  }
  if(order==2){
    next.ind <- p+1
    for(i in index(1,p-1)){
      for(j in index(i+1,p)){
        if(theta[next.ind] > 1E-6)
          gram.mat <-gram.mat+theta[next.ind]*w[next.ind]^2*gram[[i]]*gram[[j]]
        next.ind <- next.ind+1
      }
    }
  }
  return(gram.mat)
}


###### Not using since efficiency gain was minimal #######
#get.gram.predict.C <- function(X1, X2, order, theta, w){
#  n1 <- nrow(X1)
#  n2 <- nrow(X2)
#  p <- ncol(X1)
#  P <- length(theta)
#  gram.mat <- matrix(0, n1, n2)  
#  gram.mat <- .C("R_get_gram_predict", as.double(as.matrix(X1)), as.integer(n1),  as.integer(p), as.double(as.matrix(X2)), as.integer(n2), as.integer(order), as.double(theta), as.integer(P), as.double(w), as.double(gram.mat))[[10]]
#
#  return(matrix(gram.mat, ncol=n2))
#}



###############################################################################
##### Creates a single Gram matrix for Prediction of a bootstrap sample #######
###############################################################################

get.gram.boot <- function(X.perm, gram.list, order, theta, w){
  
  n1 <- nrow(X.perm)
  n2 <- nrow(gram.list[[1]])
  p <- ncol(X.perm)
  gram.mat <- matrix(0, n1, n2)
  gram.new.list <- list()
  ind2int <- get.int2ind(p)$ind2int
  imp.vars <- sort(unique(as.vector(ind2int[theta>1E-6,])))
  
  for(i in imp.vars){
    #    gram.new.list[[i]] <- gram.list[[i]][X.perm[,i],]
    #    if(theta[i] > 1E-6)
    #      gram.mat <- gram.mat + theta[i]*w[i]^2*gram.new.list[[i]]
    
    if(theta[i] > 1E-6)
      gram.mat <- gram.mat + theta[i]*w[i]^2*gram.list[[i]][X.perm[,i],]
  }
  if(order==2){
    ind <-  which(theta>1E-6 & c(rep(F,p), rep(T,choose(p,2))))
    for(i in ind){
      int1 <- ind2int[i,1]
      int2 <- ind2int[i,2]
      #      gram.mat <- gram.mat + theta[i]*w[i]^2*gram.new.list[[int1]]*
      #                             gram.new.list[[int2]]
      
      gram.mat <- gram.mat + theta[i]*w[i]^2*gram.list[[int1]][X.perm[,int1],]*
        gram.list[[int2]][X.perm[,int2],]
      
    }
  }
  return(gram.mat)
}




#########################################################################
############# Fits a penalized spline for given theta's  ################
#########################################################################


pen.spline <- function(X, y, order=1, lambda.0=1, theta, gcv.pen=1.01,
                       Gram, cv='gcv', seed=220){
  
  n <- nrow(X)
  p <- ncol(X)
  if(order==1)
    P <- p
  else
    P <- p + choose(p,2)
  
  if(missing(theta))
    theta <- 1
  if(length(theta)==1)
    theta <- rep(theta,P)
  
  ## shift and rescale x's to [0,1]
  rescale <- rep(F, p)
  for(i in 1:p){
    if(any( X[,i]<0 | X[,i]>1)){
      X[,i] <- (X[,i]-min(X[,i]))/(max(X[,i])-min(X[,i]))
      rescale[i] <- T
    }
  }
  
  ## Get Gram Matrix
  if(missing(Gram)){
    ## Get Gram Matrix
    Gram <- get.gram(X, X, order=order, cat.pos=numeric(0))
  }
  
  ############### Use 5 fold CV ##################
  if(cv=='5cv'){
    y.hat <- numeric(n)
    obs.ind <- get.obs.ind(n, nfolds=5, seed=seed)
    for(i in 1:5){
      X.i <- as.matrix(X[-obs.ind[[i]],])
      y.i <- y[-obs.ind[[i]]]
      Gram.i <- list()
      for(j in 1:P)
        Gram.i[[j]] <- (Gram[[j]])[-obs.ind[[i]],-obs.ind[[i]]]
      fit.i <- pen.spline(X.i, y.i, order=order, lambda.0=lambda.0,theta=theta,
                          cv='gcv', gcv.pen=gcv.pen, Gram=Gram.i)
      
      K.theta <- matrix(0,n,n)
      for(j in 1:P)
        K.theta <- K.theta + fit.i$theta[j]*Gram[[j]]
      y.hat[obs.ind[[i]]]<- K.theta[obs.ind[[i]],-obs.ind[[i]]]%*%fit.i$c.hat +
        fit.i$mu.hat 
    }
    fit.spline <- pen.spline(X, y, order=order, lambda.0=lambda.0, theta=theta,
                             cv='gcv', gcv.pen=gcv.pen, Gram=Gram)
    gcv <- sum((y-y.hat)^2)
    mu.hat <- fit.spline$mu.hat
    c.hat <- fit.spline$c.hat
    y.hat <- fit.spline$y.hat
    y.mat <- fit.spline$y.mat
    res <- fit.spline$res
    df <- fit.spline$df 
    Rsq <- fit.spline$Rsq
    bic <- fit.spline$bic
    norm <- fit.spline$norm
  }
  
  
  ############### Use GCV ##################
  else{  ## Use gcv
    
    
    K.theta <- matrix(0,n,n)
    for(i in 1:P){
      K.theta <- K.theta + theta[i]*Gram[[i]]
    }
    
    K.inv <- solve(K.theta + lambda.0*diag(n))
    J <- rep(1,n)
    alpha <- sum(K.inv)^(-1)*t(J)%*%K.inv
    mu.hat <- as.numeric(alpha%*%y)
    c.hat <- K.inv%*%(y-J*mu.hat)
    H <- K.theta%*%K.inv%*%(diag(n)-J%*%alpha)+J%*%alpha
    
    y.hat <- mu.hat + K.theta%*%c.hat
    res <- y-y.hat
    SSE <- sum(res^2)
    Rsq <- 1-SSE/sum((y-mean(y))^2)
    df <- sum(diag(H)) 
    if(gcv.pen*df >= n)
      gcv <- Inf
    else
      gcv <- SSE/(1-gcv.pen*df/n)^2
    bic <- n*log(SSE/n) + df*log(n)
    norm <- numeric(P)
    for(i in 1:P){
      norm[i] <- t(c.hat)%*%(theta[[i]]^2*Gram[[i]])%*%c.hat
    }
    y.mat <- matrix(0,n,P)
    for(i in 1:P){
      y.mat[,i] <- theta[[i]]*Gram[[i]]%*%c.hat
    }
    
  }
  
  return(list(mu.hat=mu.hat, c.hat=c.hat, y.hat=y.hat, res=res, dfmod=df, 
              gcv=gcv, bic=bic, Rsq=Rsq, norm=norm, y.mat=y.mat, X=X, y=y,
              theta=theta, Gram=Gram, w=rep(1,P), rescale=rescale))
  
}



#########################################################################
##### used by VENUS: solves for beta.hat and c.hat for fixed theta's ####
#########################################################################


get.H.c <- function(Gram, y, theta, lambda.0, w, gcv.pen){
  
  n <- length(y)
  P <- length(theta)
  
  K.theta <- matrix(0,n,n)
  for(i in 1:P){
    K.theta <- K.theta + theta[i]*w[i]^2*Gram[[i]]
  }
  K.inv <- solve(K.theta + lambda.0*diag(n))
  J <- rep(1,n)
  alpha <- sum(K.inv)^(-1)*t(J)%*%K.inv
  mu.hat <- as.numeric(alpha%*%y)
  c.hat <- K.inv%*%(y-J*mu.hat)
  H <- K.theta%*%K.inv%*%(diag(n)-J%*%alpha)+J%*%alpha
  
  df <- sum(diag(H))
  y.hat <- mu.hat + K.theta%*%c.hat
  res <- y-y.hat
  SSE <- sum(res^2)
  if(gcv.pen*df >= n)
    gcv <- Inf
  else
    gcv <- SSE/(1-gcv.pen*df/n)^2
  bic <- n*log(SSE/n) + df*log(n)
  Rsq <- 1-SSE/sum((y-mean(y))^2)
  
  # Addition by SDV : computation of Q2 leave-one-out
  C <- K.theta + lambda.0*diag(n)
  T <- chol(C)
  M <- backsolve(t(T), J, upper.tri = FALSE)
  Cinv <- chol2inv(T)
  Cinv.F <- Cinv %*% J
  T.M <- chol(crossprod(M))
  aux <- backsolve(t(T.M), t(Cinv.F), upper.tri=FALSE)
  Q <- Cinv - crossprod(aux)
  Q.y <- Q%*%y
  sigma2 <- 1/diag(Q)
  epsilon <- sigma2 * (Q.y)
  yloo <- as.vector(y - epsilon)
  Q2loo <- 1 - sum((y-yloo)^2)/sum((y-mean(y))^2)

  return(list(mu.hat=mu.hat, H=H, df=df, y.hat=y.hat, gcv=gcv, Rsq=Rsq,
              c.hat=c.hat, bic=bic, Q2loo=Q2loo))
}



#######################################################################
############# used by VENUS: solves for theta for fixed c's ###########
#######################################################################


get.theta.hat <- function(Gram, mu.hat, c.hat, y, lambda.0, theta.0.ind, K, w){
  
  #theta.0.ind <- numeric(0)
  
  n <- length(y)
  P <- length(Gram)
  if(length(theta.0.ind)==0)
    keep.col <- 1:P
  else
    keep.col <- (1:P)[-theta.0.ind]
  G <- matrix(NA,n,P)
  
  #n..<<-n
  #Gram..<<-Gram
  #c.hat..<<-c.hat
  #keep.col..<<-keep.col
  
  for(i in 1:P){
    G[,i] <- w[i]^2*Gram[[i]]%*%c.hat
  }
  G.red <- G[,keep.col]
  P.red <- length(keep.col)
  D <- t(G.red)%*%G.red
  d <- t( (t(y) - mu.hat - .5*lambda.0*t(c.hat))%*%G.red )
  A <- t(rbind(diag(1,P.red), rep(-1,P.red)))
  b.0 <- c(rep(0,P.red), -K)
  
  #G.red..<<-G.red
  #P.red..<<-P.red
  #A2..<<-A
  #D2..<<-D
  #d2..<<-d
  #b2.0..<<-b.0
  
  #opt.ans <- solve.QP2(D, d, A, b.0, meq=0)
  #add.to.diag <- sum(diag(D)/ncol(D))*1E-10
  #while(opt.ans$solution[1] == -2){
  #  add.to.diag <- add.to.diag*10
  #  opt.ans <- solve.QP2(D+diag(add.to.diag,P.red), d, A, b.0, meq=0)
  #}
  
  
  opt.ans <- try(solve.QP(D, d, A, b.0, meq=0), silent=T)
  add.to.diag <- sum(diag(D)/ncol(D))*1E-10
  while(is.character(opt.ans)){
    add.to.diag <- add.to.diag*10
    opt.ans <- try(solve.QP(D+diag(add.to.diag,P.red), d, A, b.0, meq=0), silent=T)
  }
  
  theta.new <- opt.ans$solution
  theta <- rep(0,P)
  theta[keep.col] <- theta.new
  return(theta)
}


#######################################################################
######### VENUS for a fixed smoothing param M   #######################
#######################################################################


venus <- function(X, y, K, order=1, gcv.pen=1.01,lambda.0, theta.0, rel.tol=1E-2,
                  maxit=2, Gram, cv='gcv', w, seed=220, 
                  alpha=.05, nvar=ncol(X), nfit=20, cat.pos="auto",
                  min.distinct=7){
  
  n <- nrow(X)
  p <- ncol(X)
  if(order==1)
    P <- p
  else
    P <- p + choose(p,2)
  if(missing(lambda.0))
    lambda.0 <- .01
  if(missing(theta.0))
    theta.0 <- rep(1,P)
  if(missing(w))
    w <- rep(1,P)
  if(length(w)==1)
    w <- rep(w,P)
  theta.hat <- theta.0
  
  ## Identify categorical vars 
  if(length(cat.pos)>0 && cat.pos[1]=='auto'){
    # scan for variables with min.distinct or less distinct values. 
    cat.pos <- numeric(0)
    for(i in 1:p){
      unique.i <- unique(X[,i])
      if(length(unique.i)<=min.distinct || is.character(X[,i])){
        cat.pos <- c(cat.pos, i)
      }
    }
  }
  
  if(length(cat.pos)>0)
    cont.pos <- (1:p)[-cat.pos]
  else
    cont.pos <- (1:p)
  ## shift and rescale continuous x's to [0,1]
  rescale <- rep(F, p)
  X.orig <- X
  for(i in cont.pos){
    if(any( X[,i]<0 | X[,i]>1)){
      X[,i] <- (X[,i]-min(X[,i]))/(max(X[,i])-min(X[,i]))*.9 + .05
      rescale[i] <- T
    }
  }
  
  ## Get Gram Matrix
  if(missing(Gram)){
    Gram <- get.gram(X, X, order=order, cat.pos=cat.pos)
  }
  
  ############### Use 5 fold CV ##################
  if(cv=='5cv'){
    y.hat <- numeric(n)
    obs.ind <- get.obs.ind(n, nfolds=5, seed=seed)
    for(i in 1:5){
      X.i <- as.matrix(X[-obs.ind[[i]],])
      y.i <- y[-obs.ind[[i]]]
      Gram.i <- list()
      for(j in 1:P)
        Gram.i[[j]] <- (Gram[[j]])[-obs.ind[[i]],-obs.ind[[i]]]
      fit.i<-venus(X=X.i, y=y.i, order=order, K=K, gcv.pen=gcv.pen, 
                   lambda.0=.8*lambda.0, theta.0=theta.0, rel.tol=rel.tol,
                   maxit=maxit, Gram=Gram.i, cv='gcv', w=w,
                   min.distinct=min.distinct)
      K.theta <- matrix(0,n,n)
      for(j in 1:P)
        K.theta <- K.theta + fit.i$theta[j]*w[j]^2*Gram[[j]]
      
      y.hat[obs.ind[[i]]]<- K.theta[obs.ind[[i]],-obs.ind[[i]]]%*%fit.i$c.hat +
        fit.i$mu.hat 
    }
    
    fit.venus <- venus(X=X, y=y, order=order, K=K, gcv.pen=gcv.pen, 
                       lambda.0=lambda.0, theta.0=theta.0, rel.tol=rel.tol, 
                       maxit=maxit, Gram=Gram, cv='gcv', w=w,
                       min.distinct=min.distinct)
    fit.venus$Gram <- NULL
    gcv <- sum((y-y.hat)^2)
    mu.hat <- fit.venus$mu.hat
    c.hat <- fit.venus$c.hat
    y.hat <- fit.venus$y.hat
    res <- fit.venus$res
    df <- fit.venus$df 
    theta.hat <- fit.venus$theta
    Rsq <- fit.venus$Rsq
    bic <- fit.venus$bic
    Q2loo <- fit.venus$Q2loo
  }
  
  ############### Use 10 fold CV ##################
  else if(cv=='10cv'){
    y.hat <- numeric(n)
    obs.ind <- get.obs.ind(n, nfolds=10, seed=seed)
    for(i in 1:10){
      X.i <- as.matrix(X[-obs.ind[[i]],])
      y.i <- y[-obs.ind[[i]]]
      Gram.i <- list()
      for(j in 1:P)
        Gram.i[[j]] <- (Gram[[j]])[-obs.ind[[i]],-obs.ind[[i]]]
      fit.i<-venus(X=X.i, y=y.i, order=order, K=K, gcv.pen=gcv.pen, 
                   lambda.0=.9*lambda.0, theta.0=theta.0, rel.tol=rel.tol,
                   maxit=maxit, Gram=Gram.i, cv='gcv', w=w,
                   min.distinct=min.distinct)
      K.theta <- matrix(0,n,n)
      for(j in 1:P)
        K.theta <- K.theta + fit.i$theta[j]*w[j]^2*Gram[[j]]
      
      y.hat[obs.ind[[i]]]<- K.theta[obs.ind[[i]],-obs.ind[[i]]]%*%fit.i$c.hat +
        fit.i$mu.hat 
    }
    
    fit.venus <- venus(X=X, y=y, order=order, K=K, gcv.pen=gcv.pen, 
                       lambda.0=lambda.0, theta.0=theta.0, rel.tol=rel.tol, 
                       maxit=maxit, Gram=Gram, cv='gcv', w=w,
                       min.distinct=min.distinct)
    fit.venus$Gram <- NULL
    gcv <- sum((y-y.hat)^2)
    mu.hat <- fit.venus$mu.hat
    c.hat <- fit.venus$c.hat
    y.hat <- fit.venus$y.hat
    res <- fit.venus$res
    df <- fit.venus$df 
    theta.hat <- fit.venus$theta
    Rsq <- fit.venus$Rsq
    bic <- fit.venus$bic
    Q2loo <- fit.venus$Q2loo
  }
  
  ############### Use type I Error Rate ##################
  else if(cv=='var'){
    for(i in 1:nfit){
      X.i <- as.matrix(X[-obs.ind[[i]],])
      y.i <- y[-obs.ind[[i]]]
      Gram.i <- list()
      for(j in 1:P)
        Gram.i[[j]] <- (Gram[[j]])[-obs.ind[[i]],-obs.ind[[i]]]
      fit.i<-venus(X=X.i, y=y.i, order=order, K=K, gcv.pen=gcv.pen, 
                   lambda.0=lambda.0, theta.0=theta.0, rel.tol=rel.tol,
                   maxit=maxit, Gram=Gram.i, cv='gcv', w=w,
                   min.distinct=min.distinct)
      K.theta <- matrix(0,n,n)
      for(j in 1:P)
        K.theta <- K.theta + fit.i$theta[j]*w[j]^2*Gram[[j]]
      
      y.hat[obs.ind[[i]]]<- K.theta[obs.ind[[i]],-obs.ind[[i]]]%*%fit.i$c.hat +
        fit.i$mu.hat 
    }
    
    fit.venus <- venus(X=X, y=y, order=order, K=K, gcv.pen=gcv.pen, 
                       lambda.0=lambda.0, theta.0=theta.0, rel.tol=rel.tol, 
                       maxit=maxit, Gram=Gram, cv='gcv', w=w,
                       min.distinct=min.distinct)
    fit.venus$Gram <- NULL
    gcv <- sum((y-y.hat)^2)
    mu.hat <- fit.venus$mu.hat
    c.hat <- fit.venus$c.hat
    y.hat <- fit.venus$y.hat
    res <- fit.venus$res
    df <- fit.venus$df 
    theta.hat <- fit.venus$theta
    Rsq <- fit.venus$Rsq
    bic <- fit.venus$bic
    Q2loo <- fit.venus$Q2loo
  }
  
  ############### Use gcv ##################
  else{
    
    ## Solve for c.hat when theta=theta.0
    H.c <- get.H.c(Gram, y, theta.0, lambda.0, w, gcv.pen)
    mu.hat <- H.c$mu.hat
    c.hat <- H.c$c.hat
    df <- H.c$df
    y.hat <- H.c$y.hat
    res <- H.c$res
    gcv <- H.c$gcv
    bic <- H.c$bic
    Rsq <- H.c$Rsq
    Q2loo <- H.c$Q2loo
    
    ## Iterative solving for theta, then c ...
    iter <- 1
    
    theta.0.ind <- which(theta.hat < 1E-9 | w==0)
    
    repeat{
      #cat("\niter =", iter)
      if(iter > maxit){
        #warning(paste("Maximum iterations", maxit, "reached"))
        break
      }
      iter <- iter + 1
      
      ## First solve for theta for a fixed c
      if(K==0){
        theta.new <- rep(0,P)
      }
      else{
        theta.new <- get.theta.hat(Gram, mu.hat, c.hat, y, lambda.0, 
                                   theta.0.ind, K, w)
        if(any(theta.new == -1)||any(is.na(theta.new))){### prob with solution
          theta.new <- theta.hat
        }
      }
      
      theta.0.ind <- which(theta.new <= 1E-9 | w==0)
      theta.new[theta.0.ind] <- 0
      
      #print(theta.new)
      
      ## Now solve for c.hat for a fixed theta
      H.c <- get.H.c(Gram, y, theta.new, lambda.0, w, gcv.pen)
      mu.hat <- H.c$mu.hat
      c.hat <- H.c$c.hat
      H <- H.c$H
      df <- H.c$df
      y.hat <- H.c$y.hat
      
      
      res <- H.c$res
      gcv <- H.c$gcv
      bic <- H.c$bic
      Rsq <- H.c$Rsq
      Q2loo <- H.c$Q2loo
      
      ## Now check for convergence
      divisor <- theta.new
      divisor[theta.new < 1E-6] <- 1
      rel.norm <- sqrt(sum(((theta.hat-theta.new)/divisor)^2))
      
      #theta.new..<<-theta.new
      #theta.hat..<<-theta.hat
      #cat("\ntheta.hat =","\n")
      #print(theta.hat)
      #cat("\nrel.norm =", rel.norm,"\n")
      
      if(rel.norm < rel.tol){
        theta.hat <- theta.new
        break
      }
      theta.hat <- theta.new
    }  
  }
  y.mat <- matrix(0,n,P)
  for(i in 1:P){
    y.mat[,i] <- theta.hat[i]*w[i]^2*Gram[[i]]%*%c.hat
  }
  return(list(c.hat=c.hat, mu.hat=mu.hat, y.hat=y.hat, res=res, dfmod=df,
              w=w, gcv=gcv, bic=bic, Q2loo=Q2loo, theta=theta.hat, Rsq=Rsq, Gram=Gram, 
              y.mat=y.mat, X=X, X.orig=X.orig, y=y, rescale=rescale,
              cat.pos=cat.pos))
  
}



#######################################################################
############# Predict new obs for Venus ###############################
#######################################################################

predict.venus <- function(X.new, obj, order=1){
  
  theta <- obj$theta
  w <- obj$w
  c.hat <- obj$c.hat
  mu.hat <- obj$mu.hat
  X <- obj$X
  X.orig <- obj$X.orig
  p <- ncol(X)
  rescale <- obj$rescale
  cat.pos <- obj$cat.pos
  
  ## shift and rescale x's to [0,1]
  for(i in (1:p)[rescale])
    X.new[,i] <- (X.new[,i]-min(X.orig[,i]))/(max(X.orig[,i])-min(X.orig[,i]))*.9 + .05
  
  ## Get Gram Matrix & predict y
  Gram.mat <- get.gram.predict(X.new, X, order, theta, w, cat.pos=cat.pos)
  y.hat <- as.vector(Gram.mat%*%c.hat + mu.hat)
  return(y.hat)
}


#######################################################################
############# Predict bootstrap sample of obs for Venus ###############
#######################################################################

predict.acosso.boot <- function(X.perm, obj){
  
  gram.list <- obj$Gram
  order <- obj$order
  theta <- obj$theta
  w <- obj$w
  c.hat <- obj$c.hat
  mu.hat <- obj$mu.hat
  cat.pos <- obj$cat.pos
  
  ## Get Gram Matrix & predict y
  Gram.mat <- get.gram.boot(X.perm, gram.list, order, theta, w)
  y.hat <- as.vector(Gram.mat%*%c.hat + mu.hat)
  return(y.hat)
}


#######################################################################
########## Predict component curves for Venus #########################
#######################################################################

predict.venus.components <- function(X.new, obj, order=1){
  
  n.new <- nrow(X.new)
  theta <- obj$theta
  w <- obj$w
  c.hat <- obj$c.hat
  mu.hat <- obj$mu.hat
  X <- obj$X
  n <- nrow(obj$X)
  p <- ncol(X)
  rescale <- obj$rescale
  cat.pos <- obj$cat.pos
  if(order==1)
    P <- p
  else
    P <- p + choose(p,2)
  
  ## shift and rescale x's to [0,1]
  for(i in (1:p)[rescale])
    X.new[,i] <- (X.new[,i]-min(X[,i]))/(max(X[,i])-min(X[,i]))
  
  ## Get Gram Matrix
  Gram <- get.gram(X.new, X, order, cat.pos=cat.pos)
  
  y.mat <- matrix(0,n.new,P)
  for(j in 1:P){
    y.mat[,j] <- theta[j]*w[j]^2*Gram[[j]]%*%c.hat
  }
  y.hat <- rowSums(y.mat) + mu.hat
  
  return(list(y.hat=y.hat, y.mat=y.mat))
}


#######################################################################
################# GET NORMS FOR A VENUS OBJECT  #######################
#######################################################################

get.venus.norms <- function(obj){
  
  y.mat <- obj$y.mat
  P <- ncol(y.mat)
  c.hat <- obj$c.hat
  Gram <- obj$Gram
  theta <- obj$theta
  w <- obj$w
  
  L2.norm <- numeric(P)
  H2.norm <- numeric(P)
  for(i in 1:P){
    H2.norm[i] <- sqrt(theta[i]^2*w[i]^4*t(c.hat)%*%(Gram[[i]])%*%c.hat)
    L2.norm[i] <- sqrt(mean((y.mat[,i])^2))
  }
  
  return(list(L2.norm=L2.norm, H2.norm=H2.norm))
}



#######################################################################
############# Plot the functions for additive model ###############
#######################################################################

plot.venus <- function(..., y.true, nvar, pch.col=grey(.6), X, y, use.col=T, 
                       use.lty=F, use.grey=T, use.lwd=F, legend=T, line=2.25, 
                       leg.pos, leg.cex=.8, true.only=F, no.data=F, 
                       xlab=paste("x_",1:nvar,sep=""), ylab=rep('y',nvar),
                       special.labels.x=F, special.labels.y=F, font.lab=3){
  
  ## ... are fit objects
  obj.list <- list(...)
  nf <- length(obj.list)
  if(nf>=1){
    nf <- length(obj.list)
    X <- obj.list[[nf]]$X
    y <- obj.list[[nf]]$y - mean(obj.list[[1]]$y)
    y.mat <- obj.list[[nf]]$y.mat
    if(missing(y.true)){
      y.true <- matrix(0, nrow=nrow(y.mat), ncol=ncol(y.mat))
    }
    if(ncol(y.true)<ncol(y.mat)){
      y.true <- cbind(y.true, matrix(0, nrow=nrow(y.mat), 
                                     ncol=ncol(y.mat)-ncol(y.true)))
    }
    if(special.labels.x==T)
      xlab=rep('',nf)
    if(special.labels.y==T)
      ylab=rep('',nf) 
    if(no.data){
      pch.col <- 0
      y <- rep(0,length(y))
      y[1] <- min(y.true)
      y[2] <- max(y.true)
      for(i in 1:nf){
        y[i+2] <- min(obj.list[[nf]]$y.mat)
        y[nf+i+2] <- max(obj.list[[nf]]$y.mat)
      }
    }    
    
    ## Remove intercept from y.true
    for(i in 1:ncol(y.true))
      y.true[,i] <- y.true[,i] - mean(y.true[,i])
    
    if(missing(nvar) || nvar > min(ncol(X), ncol(y.mat)))
      nvar <- min(ncol(X), ncol(y.mat))
    if(missing(nvar))
      nvar <- min(9,ncol(y.mat))
    if(nvar <= 2)
      par(mfrow=c(nvar,1), mar=c(3.5,3,1,.5))
    else if(nvar <= 4)
      par(mfrow=c(2,2), mar=c(3.5,3,1,.5))
    else if(nvar <= 6)
      par(mfrow=c(3,2), mar=c(3.5,3,1,.5))
    else if(nvar <= 9)
      par(mfrow=c(3,3), mar=c(3.5,3,1,.5))
    else if(nvar <= 12)
      par(mfrow=c(4,3), mar=c(3.5,3,1,.5))
    else if(nvar <= 16)
      par(mfrow=c(4,4), mar=c(3.5,3,1,.5))
    else
      par(mfrow=c(5,4), mar=c(3.5,3,1,.5))
    
    for(i in 1:nvar){
      xi <- X[,i]
      if(no.data){
        y[1] <- min(y.true[,i])
        y[2] <- max(y.true[,i])
        for(j in 1:nf){
          y[j+2] <- min(obj.list[[nf]]$y.mat[,i])
          y[nf+j+2] <- max(obj.list[[nf]]$y.mat[,i])
        }
        plot(xi,y,col=0,ylim=c(min(y),max(y)), xlab="", ylab="")
      }
      else{
        plot(xi,y,col=pch.col,ylim=c(min(y),max(y)), xlab="", ylab="")
      }
      if(use.lty) t.lty <- 1
      else t.lty <- 2
      if(true.only) t.lwd <- 1.0
      else t.lwd <- 1
      lines(xi[order(xi)],y.true[order(xi),i],col=1,lty=t.lty,lwd=t.lwd,ylim=c(min(y),max(y)))
      ## Plot each fit in obj.list
      if(use.col==T) col <- 2:(nf+1)
      else if(use.grey==T) col <- grey(seq(0,.7,length=nf))[c(2,1,index(3,nf))]
      else col <- rep(1,nf)
      if(use.lty==T) lty <- seq(2*nf-1, 1, by=-2)
      else lty <- rep(1,nf)
      if(use.lwd==T){
        if(nf>1) lwd <- (c(seq(1,nf,by=2), seq(2,nf,by=2))*.5)[c(2,1,index(3,nf))]
        else lwd <- 1
      }
      else lwd <- rep(1,nf)
      
      if(!true.only){
        for(j in 1:nf){
          y.hat.j <- obj.list[[j]]$y.mat[,i] - mean(obj.list[[j]]$y.mat[,i])
          lines(xi[order(xi)],y.hat.j[order(xi)],col=col[j],lty=lty[j],lwd=lwd[j],ylim=c(min(y),max(y)))
        }
        if(missing(leg.pos))
          leg.pos <- c(-.0125, max(y-.01))
        if(legend && i==1){
          legend(x=leg.pos[1],y=leg.pos[2],lty=c(t.lty, lty), lwd=c(1, lwd), col=c(1,col), cex=.7)
        }
      }
      title(xlab=xlab[i], ylab=ylab[i], line=line, font.lab=font.lab, family='serif')
      if(special.labels.x){
        title(xlab="X", line=line-.2, font.lab=font.lab, family='serif')
        title(xlab=paste("      ",i,sep=''), line=line-.05, cex.lab=.75, family='serif')
      }
      if(special.labels.y){
        title(ylab=" P  f ", line=line-.35, font.lab=font.lab, family='serif')
        title(ylab=paste("  ",i,sep=''), line=line, cex.lab=.75, family='serif')
      }
    }
  }
  
  else{  ## no fits .  Just plot truth
    if(missing(nvar))
      nvar <- min(9,ncol(y.true))
    if(nvar <= 2)
      par(mfrow=c(nvar,1), mar=c(2,4,2,1))
    else if(nvar <= 4)
      par(mfrow=c(2,2), mar=c(2,4,2,1))
    else if(nvar <= 6)
      par(mfrow=c(3,2), mar=c(2,4,2,1))
    else
      par(mfrow=c(3,3), mar=c(2,4,2,1))
    for(i in 1:nvar){
      xi <- X[,i]
      plot(xi,y,col=pch.col,ylim=c(min(y),max(y)),xlab=xlab[i],ylab=ylab[i])
      lines(xi[order(xi)],y.true[order(xi),i],col=1,lty=3,ylim=c(min(y),max(y)))
    }
  }
  par(mfrow=c(1,1), mar=c(5,4,4,2))
  invisible()
}



#######################################################################
######### ESTIMATE w VIA FULL SMOOTHING SPLINE ###################
#######################################################################

get.w <- function(X, y, order, lambda.lim, nlambda=3, gcv.pen, wt.pow,
                  rel.tol, df.lim, w='L2', w.0=1, w.lim=c(1E-10, 1E10), Gram,
                  cat.pos){
  
  n <- nrow(X)
  p <- ncol(X)
  if(order==1)
    P <- p
  else
    P <- p + choose(p,2)  
  
  if(missing(df.lim)){
    df.lim <- c(0,.5*n)
  }
  
  if(w[1]!='L2' && w[1]!='RKHS'){
    if(length(w)==1)
      w <- rep(w,P)
    w.0 <- w
  }
  else if(length(w.0)==1){
    w.0 <- rep(w.0,P)
  }
  
  ## shift and rescale x's to [0,1]
  for(i in 1:p){
    if(any( X[,i]<0 | X[,i]>1)){
      X[,i] <- (X[,i]-min(X[,i]))/(max(X[,i])-min(X[,i]))
    }
  }
  
  ## Get Gram Matrix
  if(missing(Gram)){
    Gram <- get.gram(X, X, order=order, cat.pos=cat.pos)
  }
  K.theta <- matrix(0,n,n)
  for(i in 1:P){
    K.theta <- K.theta + w.0[i]^2*Gram[[i]]
  }
  lambda.min <- lambda.lim[1]
  lambda.max <- lambda.lim[2]
  log.lambda.best <- .01
  LAMBDAMIN <- log(lambda.min)
  LAMBDAMAX <- log(lambda.max)
  log.lambda.min <- LAMBDAMIN
  log.lambda.max <- LAMBDAMAX
  inc <- (log.lambda.max - log.lambda.min)/(nlambda-1)
  log.lambda.vec <- seq(log.lambda.min, log.lambda.max, inc)
  log.lambda.old <- log.lambda.vec
  gcv.best <- Inf
  
  repeat{
    nlambda.now <- length(log.lambda.vec)
    df.vec <- rep(0, length(log.lambda.vec))
    for(j in 1:nlambda.now){
      llambda <- log.lambda.vec[j]
      lambda <- exp(llambda)
      
      #K.theta..<<-K.theta
      #lambda..<<-lambda
      
      K.inv <- solve(K.theta + lambda*diag(n), tol=1E-25)
      J <- rep(1,n)
      alpha <- sum(K.inv)^(-1)*t(J)%*%K.inv
      mu.hat <- as.numeric(alpha%*%y)
      c.hat <- K.inv%*%(y-J*mu.hat)
      H <- K.theta%*%K.inv%*%(diag(n)-J%*%alpha)+J%*%alpha
      
      df <- sum(diag(H))
      df.vec[j] <- df
      y.hat <- mu.hat + K.theta%*%c.hat
      res <- y-y.hat
      SSE <- sum(res^2)
      if(gcv.pen*df >= n)
        gcv <- Inf
      else
        gcv <- SSE/(1-gcv.pen*df/n)^2
      
      #cat("\nlambda =",lambda)
      #cat("    df =",df)
      #cat("    gcv =",gcv)
      #cat("    gcv.best =",gcv.best)
      
      if(gcv < gcv.best && df>=df.lim[1] && df<=df.lim[2]){
        gcv.best <- gcv     
        c.best <- c.hat
        df.best <- df
        log.lambda.best <- llambda
        lambda.best <- exp(log.lambda.best)
        mu.best <- mu.hat
      }
    }
    
    if(gcv.best==Inf){
      df.err <- (df.vec-df.lim[1])^2+(df.vec-df.lim[2])^2
      ind.j <- order(df.err)[1]
      df.best <- df.vec[ind.j]
      log.lambda.best <- log.lambda.vec[ind.j]
      lambda.best <- exp(log.lambda.best)
    }
    
    diff <- exp(log.lambda.best+inc) - lambda.best
    if(diff/lambda.best <= rel.tol)
      break
    
    ## create log.lambda.vec for next pass
    log.lambda.min <- log.lambda.best - floor(nlambda/2)/2*inc
    log.lambda.min <- max(LAMBDAMIN, log.lambda.min)  
    log.lambda.max <- log.lambda.best + floor(nlambda/2)/2*inc
    log.lambda.max <- min(LAMBDAMAX, log.lambda.max)  
    inc <- inc/2
    log.lambda.vec <- seq(log.lambda.min, log.lambda.max, inc) 
    ind <- which.equal(log.lambda.vec, log.lambda.old)
    log.lambda.vec <- log.lambda.vec[!ind]
    log.lambda.old <- c(log.lambda.old, log.lambda.vec)
  }
  
  ## Now get weights w
  L2.norm <- numeric(P)
  H2.norm <- numeric(P)
  y.mat <- matrix(0,n,P)
  for(i in 1:P){
    y.mat[,i] <- w.0[i]^2*Gram[[i]]%*%c.hat
    H2.norm[i] <- sqrt(t(c.best)%*%(w.0[i]^4*Gram[[i]])%*%c.best)
    L2.norm[i] <- sqrt(mean((y.mat[,i])^2))
  }
  
  #L2.norm..<<-L2.norm
  #H2.norm..<<-H2.norm
  
  if(w[1]=='L2'){
    w <- L2.norm^wt.pow
  }
  else if(w[1]=='RKHS'){
    w <- H2.norm^wt.pow
  }
  
  return(list(lambda=lambda.best, gcv=gcv.best, df=df.best, w=w, 
              Gram=Gram, y.mat=y.mat, X=X, y=y, c.hat=c.best, mu.hat=mu.best))
}



#######################################################################
############ GET LAMBDA_0 VIA FULL SMOOTHING SPLINE ###################
#######################################################################

get.lambda0 <- function(X, y, order, lambda.lim, nlambda, gcv.pen, rel.tol, 
                        w, Gram, df.lim, cat.pos){
  
  n <- nrow(X)
  p <- ncol(X)
  if(order==1)
    P <- p
  else
    P <- p + choose(p,2)  
  
  if(missing(df.lim)){
    df.lim <- c(0,.5*n)
  }
  
  if(length(w)==1)
    w <- rep(w,P)
  
  ## shift and rescale x's to [0,1]
  for(i in 1:p){
    if(any( X[,i]<0 | X[,i]>1)){
      X[,i] <- (X[,i]-min(X[,i]))/(max(X[,i])-min(X[,i]))
    }
  }
  
  ## Get Gram Matrix
  if(missing(Gram)){
    Gram <- get.gram(X, X, order=order, cat.pos=cat.pos)
  }
  K.theta <- matrix(0,n,n)
  for(i in 1:P){
    K.theta <- K.theta + w[i]^2*Gram[[i]]
  }
  
  lambda.min <- lambda.lim[1]
  lambda.max <- lambda.lim[2]
  log.lambda.best <- .01
  LAMBDAMIN <- log(lambda.min)
  LAMBDAMAX <- log(lambda.max)
  log.lambda.min <- LAMBDAMIN
  log.lambda.max <- LAMBDAMAX
  inc <- (log.lambda.max - log.lambda.min)/(nlambda-1)
  log.lambda.vec <- seq(log.lambda.min, log.lambda.max, inc)
  log.lambda.old <- log.lambda.vec
  gcv.best <- Inf
  
  repeat{
    nlambda.now <- length(log.lambda.vec)
    df.vec <- rep(0, length(log.lambda.vec))
    for(j in 1:nlambda.now){
      llambda <- log.lambda.vec[j]
      lambda <- exp(llambda)
      
      #K.theta..<<-K.theta
      #lambda..<<-lambda
      
      K.inv <- solve(K.theta + lambda*diag(n), tol=1E-25)
      J <- rep(1,n)
      alpha <- sum(K.inv)^(-1)*t(J)%*%K.inv
      mu.hat <- as.numeric(alpha%*%y)
      c.hat <- K.inv%*%(y-J*mu.hat)
      H <- K.theta%*%K.inv%*%(diag(n)-J%*%alpha)+J%*%alpha
      
      df <- sum(diag(H))
      df.vec[j] <- df
      y.hat <- mu.hat + K.theta%*%c.hat
      res <- y-y.hat
      SSE <- sum(res^2)
      if(gcv.pen*df >= n)
        gcv <- Inf
      else
        gcv <- SSE/(1-gcv.pen*df/n)^2
      
      
      #cat("\nlambda =",lambda)
      #cat("    df =",df)
      #cat("    gcv =",gcv)
      #cat("    gcv.best =",gcv.best)
      #cat("\nw =")
      #print(w)
      
      if(gcv < gcv.best && df>=df.lim[1] && df<=df.lim[2]){
        gcv.best <- gcv     
        c.best <- c.hat
        log.lambda.best <- llambda
        lambda.best <- exp(log.lambda.best)
      }
    }
    
    if(gcv.best==Inf){
      df.err <- (df.vec-df.lim[1])^2+(df.vec-df.lim[2])^2
      ind.j <- order(df.err)[1]
      log.lambda.best <- log.lambda.vec[ind.j]
      lambda.best <- exp(log.lambda.best)
    }
    
    diff <- exp(log.lambda.best+inc) - lambda.best
    #cat("\ndiff =",diff)
    #cat("\nlambda.best =",lambda.best)
    
    if(diff/lambda.best <= rel.tol)
      break
    
    ## create log.lambda.vec for next pass
    log.lambda.min <- log.lambda.best - floor(nlambda/2)/2*inc
    log.lambda.min <- max(LAMBDAMIN, log.lambda.min)  
    log.lambda.max <- log.lambda.best + floor(nlambda/2)/2*inc
    log.lambda.max <- min(LAMBDAMAX, log.lambda.max)  
    inc <- inc/2
    log.lambda.vec <- seq(log.lambda.min, log.lambda.max, inc) 
    ind <- which.equal(log.lambda.vec, log.lambda.old)
    log.lambda.vec <- log.lambda.vec[!ind]
    log.lambda.old <- c(log.lambda.old, log.lambda.vec)
  }
  
  ## Now get norm and y.mat
  norm <- numeric(P)
  y.mat <- matrix(0,n,P)
  for(i in 1:P){
    y.mat[,i] <- w[i]^2*Gram[[i]]%*%c.hat
    norm[i] <- t(c.hat)%*%(w[i]^4*Gram[[i]])%*%c.hat
  }
  return(list(lambda=lambda.best, gcv=gcv.best, w=w, Gram=Gram, 
              y.mat=y.mat, X=X, y=y, norm=norm))
}




#######################################################################
######## GET INITIAL WEIGHTS VIA FULL SMOOTHING SPLINE ################
#######################################################################

get.lambda.w <- function(X, y, order, lambda.lim, nlambda, w='L2', 
                         gcv.pen, rel.tol, wt.pow, w.0, w.lim=c(1E-10, 1E10),
                         f.est='trad', rel.K, rel.lambda, rel.theta, maxit, 
                         nK, seed, cv, init.cv='gcv', cat.pos, min.distinct){
  
  if(f.est=='trad'){
    ans1 <- get.w(X=X, y=y, order=order, lambda.lim=lambda.lim, 
                  nlambda=nlambda, gcv.pen=gcv.pen, wt.pow=wt.pow, 
                  rel.tol=rel.tol, w=w, w.lim=w.lim, cat.pos=cat.pos)
    ans1$Gram <- NULL
    w <- ans1$w
    
  }
  
  else{ #f.est = 'cosso'
    ans1 <- venus.cv(X, y, order=order, w=1, wt.pow=0, seed=seed, K.lim='data',
                     nK=nK, gcv.pen=gcv.pen, rel.K=rel.K, nlambda=nlambda, 
                     lambda.lim=lambda.lim, rel.lambda=rel.lambda, 
                     rel.theta=rel.theta, maxit=maxit, cv=init.cv,
                     cat.pos=cat.pos, min.distinct=min.distinct)
    ans1$Gram <- NULL
    fit.venus <- venus(X, y, order=order, K=ans1$K, gcv.pen=gcv.pen, maxit=5, 
                       rel.tol=1E-2, cv='gcv', lambda.0=ans1$lambda.0, 
                       w=ans1$w, cat.pos=cat.pos, min.distinct=min.distinct)
    ans.norm <- get.venus.norms(fit.venus)
    
    if(w[1]=='L2'){
      w <- ans.norm$L2.norm^wt.pow
    }
    else if(w[1]=='RKHS'){
      w <- ans.norm$H2.norm^wt.pow
    }
  }
  
  ans2 <- get.lambda0(X=X, y=y, order=order, lambda.lim=lambda.lim, 
                      nlambda=nlambda, gcv.pen=gcv.pen, rel.tol=rel.tol, w=w,
                      cat.pos=cat.pos)
  lambda <- ans2$lambda
  norm <- ans2$norm
  y.mat <- ans2$y.mat
  w <- ans2$w
  
  return(list(lambda=lambda, norm=norm, X=X, y=y, Gram=ans2$Gram, y.mat=y.mat, 
              w=w))
}




#######################################################################
################### CROSS VALIDATE ON K ###############################
#######################################################################


venus.cv <- function(X, y, order=1, w='L2', K.lim='data', nK=5, gcv.pen, 
                     rel.K=1E-2, nlambda=10, lambda.lim=c(1E-10, 1E2), rel.lambda=1E-2, 
                     theta.0, rel.theta=1E-2, maxit=2, cv='gcv', wt.pow=1, seed=220, 
                     w.lim=c(1E-12, 1E10), lambda.0='est', Gram, f.est='trad', 
                     init.cv='gcv', cat.pos="auto", min.distinct=7){
  
  n <- nrow(X)
  p <- ncol(X)
  if(order==1)
    P <- p
  else
    P <- p + choose(p,2)  
  
  if(missing(theta.0))
    theta.0 <- rep(1,P)
  theta.00 <- theta.0
  
  ## Identify categorical vars 
  if(length(cat.pos)>0 && cat.pos[1]=='auto'){
    # scan for variables with 5 or less distinct values. 
    cat.pos <- numeric(0)
    for(i in 1:p){
      unique.i <- unique(X[,i])
      if(length(unique.i)<=min.distinct || is.character(X[,i])){
        cat.pos <- c(cat.pos, i)
      }
    }
  }
  
  ## First get lambda.0 
  if(lambda.0=='est' || missing(Gram)){
    ans.lambda0 <- get.lambda.w(X=X, y=y, order=order, lambda.lim=lambda.lim, 
                                nlambda=nlambda, w=w, gcv.pen=gcv.pen, rel.tol=rel.lambda, 
                                wt.pow=wt.pow, w.0=1, w.lim=w.lim, f.est=f.est, rel.K=rel.K,
                                rel.lambda=rel.lambda, rel.theta=rel.theta, maxit=maxit,
                                nK=nK, seed=seed, cv=cv, init.cv=init.cv, cat.pos=cat.pos,
                                min.distinct=min.distinct)
    w <- ans.lambda0$w
    
    ### Changed this for numerical stability
    #    lambda.0 <- ans.lambda0$lambda*1E-3
    lambda.0 <- ans.lambda0$lambda*1E0
    #######################################
    Gram <- ans.lambda0$Gram
    ans.lambda0$Gram <- NULL
    norm <- ans.lambda0$norm
  }
  
  if(K.lim[1]=='data'){
    K.lim <- c(0, 10*P)
  }
  
  #lambda.0..<<-lambda.0
  #w..<<-w
  #cat("\n lambda.0 =", lambda.0)
  #cat("\n w =")
  #print(w)
  
  
  ## Now conduct grid search on K
  K.min <- K.lim[1]
  K.max <- K.lim[2]
  KMIN <- log10(K.min+1)
  KMAX <- log10(K.max+1)
  log.K.min <- KMIN
  log.K.max <- KMAX
  
  inc <- (log.K.max - log.K.min)/(nK-1)
  log.K.vec <- seq(log.K.min, log.K.max, inc)
  log.K.old <- log.K.vec
  pen.best <- Inf
  
  repeat{
    nK.now <- length(log.K.vec)
    theta.0 <- theta.00
    for(j in 1:nK.now){
      K <- 10^(log.K.vec[j])-1
      #cat("\n K =",K)
      fit.venus <- venus(X=X,y=y,order=order, K=K,gcv.pen=gcv.pen, 
                         lambda.0=lambda.0, theta.0=theta.0,rel.tol=rel.theta, 
                         maxit=maxit, Gram=Gram, cv=cv, w=w, seed=seed,
                         cat.pos=cat.pos, min.distinct=min.distinct)
      fit.venus$Gram <- NULL
      if(cv=='bic')
        pen <- fit.venus$bic
      
      if(cv=='gcv')
        pen <- fit.venus$gcv
      
      if (cv=="loo")
        pen <- -fit.venus$Q2loo # Q2loo must be maximized
      
      #cat("   pen =",pen)
      
      #theta.0 <- fit.venus$theta
      
      if(pen < pen.best){
        pen.best <- pen     
        K.best <- K
        log.K.best <- log.K.vec[j]
        theta.best <- fit.venus$theta
        
        #theta.00 <- theta.0
        
      }
    }
    diff <- 10^(log.K.best+inc)-1 - K.best
    if(diff/(K.best+1E-6) <= rel.K)
      break
    
    ## create K.vec for next pass
    log.K.min <- log.K.best - floor(nK/2)/2*inc
    log.K.min <- max(KMIN, log.K.min)  
    log.K.max <- log.K.best + floor(nK/2)/2*inc
    log.K.max <- min(KMAX, log.K.max)  
    inc <- inc/2
    log.K.vec <- seq(log.K.min, log.K.max, inc) 
    ind <- which.equal(log.K.vec, log.K.old)
    log.K.vec <- log.K.vec[!ind]
    log.K.old <- c(log.K.old, log.K.vec)
  }
  
  return(list(K=K.best, lambda.0=lambda.0, theta=theta.best, Gram=Gram,
              pen=pen.best, w=w, cat.pos=cat.pos))
}





#######################################################################
################# CROSS VALIDATE ON K and wt.pow ######################
#######################################################################


venus.cv2 <- function(X, y, order=1, w='L2', K.lim='data', nK=5, gcv.pen, 
                      rel.K=1E-2, nlambda=10, lambda.lim=c(1E-10, 1E2), rel.lambda=1E-2, 
                      theta.0, rel.theta=1E-2, maxit=2, cv='gcv', wt.pow.vec=c(.5,1,2), 
                      seed=220, w.lim=c(1E-12, 1E10), lambda.0='est', Gram, 
                      f.est='trad', init.cv='gcv'){
  
  pen.best <- Inf
  for(wt.pow in wt.pow.vec){
    if(wt.pow==0){
      f.est.now <- 'trad'
      init.cv.now <- 'gcv'
    }
    else{
      f.est.now <- f.est
      init.cv.now <- init.cv
    }
    
    ans.now <- venus.cv(X=X, y=y, order=order, w=w, K.lim=K.lim, nK=nK, 
                        gcv.pen=gcv.pen, rel.K=rel.K, nlambda=nlambda, 
                        lambda.lim=lambda.lim, rel.lambda=rel.lambda, theta.0=theta.0,
                        rel.theta=rel.theta, maxit=maxit, cv=cv, wt.pow=wt.pow, 
                        seed=seed, w.lim=w.lim, lambda.0=lambda.0, Gram=Gram, 
                        f.est=f.est.now, init.cv=init.cv.now)
    
    cat("\n wt.pow =",wt.pow, "     pen =",ans.now$pen) 
    
    if(ans.now$pen < pen.best){
      ans.best <- ans.now
      pen.best <- ans.now$pen
      wt.pow.best <- wt.pow
    }
  }
  
  print(wt.pow.best) 
  
  return(ans.best)
}


#######################################################################
########### CROSS VALIDATE FOR TRAD SMOOTHING SPLINE ##################
#######################################################################


pen.spline.cv <- function(X, y, order=1, gcv.pen=1.01, nlambda=5,
                          lambda.lim=c(1E-8,1E4), theta, df.lim,
                          rel.lambda=1E-2, cv='gcv', seed=220){
  
  n <- nrow(X)
  p <- ncol(X)
  if(order==1)
    P <- p
  else
    P <- p + choose(p,2)
  
  if(missing(df.lim)){
    df.lim <- c(0,.75*n)
  }
  
  if(missing(theta))
    theta <- rep(1,P)
  
  ## shift and rescale x's to [0,1]
  for(i in 1:p){
    if(any( X[,i]<0 | X[,i]>1)){
      X[,i] <- (X[,i]-min(X[,i]))/(max(X[,i])-min(X[,i]))
    }
  }
  
  ## Get Gram Matrix
  Gram <- get.gram(X, X, order=order, cat.pos=numeric(0))
  
  ## Now conduct grid search on lambda
  lambda.min <- lambda.lim[1]
  lambda.max <- lambda.lim[2]
  lambdaMIN <- log10(lambda.min+1)
  lambdaMAX <- log10(lambda.max+1)
  log.lambda.min <- lambdaMIN
  log.lambda.max <- lambdaMAX
  
  inc <- (log.lambda.max - log.lambda.min)/(nlambda-1)
  log.lambda.vec <- seq(log.lambda.min, log.lambda.max, inc)
  log.lambda.old <- log.lambda.vec
  pen.best <- Inf
  
  repeat{
    nlambda.now <- length(log.lambda.vec)
    df.vec <- rep(0, length(log.lambda.vec))
    for(j in 1:nlambda.now){
      lambda <- 10^(log.lambda.vec[j])-1
      #cat("\n lambda =",lambda)
      
      fit.spline<- pen.spline(X, y, order=order, lambda.0=lambda, theta=theta, 
                              seed=seed, gcv.pen=gcv.pen, Gram=Gram, cv=cv)
      if(cv=='bic')
        pen <- fit.spline$bic
      else
        pen <- fit.spline$gcv
      
      #cat("   df =", fit.spline$df, "   pen =",pen)
      
      df.vec[j] <- df <- fit.spline$df
      
      if(pen < pen.best && df>=df.lim[1] && df<=df.lim[2]){
        pen.best <- pen     
        lambda.best <- lambda
        log.lambda.best <- log.lambda.vec[j]
        theta.best <- fit.spline$theta
        
        #theta.00 <- theta.0
        
      }
    }
    
    if(pen.best==Inf){
      df.err <- (df.vec-df.lim[1])^2+(df.vec-df.lim[2])^2
      ind.j <- order(df.err)[1]
      log.lambda.best <- log.lambda.vec[ind.j]
      lambda.best <- exp(log.lambda.best)
    }
    
    diff <- 10^(log.lambda.best+inc)-1 - lambda.best
    if(diff/(lambda.best+1E-6) <= rel.lambda)
      break
    
    ## create lambda.vec for next pass
    log.lambda.min <- log.lambda.best - floor(nlambda/2)/2*inc
    log.lambda.min <- max(lambdaMIN, log.lambda.min)  
    log.lambda.max <- log.lambda.best + floor(nlambda/2)/2*inc
    log.lambda.max <- min(lambdaMAX, log.lambda.max)  
    inc <- inc/2
    log.lambda.vec <- seq(log.lambda.min, log.lambda.max, inc) 
    ind <- which.equal(log.lambda.vec, log.lambda.old)
    log.lambda.vec <- log.lambda.vec[!ind]
    log.lambda.old <- c(log.lambda.old, log.lambda.vec)
  }
  
  return(list(lambda=lambda.best, Gram=Gram, pen=pen.best))
}




#######################################################################
################### REWRITE QP.solve to change error to warning #######
#######################################################################



solve.QP2 <- function (Dmat, dvec, Amat, bvec, meq = 0, factorized = FALSE)
{
  n <- nrow(Dmat)
  q <- ncol(Amat)
  if (missing(bvec))
    bvec <- rep(0, q)
  if (n != ncol(Dmat))
    stop("Dmat is not symmetric!")
  if (n != length(dvec))
    stop("Dmat and dvec are incompatible!")
  if (n != nrow(Amat))
    stop("Amat and dvec are incompatible!")
  if (q != length(bvec))
    stop("Amat and bvec are incompatible!")
  if ((meq > q) || (meq < 0))
    stop("Value of meq is invalid!")
  iact <- rep(0, q)
  nact <- 0
  r <- min(n, q)
  sol <- rep(0, n)
  crval <- 0
  work <- rep(0, 2 * n + r * (r + 5)/2 + 2 * q + 1)
  iter <- rep(0, 2)
  res1 <- .Fortran("qpgen2", as.double(Dmat), dvec = as.double(dvec),
                   as.integer(n), as.integer(n), sol = as.double(sol), crval = as.double(crval),
                   as.double(Amat), as.double(bvec), as.integer(n), as.integer(q),
                   as.integer(meq), iact = as.integer(iact), nact = as.integer(nact),
                   iter = as.integer(iter), work = as.double(work), ierr = as.integer(factorized),
                   PACKAGE = "quadprog")
  if (res1$ierr == 1){
    #warning("constraints are inconsistent, no solution!")
    res1$sol <- rep(-1,n)
  }
  else if (res1$ierr == 2){
    #warning("matrix D in quadratic function is not positive definite!")
    res1$sol <- rep(-2,n)
  }
  list(solution = res1$sol, value = res1$crval, unconstrainted.solution = res1$dvec, iterations = res1$iter, iact = res1$iact[1:res1$nact])
}










svd2 <- function (x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE)
{
  x <- as.matrix(x)
  if (any(!is.finite(x)))
    stop("infinite or missing values in 'x'")
  dx <- dim(x)
  n <- dx[1]
  p <- dx[2]
  if (!n || !p)
    stop("0 extent dimensions")
  if (is.complex(x)) {
    res <- La.svd2(x, nu, nv)
    return(list(d = res$d, u = if (nu) res$u, v = if (nv) Conj(t(res$vt))))
  }
  if (!LINPACK) {
    res <- La.svd2(x, nu, nv, method = "dgesvd")
    return(list(d = res$d, u = if (nu) res$u, v = if (nv) t(res$vt)))
  }
  if (!is.numeric(x))
    stop("argument to 'svd' must be numeric")
  if (nu == 0) {
    job <- 0
    u <- double(0)
  }
  else if (nu == n) {
    job <- 10
    u <- matrix(0, n, n)
  }
  else if (nu == p) {
    job <- 20
    u <- matrix(0, n, p)
  }
  else stop("'nu' must be 0, nrow(x) or ncol(x)")
  job <- job + if (nv == 0)
    0
  else if (nv == p || nv == n)
    1
  else stop("'nv' must be 0 or ncol(x)")
  v <- if (job == 0)
    double(0)
  else matrix(0, p, p)
  mn <- min(n, p)
  mm <- min(n + 1, p)
  z <- .Fortran("dsvdc", as.double(x), n, n, p, d = double(mm),
                double(p), u = u, n, v = v, p, double(n), as.integer(job),
                info = integer(1), DUP = FALSE, PACKAGE = "base")[c("d",
                                                                    "u", "v", "info")]
  if (z$info){
    stop(gettextf("error %d in 'dsvdc'", z$info), domain = NA)
  }
  z$d <- z$d[1:mn]
  if (nv && nv < p)
    z$v <- z$v[, 1:nv, drop = FALSE]
  z[c("d", if (nu) "u", if (nv) "v")]
}



La.svd2 <- function (x, nu = min(n, p), nv = min(n, p), method = c("dgesdd",
                                                                   "dgesvd"))
{
  if (!is.numeric(x) && !is.complex(x))
    stop("argument to 'La.svd' must be numeric or complex")
  if (any(!is.finite(x)))
    stop("infinite or missing values in 'x'")
  method <- match.arg(method)
  #    if (is.numeric(x) && method == "dgesvd")
  #        .Deprecated("La.svd(method = \"dgesvd\")")
  x <- as.matrix(x)
  if (is.numeric(x))
    storage.mode(x) <- "double"
  n <- nrow(x)
  p <- ncol(x)
  if (!n || !p)
    stop("0 extent dimensions")
  if (is.complex(x) || method == "dgesvd") {
    if (nu == 0) {
      jobu <- "N"
      u <- matrix(0, 1, 1)
    }
    else if (nu == n) {
      jobu <- ifelse(n > p, "A", "S")
      u <- matrix(0, n, n)
    }
    else if (nu == p) {
      jobu <- ifelse(n > p, "S", "A")
      u <- matrix(0, n, p)
    }
    else stop("'nu' must be 0, nrow(x) or ncol(x)")
    if (nv == 0) {
      jobv <- "N"
      v <- matrix(0, 1, 1)
    }
    else if (nv == n) {
      jobv <- ifelse(n > p, "A", "S")
      v <- matrix(0, min(n, p), p)
    }
    else if (nv == p) {
      jobv <- ifelse(n > p, "S", "A")
      v <- matrix(0, p, p)
    }
    else stop("'nv' must be 0, nrow(x) or ncol(x)")
    if (is.complex(x)) {
      u[] <- as.complex(u)
      v[] <- as.complex(v)
      res <- .Call("La_svd_cmplx", jobu, jobv, x, double(min(n,
                                                             p)), u, v, PACKAGE = "base")
    }
    else {
      res <- .Call("La_svd", jobu, jobv, x, double(min(n,
                                                       p)), u, v, method, PACKAGE = "base")
    }
    return(res[c("d", if (nu) "u", if (nv) "vt")])
  }
  else {
    if (nu > 0 || nv > 0) {
      np <- min(n, p)
      if (nu <= np && nv <= np) {
        jobu <- "S"
        u <- matrix(0, n, np)
        v <- matrix(0, np, p)
      }
      else {
        jobu <- "A"
        u <- matrix(0, n, n)
        v <- matrix(0, p, p)
      }
    }
    else {
      jobu <- "N"
      u <- matrix(0, 1, 1)
      v <- matrix(0, 1, 1)
    }
    jobv <- ""
    res <- .Call("La_svd", jobu, jobv, x, double(min(n, p)),
                 u, v, method, PACKAGE = "base")
    res <- res[c("d", if (nu) "u", if (nv) "vt")]
    if (nu)
      res$u <- res$u[, 1:min(n, nu), drop = FALSE]
    if (nv)
      res$vt <- res$vt[1:min(p, nv), , drop = FALSE]
    return(res)
  }
}


