# Functions to accompany the Compound Poisson Application
# Douglas McLean
# 26/2/16


# Density function
dcp <- function(y,k,theta,lambda,ntrunc=25){
  if( any(y<0)  ) return(NA)
  if( all(y>=0) ){
    f <- rep(0,length(y))
    for( n in 1:ntrunc ){
      f <- f + dgamma(x=y,shape=n*k,scale=theta) * dpois(x=n,lambda=lambda)
    }
    f[y==0] <- 1e6 #Inf
    return(f)
  }
  return(NULL)
}


# The distribution function implementation is also a straight forward implementation of (\ref{distribution}):
pcp <- function(y,k,theta,lambda,ntrunc=25){
  if( any(y<0 ) ) return(NA)
  if( all(y>=0) ){
    p <- dpois(x=0,lambda=lambda)
    for( n in 1:ntrunc ){
      p <- p + pgamma(q=y,shape=n*k,scale=theta) * dpois(x=n,lambda=lambda)
    }
    return(p)
  }
  return(NULL)
}



# Random number generation from compound Poisson variable
rcp <- function(n,lambda,k,theta,...){
  
  N <- rpois(n=n, lambda=lambda)
  
  K <- matrix(rgamma(n=n*max(N),shape=k,scale=theta),ncol=max(N))
  K <- cbind(0,K)
  M <- matrix(rep(1,n*(1+max(N))),ncol=1+max(N))
  M[col(M)>N+1] <- 0
  y <- apply(K*M,1,sum)
  
  return( y )
}


# Density function for the conditional Poisson event given a CP value y 
dce <- function(n,y,k,theta,lambda,ntrunc=25){
  if( y==0 ){
    return( c(1,rep(0,length(n)-1)) )
  } else {
    N <- 1:ntrunc
    f <- dgamma(x=y,shape=N*k,scale=theta) * dpois(x=N,lambda=lambda)
    f <- f/sum(f)
    return( c(0,f)[1+n] )
  }
}

# Random number generation from conditional Poisson event given a CP value y
rce <- function(n,y,k,theta,lambda,ntrunc=25){
  if( y==0 ){
    return( rep(0,n) )
  } else {
    N <- 1:ntrunc
    f <- dgamma(x=y,shape=N*k,scale=theta) * dpois(x=N,lambda=lambda)
    f <- cumsum(f)/sum(f)
    u <- matrix(runif(n),ncol=1)
    return( apply(u,1,function(x)1+sum(x>f)) )
  }
}
