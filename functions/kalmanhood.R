kalmanhood <- function(param, N, vfact, m2q, data, T, index, latentlen, Q, M, L, AR) {
## Construct Kalman filter and likelihood function
# Inputs:
# - data, dimensions, filled obs indicator, model params, month2quarter conversion
# Outputs:
# - evaluated likelihood
# -------------------------------------------------------------------------

# build state-space param matrices
matrices <- statespace(param, N, Q, M, latentlen, L, AR, vfact, m2q)
H <- matrices$H
R <- matrices$R
F <- matrices$F
Q <- matrices$Q

# starting values for KF
beta00 <- rep(0,latentlen)  # init latent
P00 <- diag(latentlen)  # init latent uncertainty                      
like <- rep(0,T) # init likelihood
latent <- matrix(0, nrow=T, ncol=latentlen)  # preallocate filtered latent variable


## Kalman recursion with likelihood
it <- 1  # init KF iterator
while ( it<=T ) {  # finish when reach number of obs

  # modification step to select specific observations 
  # (1) multiply measurement coeff matrix with selector matrix (filled datapoints)
  # to set rows of measurement matrix to zero if missing observation
  Hit <- index[it,]*H   

  # (2) add measurement error vector with opposite selector matrix (done
  # through the measurement error covariance matrix) to set rows of
  # measurement error vector to zero if there is an observation
  tempRit <- c((1-index[it,])*R)  # define with <c()> to get an array 
  Rit <- diag(tempRit) 

  # measurement error covariance matrix has zeros in off-diagonal and has
  # diagonal elements filled with 1 if missing obs <=> (1-selector matrix)
  # with zero if there is an observation

  # prediction step
  beta10 <- F %*% beta00 
  P10 <- F %*% P00 %*% t(F) + Q
  
  # prediction error
  n10 <- data[it,] - Hit %*% beta10  # use modified measurement coeff matrix
  F10 <- Hit %*% P10 %*% t(Hit) + Rit  # use modified measurement error cov matrix
        
  # likelihood function (Gaussian)
  like[it] <- -0.5 * (log(2*pi*det(F10)) + (t(n10) %*% solve(F10) %*% n10))  # log

  # update step
  K <- P10 %*% t(Hit) %*% solve(F10)  # Kalman gain
  beta11 <- beta10 + K %*% n10
  P11 <- P10 - K %*% Hit %*% P10
  
  # latent/state variable
  latent[it,] <- beta11  # update after observe data
  
  # rename for next iteration
  beta00 <- beta11
  P00 <- P11      
  it <- it+1      

}
         
# likelihood evaluated at set of params with filtered latent
likelihood <- (-1)*sum(like)
       
return(likelihood)

}