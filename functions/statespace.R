statespace <- function(param, N, Q, M, S, L, AR, vfact, m2q) {
## Build state-space param matrices
# Input:
# - model params
# - variance of factor
# - month2quarter conversion vector
# - number of variables
# Output:
# - state-space coeff and innovation/error covariance matrices
# -------------------------------------------------------------------------
    
# automatic construction and preallocation of matrices

# H: coeff matrix obs equation
h1 <- c(m2q*param[Q], m2q, rep(0,AR*M))  # lambda/gamma with m2q map (loading); AR*M since enlarge state vector according to AR (since state eq is VAR(1) in companion form)
htemp <- alternateMat(M, AR*M, AR)
h2 <- cbind(param[(Q+1):(Q+M)], matrix(0,nrow=M,ncol=L+(L-1)), htemp)  # lambda/gamma (loading); L+(L-1) since you define first column of H' and then have 2*L (m2q for common and m2q for idio)
H <- rbind(h1,h2)      


# R: innovation covariance matrix obs equation
RR <- matrix(1,nrow=1,ncol=N)                   


# F: coeff matrix state equation
z1 <- param[(Q+M+1):(Q+M+AR)]  # phi (autoregressive of common)
z2 <- param[(Q+M+AR+1):(Q+M+AR*2)]  # psi of quarterly (autoregressive of idio)
pamz3 <- param[(Q+M+AR*2+1):(Q+M+AR*2+M*AR)] # psi of monthly (autoregressive of idio)
z3 <- alternateIdioMat(AR*M, AR*M, AR, pamz3)

f1 <- rbind(c(z1, rep(0,S-AR)), cbind(diag(L-1), matrix(0, nrow=L-1, ncol=S-(L-1))))                  
f2 <- rbind(c(rep(0,L), z2, rep(0,S-AR-L)), cbind(matrix(0, nrow=L-1, ncol=L), diag(L-1), matrix(0, nrow=L-1, ncol=S-AR*2-L)))
f3 <- cbind(matrix(0, nrow=AR*M, ncol=L*2), z3)
F <- rbind(f1, f2, f3)


# Q: innovation covariance matrix state equation
parmq1 <- param[(Q+M+AR*2+M*AR+1):length(param)]  # error variance of common and idio components
q1 <- rep(0,L)
q1[1] <- vfact
q2 <- rep(0,L)
q2[1] <- parmq1[1]
q3 <- rep(0,M*AR)

counter <- 1
for (j in 1:(M*AR)){
  if (j%%2 == 1) {
    q3[j] <- parmq1[counter+1]
    counter <- counter + 1
  }
}

q4 <- c(q1, q2, q3)
q4 <- q4^2  
QQ <- diag(q4)


return(list(R=RR, Q=QQ, H=H, F=F))

}