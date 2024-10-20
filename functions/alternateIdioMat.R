alternateIdioMat <- function(rows, cols, AR, pamz) {
## Build customized matrix with alternating columns
# inputs:
# - number of rows and cols
# - number of autoregressive lags for jumps in cols of state vector (get
# only the contemporaneous elements)
# outputs:
# - customized matrix
# -------------------------------------------------------------------------  
outMat <- matrix(0, nrow=rows, ncol=cols)

for (i in 1:rows) {
  for (j in seq(1, cols, by=AR)) {
    if  (j == i & i%%2==1) {   # logic of matching diagonal elements only in odd rows
      outMat[i, j:(j+AR-1)] <- pamz[j:(j+AR-1)]  # put range of autoregressive coeff for idio terms
      outMat[i+1, j] <- 1  # put 1 in even rows and odd cols, (2,1), (4,3), (6,5) etc
    }
  }
}

# example customized matrix 8x8
# [[psi11 psi12 0     0     0     0     0     0 ];              
# [ 1     0     0     0     0     0     0     0 ];
# [ 0     0     psi21 psi22 0     0     0     0 ];
# [ 0     0     1     0     0     0     0     0 ];
# [ 0     0     0     0     psi31 psi32 0     0 ];
# [ 0     0     0     0     1     0     0     0 ];
# [ 0     0     0     0     0     0     psi41 psi42 ];
# [ 0     0     0     0     0     0     1     0 ]];

return(outMat)

}