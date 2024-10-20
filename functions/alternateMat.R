alternateMat <- function(rows, cols, AR) {
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
    if  (j == (i*2-1)) {   # logic of matching 1,2,3,4 rows with 1,3,5,7 cols   
        outMat[i, j] = 1
    }
  }
}
        
# example customized matrix 4x8
# [[1 0 0 0 0 0 0 0 ];              
# [0 0 1 0 0 0 0 0 ];
# [0 0 0 0 1 0 0 0 ];
# [0 0 0 0 0 0 1 0 ]];

return(outMat)

}