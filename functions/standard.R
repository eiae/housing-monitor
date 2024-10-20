standard <- function(data){
## Standardize variables
# inputs:
# - data to standardize
# outputs:
# - data demeaned and divided by its standard deviation
# -------------------------------------------------------------------------
  
# specs
dataStandard <- data  # preallocate output
N <- dim(data)[2]  # number of variables to iterate over
T <- dim(data)[1]  # number of periods to iterate over

# compute moments
j <- 1  # init iterator

while ( j <= N ) {              
  dataj <- data[,j]  # jth variable extraction
  dataj <- dataj[!is.na(dataj)]  # select non-missing data only 
  datajm <- mean(dataj)  # mean
  datajst <- sd(dataj)  # std
 
  # standardize
  i <- 1  # init iterator                     
  
  while( i <= T ) {         
    
    # in each period subtract mean and divide by standard deviation
    if ( !is.na(data[i,j]) ) {  # if not NA/nan
      dataStandard[i,j] <- (data[i,j]-datajm) / datajst  
    }  
  
    i <- i+1  # update iteration
  
  }
  
  j <- j+1  # update iteration
}
  
return(dataStandard)
}