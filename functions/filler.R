filler <- function(data, guess) {
## Fill missing observations
# inputs:
# - data with missing obs
# - guess for variance of standard normal
# outputs:
# - data with filled obs from draws of a normal distribution
# -------------------------------------------------------------------------
  
# specs
cd <- dim(data)[2]  # variables
rd <- dim(data)[1]  # periods

# random normal draws
dataFilled <- rnorm(n=rd*cd, mean=0, sd=1)  # vector of random standard normal draws
dataFilled <- matrix(dataFilled, nrow=rd, ncol=cd)  # reshape vector into matrix
dataFilled <- guess*dataFilled  # apply guess to matrix

# loop over variables and periods to replace missing obs with draws
j <- 1  # var counter

while ( j <= cd) {  # variables
  
  i <- 1  # period counter
  
  while ( i <= rd ) { # periods
  
  if ( !is.na(data[i,j]) ) {  # if not missing
      dataFilled[i,j] <- data[i,j]  # replace random obs with actual data (as consequence, you are replacing missing from original data with random draws)
    }
  
    i <- i+1
  
  }   

  j <- j+1

}

return(dataFilled)

}

