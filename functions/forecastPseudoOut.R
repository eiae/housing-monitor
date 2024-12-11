forecastPseudoOut <- function(trainEnd, h, data, vfact, m2q, latentlen, Q, M, L, AR, startval) {
  # Define training and test sets
  # train <- window(data, end = c(floor((trainEnd - 1) / 12), (trainEnd - 1) %% 12 + 1))
  # test <- window(data, start = c(floor(trainEnd / 12), trainEnd %% 12 + 1), 
  #                end = c(floor((trainEnd + h - 1) / 12), (trainEnd + h - 1) %% 12 + 1))
  
  # subset data
  train <- data[1:trainEnd, ]
  test <- data[(trainEnd + 1):(trainEnd + h), 1]  # take only target variable
  
  # compute moments for standardization
  trainMean <- apply(train, 2, function(col) mean(col, na.rm = TRUE))
  trainSd <- apply(train, 2, function(col) sd(col, na.rm = TRUE))
  
  # standardize data
  trainStandard <- scale(train, center = trainMean, scale = trainSd)
  
  # expand train with NaN for forecast horizon
  nanforecast <- matrix(NaN, nrow = h, ncol = ncol(train))
  trainStandard <- rbind(trainStandard, nanforecast)
  
  # fill missing values with random data
  trainFilled <- filler(trainStandard, 1)
  
  # indicator for filled values
  index <- !is.na(trainStandard)
  index <- ifelse(index, 1, 0)
  
  # dimensions
  N <- dim(trainFilled)[2]
  T <- dim(trainFilled)[1]  # define dimensions here to have forecast periods included
  
  # estimate optimal param with maximum likelihood
  maxLike <- optim(par = startval, fn = kalmanhood, N = N, vfact = vfact, m2q = m2q, 
                   data = trainFilled, T = T, index = index, latentlen = latentlen, 
                   Q = Q, M = M, L = L, AR = AR, method = "Nelder-Mead", hessian = TRUE)
  
  coeff <- maxLike$par 
  cramerrao <- solve(maxLike$hessian)
  se <- sqrt(abs(diag(cramerrao)))
  
  # use optimal param values to get state vector
  latentfilled <- kalman(coeff, N, vfact, m2q, trainFilled, T, index, latentlen, Q, M, L, AR)
  
  # states and refilled data using measurement equation
  latent <- latentfilled$latent
  filled <- latentfilled$dataReFilled
  filledCommon <- latentfilled$dataReFilledCommon
  
  # add mean and std back in data vector since standardized at the beginning
  for (i in 1:N) {
    filled[,i] <- filled[,i]*trainSd[i] + trainMean[i]
    filledCommon[,i] <- filledCommon[,i]*trainSd[i] + trainMean[i]
  }
  
  # get forecast of target
  yPredicted <- filled[,1]  # predicted target (filled using transition equation in ss)
  yPredictedCommon <- filledCommon[,1]  # predicted target (filled using common part of transition equation in ss)
  
  # calculate RMSE
  rmse <- sqrt(mean((test[h] - yPredicted[trainEnd+h])^2, na.rm = TRUE))
  
  return(list(rmse = rmse, estimatedCoeff = coeff, estimatedSe = se, trainForecast = yPredicted, test = test))
}