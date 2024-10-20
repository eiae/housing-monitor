## visual inspection

# lineplot
plot(dfCC$date, dfCC$obs, type="l", lwd=2, col="red", main="US nominal house prices", xlab="quarters", ylab="index")

# ACF and PACF
par(mfrow=c(1,2))
acf(dfCC$obs, lag.max = 10, lwd=2, main="ACF", na.action = na.pass) #, demean = TRUE,
pacf(dfCC$obs, lag.max = 10, lwd=2, main="PACF", na.action = na.pass) #, demean = TRUE,
par(mfrow=c(1,1)) # back to usual frame

# => trendy pattern in series and significant linear decay in ACF -> non-stationary series


## test for unit roots

# ADF test -> constant with linear trend
adfRes <- adf.test(dfCC$obs, k=8)  # 2years lags
print(adfRes)

# PP test -> constant with linear trend
ppRes <- pp.test(dfCC$obs)  # default lag truncation
print(ppRes)

# KPSS test -> constant (or trend) 
kpssRes <- kpss.test(dfCC$obs, null="Level")   # null="Trend"
print(kpssRes)

# => all tests provide empirical evidence of non-stationary series (not reject null in ADF, PP; reject null in KPSS)

## difference series (apply filter for stationarity) & test for unit roots again for order of integration
plot(dfCC$date, dfCC$qoq, type="l", lwd=2, col="blue", main="CC nominal house prices", xlab="quarters", ylab="qoq%")

adfResDiff <- adf.test(dfCC$qoq, k=8)  # 2years lags
print(adfResDiff)
ppResDiff <- pp.test(dfCC$qoq)  # default lag truncation
print(ppResDiff)
kpssResDiff <- kpss.test(dfCC$qoq, null="Level")   # null="Trend"
print(kpssResDiff)

# => after differencing once, all tests provide empirical evidence of stationarity -> the process ~ I(1) and the diff(process) ~ I(0)


## model ARMA on differenced (stationary) series (Box-Jenkins method)

# check ACF and PACF to determine AR and MA components
par(mfrow=c(1,2))
acf(dfCC$qoq, lag.max = 10, lwd=2, main="ACF") 
pacf(dfCC$qoq, lag.max = 10, lwd=2, main="PACF") 
par(mfrow=c(1,1)) # back to usual frame

# => ACF displays some exponential decay with significant autocorrelations up to 2 lags
# => PACF sudden drop after 1 significant lag
# => the results suggest the process follows an AR(1) process -> Box-Jenkins method: try suggested model, check diagnostics, fine-tune

# estimate AR(1) model
ar_model <- arima(dfCC$qoq, order=c(2,0,0), include.mean=FALSE)  # intercept not significant
print(ar_model)

# check significance of coefficients
arCoeff <- ar_model$coef
arSTE <- sqrt(diag(ar_model$var.coef))  # take srqt of diagonal elements of estimated var-cov matrix of ar coeff
tRatio <- abs(arCoeff / arSTE)  # 2-tailed test
critVal <- c(1.645, 1.96, 2.575)  # Normal distribution critical values at 90%, 95%, 99% confidence level

for (i in 1:length(tRatio)) {
  sprintf("Test for statistical significance for AR lag = %d", i)
  if (tRatio[i] > critVal[3]) {
    print("Empirical evidence to reject the H0: coeff=0 (non-significant coefficient) at 1% significance")
  } else if (tRatio[i] > critVal[2]) {
    print("Empirical evidence to reject the H0: coeff=0 (non-significant coefficient) at 5% significance")
  } else if (tRatio[i] > critVal[1]) {
    print("Empirical evidence to reject the H0: coeff=0 (non-significant coefficient) at 10% significance")
  } else { 
    print("No empirical evidence to reject the H0: coeff=0 (non-significant coefficient) at >10% significance") 
  }
}

# plot residuals along ACF of residuals (still some correlation left?)
par(mfrow=c(1,2))
plot(dfCC$date, ar_model$residuals, type="l", lwd=2, col="red", main="Residuals", xlab="quarters", ylab="values")
acf(ar_model$residuals, lag.max = 10, lwd=2, main="ACF")
par(mfrow=c(1,1))

# plot kernel of residuals along test Jarque-Bera normality test (Gaussianity assumption?)
residualPDF <- density(ar_model$residuals)
plot(residualPDF, main="Kernel density of residuals", lwd=2, col="red")

print(mean(ar_model$residuals))
print(sd(ar_model$residuals))
print(skewness(ar_model$residuals))
print(kurtosis(ar_model$residuals))

jbRes <- jarque.bera.test(ar_model$residuals)
print(jbRes)

# => evidence suggests that errors are NOT normally distributed -> empirical evidence to reject normality

# check fitness of suggested ARMA model
ar_fitted <- dfCC$qoq - ar_model$residuals  # build fitted values
dfCC <- cbind(dfCC, ar_fitted)
cor(dfCC$qoq, dfCC$ar_fitted)  # correlation between fitted and actual