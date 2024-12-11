# ==========================================================
## ARIMA Model (Box-Jenkins procedure)
# ==========================================================

## Data preparation
# ----------------------------------------------------------

## visual inspection

# plot of index in levels
plot(dfCC$Date, lvlTarget, type="l", lwd=2, col="black", main=paste0(ctryTarget," ",varTarget, " levels"), xlab="quarters", ylab="index")
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

# ACF and PACF
par(mfrow=c(1,2))
acf(lvlTarget, lag.max = 10, lwd=2, main="ACF", na.action = na.pass) #, demean = TRUE,
pacf(lvlTarget, lag.max = 10, lwd=2, main="PACF", na.action = na.pass) #, demean = TRUE,
par(mfrow=c(1,1)) # back to usual frame

# => trendy pattern in series and significant linear decay in ACF -> non-stationary series

## test for unit roots

# ADF test -> constant with linear trend
adfRes <- adf.test(lvlTarget, k=8)  # 2years lags
print(adfRes)

# PP test -> constant with linear trend
ppRes <- pp.test(lvlTarget)  # default lag truncation
print(ppRes)

# KPSS test -> constant (or trend) 
kpssRes <- kpss.test(lvlTarget, null="Level")   # null="Trend"
print(kpssRes)

# => all tests provide empirical evidence of non-stationary series (not reject null in ADF, PP; reject null in KPSS)

## used differenced series (apply filter for stationarity) & test for unit roots again for order of integration
plot(dfCC$Date, obsTarget, type="l", lwd=2, col="blue", main=paste0(ctryTarget," ",varTarget, " qoq%"), xlab="quarters", ylab="qoq%")
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

adfResDiff <- adf.test(obsTarget, k=8)  # 2years lags
print(adfResDiff)
ppResDiff <- pp.test(obsTarget)  # default lag truncation
print(ppResDiff)
kpssResDiff <- kpss.test(obsTarget, null="Level")   # null="Trend"
print(kpssResDiff)

# => after differencing once, all tests provide empirical evidence of stationarity -> the process ~ I(1) and the diff(process) ~ I(0)

## model ARMA on differenced (stationary) series 

# check ACF and PACF to determine AR and MA components
par(mfrow=c(1,2))
acf(obsTarget, lag.max = 10, lwd=2, main="ACF") 
pacf(obsTarget, lag.max = 10, lwd=2, main="PACF") 
par(mfrow=c(1,1)) # back to usual frame

# => ACF displays some exponential decay with significant autocorrelations 
# => PACF sudden drop after 1 significant lag (or 3-4 lags)
# => the results suggest the process follows an AR(1) process -> Box-Jenkins method: try suggested model, check diagnostics, fine-tune


## Model
# ----------------------------------------------------------

# estimate AR model
arLag = 3
ar_model <- arima(obsTarget, order=c(arLag,0,0), include.mean=FALSE)  # intercept not significant
print(ar_model)

# check significance of coefficients
arCoeff <- ar_model$coef
arSTE <- sqrt(diag(ar_model$var.coef))  # take srqt of diagonal elements of estimated var-cov matrix of ar coeff
tRatio <- abs(arCoeff / arSTE)  # 2-tailed test
critVal <- c(1.645, 1.96, 2.575)  # Normal distribution critical values at 90%, 95%, 99% confidence level

for (i in 1:length(tRatio)) {
  print(sprintf("Test for statistical significance for AR lag = %d", i))
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
plot(dfCC$Date, ar_model$residuals, type="l", lwd=2, col="red", main="Residuals", xlab="quarters", ylab="values")
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


## In-sample evaluation
# ----------------------------------------------------------

# check fitness of suggested ARMA model
ar_fitted <- obsTarget - ar_model$residuals  # build fitted values
obsTarget_ts <- ts(obsTarget)
cor_value <- round(cor(obsTarget_ts, ar_fitted,  use = "complete.obs", method = "pearson"), 2) # correlation between fitted and actual
print(cor_value)

# plot and save chart
png(paste0(WD, "/charts/", "AR_fit.png"), width = 10, height = 5, units = 'in', res = 300) 

plot(obsTarget_ts, type = "l", col = "blue", lwd = 2, xlab = "quarters", ylab = "growth rates", main = paste0(ctryTarget," ",varTarget, ": ", "AR model fit"))
lines(ar_fitted, col = "red", lwd = 2) 
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
legend("bottomright", legend = c("Observations", "Fitted values"), col = c("blue", "red"), lwd = 2, xpd=TRUE, bty="n")
text(x = min(obsTarget_ts), y = min(obsTarget_ts), labels = paste("Correlation:", cor_value), pos = 4, col = "black")
dev.off()


## Forecasting
# ----------------------------------------------------------

h <- 1
ar_forecast <- forecast(ar_model, h = h)

datesAlt <- c(dfCC$Date, tail(dfCC$Date, 1) %m+% months(h*3))  # append quarters based on horizon
yPredictedAlt <- c(obsTarget, as.numeric(ar_forecast$mean))

# plot forecast along history 
cuttoff <- max(dfCC$Date)
historicalDates <- datesAlt <= cuttoff  & datesAlt >= as.Date("2019-01-01") 
forecastDates <- datesAlt >= as.Date("2019-01-01")

png(paste0(WD, "/charts/", "AR_forecast.png"), width = 10, height = 5, units = 'in', res = 300)
plot(datesAlt[forecastDates], yPredictedAlt[forecastDates], type = "l", col = "red",  
     xlab = "quarters", ylab = "growth rates", 
     main = paste0(ctryTarget, " ", varTarget, ": Quarterly target variable (history and forecast)"), lwd = 2)
lines(datesAlt[historicalDates], yPredictedAlt[historicalDates], col = "blue", lwd = 2)
geom_hline(yintercept = 0, linetype = "solid", color = "gray")
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
legend(x = "bottomleft", 
       legend = c("History", "Forecast"), 
       col = c("blue", "red"),pch = c(NA, NA), lwd = c(2, 2), xpd = TRUE, bty = "n")
dev.off()



## Pseudo out-of-sample forecast validation exercise
# ----------------------------------------------------------

# # (!) uncomment in case of new model specification
# # specs
# trainEnd <- length(datesAlt[datesAlt <= dateStart])  # init number of observations for training
# testEnd <- length(datesAlt[datesAlt <= dateEnd])
# resultsPseudoOutAlt <- list()
# 
# # loop over expanding window
# start_time <- Sys.time()
# 
# for (j in seq(1, (testEnd-trainEnd))) {
#   
#   # subset data
#   train <- obsTarget[1:trainEnd]
#   test <- obsTarget[(trainEnd + 1):(trainEnd + h)]  
#   
#   # define model and forecast
#   ar_model <- arima(train, order=c(arLag,0,0), include.mean=FALSE)  # intercept not significant
#   ar_forecast <- forecast(ar_model, h = h)
#   
#   # calculate RMSE
#   rmse <- sqrt(mean((test - as.numeric(ar_forecast$mean))^2, na.rm = TRUE))
#   
#   # collect results
#   resultsPseudoOutAlt[[paste0("iter_",j)]] <- list(rmse = rmse, trainForecast = ar_forecast, test = test)
#   
#   # update inputs
#   trainEnd <- trainEnd + 1
# 
# }
# 
# end_time <- Sys.time()
# print(paste("Running time in minutes: ", round(end_time - start_time,2)))
# 
# # save exercise
# save(resultsPseudoOutAlt, file = "data/resultsPseudoOutSampleForecastAlt.RData")

