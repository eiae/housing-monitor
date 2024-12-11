# ==========================================================
## Dynamic Factor Model
# ==========================================================

## Data preparation
# ----------------------------------------------------------

## prep data for mixed-frequency multivariate modelling

# subset data for target country and variable
dfCCwide <- select(dfCC, Date, country, all_of(modTarget)) %>% 
  pivot_wider(names_from = country, values_from = rhpi_qoq) 
dfCCts <- xts(dfCCwide[ctryTarget], order.by=as.Date(dfCCwide[["Date"]]))

# convert quarterly target to monthly series with NaN  
monthly_dates <- seq(from=start_date, to=end_date, by="month")
target <- xts(rep(NA, length(monthly_dates)), order.by=monthly_dates)  # fill

indices <- which(index(target) %in% index(dfCCts))  # filter quarterly dates in monthly series
target[indices] <- dfCCts  # include observations in quarters
dfTarget <- as.data.frame(target)
dfTarget[,1] <- as.numeric(dfTarget[,1])  # make sure values are numeric

# join housing and macro series for model
dfModel <- bind_cols(dfMacro, dfTarget)
dfModel <- dfModel %>% select(1, GDPC1, everything())  # reorder quarterly as the second variable
dfModel <- dfModel %>% select(1, last_col(), everything())  # reorder target as the first variable

# rename variables
rename_vector <- setNames(c("dates", varTarget, series_id_list[[2]]), c("date", "V1", series_id_list[[1]]))
dfModel <- dfModel %>%
  rename_with(~ rename_vector[.x], .cols = names(rename_vector))
print(names(dfModel))

# plot all variables
nrow <- ceiling(sqrt(length(names(dfModel))-1))
ncol <- ceiling((length(names(dfModel))-1) / nrow)

png(paste0(WD, "/charts/", "Data_variables.png"), width = 15, height = 10, units = 'in', res = 300)

par(mfrow = c(nrow, ncol))  

for (i in names(dfModel)[-1]) {
  var_name <- i
  
  if (var_name %in% c("RHPI", "GDP")) { 
    plot_data <- dfModel[!is.na(dfModel[, i]), i] # filter out rows with NaN values for quarterly variables
    plot_dates <- dfModel[!is.na(dfModel[, i]), 1]
  } else {
    plot_data <- dfModel[ , i]
    plot_dates <- dfModel[ , 1]
  }
  plot(plot_dates$dates, plot_data[[i]], type = "l", lwd = 2, col = "blue",
       xlab = "periods", ylab = "growth rates", main = "")
  grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
  title(i)
}

dev.off()
par(mfrow = c(1, 1))  # reset to default


# filter out explanatory variables
print(names(dfModel))
varFilter <- c("GDP", "CCS", "IPT", "CSI", "EMP", "HST", "SNH", "RRS", "ZHV", "MRF", "SPR")  # strong drop during COVID-19 not seen in house prices, and slow-moving interest rate variables 
dfModel <- dfModel %>% 
  select(!all_of(varFilter))

# wrangle data into matrix arrays for model estimation
Nraw <- dim(dfModel)[2]
Traw <- dim(dfModel)[1]
varlabel <- names(dfModel)[2:Nraw]

dataMatrix <- as.matrix(dfModel[,2:length(dfModel)])
class(dataMatrix)
N <- dim(dataMatrix)[2]
T <- dim(dataMatrix)[1]
Q <- 1  # number of quarterly variables
M <- N-Q  # number of monthly variables 
#NonMissIdx <-#complete.cases(dataMatrix[,1])
#TQ <- tail(which(NonMissIdx),1)  # get last quarterly data available
#lastNonNan <- T - tail(which(!is.na(dataMatrix[,1])), 1)

# compute growth rates
dataGrowthTarget <- dataMatrix[1:T,1]

dataGrowthMonth <- rbind(rep(NA, M), log(dataMatrix[2:T, (Q+1):(N)] / dataMatrix[1:T-1, (Q+1):(N)])*100)  

if (Q>1) {
  dataGrowthQuarter <- c(rep(NA, 3), log(dataMatrix[4:T, Q] / dataMatrix[1:(T-3), Q])*100)  # preallocate NA due to loosing observations
  column_mean <- mean(dataGrowthQuarter[(2):length(dataGrowthQuarter)], na.rm = TRUE)  # assign mean growth rate in first observation to align with length of target variable
  dataGrowthQuarter[1] <- column_mean
  dataTransf <- cbind(dataGrowthTarget, dataGrowthQuarter, dataGrowthMonth)
}

dataTransf <- cbind(dataGrowthTarget, dataGrowthMonth)

#dataGrowthMonth <- rbind(rep(NA, length(2:(N-2))), log(dataMatrix[2:T, 2:(N-2)] / dataMatrix[1:T-1, 2:(N-2)])*100)  
#dataLevelMonth <- dataMatrix[1:T, (N-1):N]  
#dataTransf <- cbind(dataGrowthQuarter, dataGrowthMonth, dataLevelMonth)

# dates
dates <- dfModel$dates
datesQ <- dfModel$dates[!is.na(dataTransf[,1])]

# plot model variables

if ( Q>=1 ) {
  nrow <- ceiling(sqrt(Q))
  ncol <- ceiling(Q / nrow)
  
  png(paste0(WD, "/charts/", "Model_variablesQ.png"), width = 10, height = 5, units = 'in', res = 300)
  
  par(mfrow = c(nrow, ncol))  
  
  for (i in 1:Q) {
  plot(datesQ, dataTransf[,i][!is.na(dataTransf[,i])], type = "l", lwd=2, col = "black",
       xlab="quarters", ylab = "growth rates", main = "")
  grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
  title(varlabel[i])
  }
  
  dev.off()
  par(mfrow = c(1, 1))  # reset to default
  
  
  nrow <- ceiling(sqrt(M))
  ncol <- ceiling(M / nrow)
  
  png(paste0(WD, "/charts/", "Model_variablesM.png"), width = 15, height = 10, units = 'in', res = 300)
  
  par(mfrow = c(nrow, ncol))  
  
  for (i in (Q+1):N) {
  plot(dates, dataTransf[,i], type = "l", lwd=2, col = "blue",
       xlab="months", ylab = "growth rates", main = "")
    grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
  title(varlabel[i])
  }
  
  dev.off()
  par(mfrow = c(1, 1))  # reset to default

} else {
  nrow <- ceiling(sqrt(N))
  ncol <- ceiling(N / nrow)
  
  png(paste0(WD, "/charts/", "Model_variables.png"), width = 10, height = 5, units = 'in', res = 300)
  
  par(mfrow = c(nrow, ncol))
  
  plot(datesQ, dataTransf[,1][!is.na(dataTransf[,1])], type = "l", lwd=2, col = "black",
       xlab="quarters", ylab = "growth rates", main = "")
  grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
  title(varlabel[1])
  
  for (i in (Q+1):N) {
    plot(dates, dataTransf[,i], type = "l", lwd=2, col = "blue",
         xlab="months", ylab = "growth rates", main = "")
    grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
    title(varlabel[i])
  }
  
  dev.off()
  par(mfrow = c(1, 1))  # reset to default
  
}

# compute moments
dataMean <- c()
dataSd <- c()

for ( i in 1:N ) {
  tempVec <- dataTransf[,i]
  tempNoMiss <- tempVec[!is.na(tempVec)]
  dataMean[i] <- mean(tempNoMiss)
  dataSd[i] <- sd(tempNoMiss)  
}

# standardize data
dataStandard <- standard(dataTransf)

# fill missing values with random data
dataFilled <- filler(dataStandard, 1)

# indicator for filled values
index <- !is.na(dataStandard)
index <- ifelse(index, 1, 0)  # code True=1, False=0


## Model
# ----------------------------------------------------------

# specs
AR <- 2  # autoregressive order
m2q <- c((1/3),(2/3),1,(2/3),(1/3))  # conversion coeff for month2quarter (Mariano-Murasawa)
L <- length(m2q)

vfact <- 1  # variance of factor (normalized to 1 for model identification)
latentlen <- L + L*Q + AR*M  # length of state/latent vector  (see matrices.pdf) 

# set init vals for KF and ML
factorLag <- 2  # factor lags
idiosynLag <- 2  # idiosyn components lags

B <- 0.1*rep(1, N)  # coeff of factor in baseline equation (factor + idio) #c(0.9, 0.8, 0.7, 0.6, 0.5)
phif <- 0.5*rep(1, factorLag)  # autoreg coeff in factor equation 
phiy <- 0.5*rep(1, N*idiosynLag)  # autoreg coeff in idio equation (for each variable)
v <- apply(dataFilled, 2, sd)  # axis=2 -> cols; error variance in idio equation (for each variable -> std of observables)

startval = c(B, phif, phiy, v)  # all params
print(length(startval))  # number of params to estimate

# # run Kalman filter and evaluate likelihood with initial values
# latentfilled <- kalman(startval, N, vfact, m2q, dataFilled, T, index, latentlen, Q, M, L, AR)
# like <- kalmanhood(startval, N, vfact, m2q, dataFilled, T, index, latentlen, Q, M, L, AR)  # only returns single output of evaluated likelihood needed for optimization function

# (!) uncomment in case of new model specification
# estimate optimal param with maximum likelihood
# maxLike <- optim(par=startval, fn=kalmanhood, N=N, vfact=vfact, m2q=m2q, data=dataFilled, T=T, index=index, latentlen=latentlen, Q=Q, M=M, L=L, AR=AR, method="Nelder-Mead", hessian=T)

# save optimal params
# save(maxLike, file = "data/optimalParamMaxLike.RData")

# get optimal param estimates and standard errors
load("data/optimalParamMaxLike.RData")
coeff <- maxLike$par 
cramerrao <- solve(maxLike$hessian)
se <- sqrt(abs(diag(cramerrao)))

# evaluate statistical significance of loadings (first N parameters)
tRatio <- abs(coeff[1:N] / se[1:N])  # 2-tailed test
critVal <- c(1.645, 1.96, 2.575)  # Normal distribution critical values at 90%, 95%, 99% confidence level

for (i in 1:length(tRatio)) {
  print(paste0("Test for statistical significance for DFM loading for ", varlabel[i]))
  if (coeff[i]>0) {
    print(sprintf("The loading is positive with value %f", coeff[i]))
  } else if (coeff[i]<0) {
    print(sprintf("The loading is negative with value %f", coeff[i]))
  }
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

# use optimal param values to get state vector
latentfilled <- kalman(coeff, N, vfact, m2q, dataFilled, T, index, latentlen, Q, M, L, AR)

# states and refilled data using measurement equation
latent <- latentfilled$latent
filled <- latentfilled$dataReFilled
filledCommon <- latentfilled$dataReFilledCommon

# add mean and std back in data vector since standardized at the beginning
for (i in 1:N) {
  filled[,i] <- filled[,i]*dataSd[i] + dataMean[i]
  filledCommon[,i] <- filledCommon[,i]*dataSd[i] + dataMean[i]
}


## In-sample evaluation
# ----------------------------------------------------------

# focus on target variable and factor
yPredicted <- filled[,1]  # predicted target (filled using transition equation in ss)
yPredictedCommon <- filledCommon[,1]  # predicted target (filled using common part of transition equation in ss)
cor_value <- round(cor(yPredicted, yPredictedCommon), 2)
print(cor_value)

# compare monthly target variable with monthly factor
png(paste0(WD, "/charts/", "Compare_factor_esimatedTarget.png"), width = 10, height = 5, units = 'in', res = 300)

plot(dates, yPredicted, type = "l", col = "blue",  
     xlab = "months", ylab = "growth rates", 
     main = paste0(ctryTarget," ",varTarget, ": ", "Monthly estimated target variable and common factor from DFM"), lwd = 2)
lines(dfModel$dates, yPredictedCommon, type = "l", col = "red", lwd = 2)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
legend(x = "bottomright", legend = c("Estimated target growth", "Common component index"), 
       col = c("blue", "red"), lwd = 2,  xpd=TRUE, bty="n")
text(x = min(dates), y = min(yPredicted), labels = paste("Correlation:", cor_value), pos = 4, col = "black")
dev.off()

# counterpart for target frequency (quarterly)
yTargetObs <- dataTransf[,1][!is.na(dataTransf[,1])]
yPredictedCommonQ <- yPredictedCommon[!is.na(dataTransf[,1])]
cor_value <- round(cor(yTargetObs, yPredictedCommonQ), 2)
print(cor_value)
# print(cor(yTargetObs, yPredicted[!is.na(dataTransf[,1])]))  # should coincide due to matching of quarterly data

# compare quarterly target variable with quarterly factor
png(paste0(WD, "/charts/", "DFM_fit.png"), width = 10, height = 5, units = 'in', res = 300)

plot(datesQ, yTargetObs, type = "l", col = "blue",  
     xlab = "quarters", ylab = "growth rates", 
     main = paste0(ctryTarget," ",varTarget, ": ", "Quartetly observed target variable and common factor from DFM"), lwd = 2)
lines(datesQ, yPredictedCommonQ, type = "l", col = "red", lwd = 2)
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
legend(x = "bottomright", legend = c("Observed target growth", "Common component index"), 
       col = c("blue", "red"), lwd = 2,  xpd=TRUE, bty="n")
text(x = min(datesQ), y = min(yTargetObs), labels = paste("Correlation:", cor_value), pos = 4, col = "black")
dev.off()


## Forecasting
# ----------------------------------------------------------

# plot forecast along history and matched quarters
cuttoff <- max(datesQ)
historicalDates <- dates <= cuttoff  & dates >= as.Date("2019-01-01") 
forecastDates <- dates >= as.Date("2019-01-01")
matchDates <- which(dates %in% datesQ)

png(paste0(WD, "/charts/", "DFM_forecast.png"), width = 10, height = 5, units = 'in', res = 300)
plot(dates[forecastDates], yPredicted[forecastDates], type = "l", col = "red",  
     xlab = "months", ylab = "growth rates", 
     main = paste0(ctryTarget, " ", varTarget, ": Monthly target variable (history and forecast)"), lwd = 2)
lines(dates[historicalDates], yPredicted[historicalDates], col = "blue", lwd = 2)
points(dates[matchDates], yPredicted[matchDates], col = "blue", pch = 1)  # Add red circles
geom_hline(yintercept = 0, linetype = "solid", color = "gray")
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")
legend(x = "bottomleft", 
       legend = c("History", "Forecast", "Matched observations"), 
       col = c("blue", "red", "blue"),pch = c(NA, NA, 1), lwd = c(2, 2, 2), xpd = TRUE, bty = "n")
dev.off()


# forecast table
datesTable <- dates >= cuttoff & dates <= end_date
forecastTable <- data.frame(Date = dates[datesTable], Forecast = round(yPredicted[datesTable], 2))
save(forecastTable, file = "data/forecastTable.RData")

# export forecast results
yPredictedOut <- data.frame(Date = dates)
yPredictedOut[[paste0(varTarget,"_forecast")]] <- yPredicted
yPredictedOut[[paste0(varTarget,"_forecastCommon")]] <- yPredictedCommon
print(max(datesQ))

write.xlsx(yPredictedOut, file = paste0(WD, "/data/", "forecast_output_", ctryTarget, ".xlsx"))

           
## Pseudo out-of-sample forecast validation exercise
# ----------------------------------------------------------

# # (!) uncomment in case of new model specification
# # specs
# h <- 3  # 1-quarter ahead forecast
# trainEnd <- length(dates[dates <= dateStart])  # init number of observations for training
# testEnd <- length(dates[dates <= dateEnd])
# startvalues <- startval
# resultsPseudoOut <- list()

# # loop over expanding window
# start_time <- Sys.time()
# 
# for (j in seq(1, (testEnd-trainEnd)/3)) {
#   
#   # run function for training sample and compare to test sample
#   resultsPseudoOut[[paste0("iter_",j)]] <- forecastPseudoOut(trainEnd = trainEnd, h = h, data = dataTransf, vfact, m2q, latentlen, Q, M, L, AR, startval = startvalues)
#   
#   # update inputs
#   trainEnd <- trainEnd + 3
#   # startvalues <- resultsPseudoOut[[paste0("iter_",j)]]$estimatedCoeff  # for more efficient likelihood estimation use previous optimal params
# }
# 
# end_time <- Sys.time()
# print(paste("Running time in minutes: ", round(end_time - start_time,2)))
# 
# # save exercise
# save(resultsPseudoOut, file = "data/resultsPseudoOutSampleForecast.RData")


## Comparison of forecast validation exercise
# ----------------------------------------------------------

# get DFM pseudo out of sample forecast results
load("data/resultsPseudoOutSampleForecast.RData")

# get ARMA pseudo out of sample forecast results
load("data/resultsPseudoOutSampleForecastAlt.RData")


# plot RMSE
rmseValues <- sapply(resultsPseudoOut, function(x) x$rmse)
rmseValuesAlt <- sapply(resultsPseudoOutAlt, function(x) x$rmse)

trainingDates <- datesQ[datesQ >= dateStart & datesQ < dateEnd]  # make sure they coincide for both models
rmse <- data.frame(
  iteration = trainingDates,
  DFM = rmseValues,
  ARMA = rmseValuesAlt
)

rmse_long <- pivot_longer(
  rmse,
  cols = c(DFM, ARMA),
  names_to = "Model",
  values_to = "RMSE"
)


# boxplot
boxplot <- ggplot(rmse_long, aes(x = Model, y = RMSE, fill = Model)) +
  geom_boxplot(color = "darkblue", alpha = 0.7) +
  scale_fill_manual(values = c("DFM" = "lightblue", "ARMA" = "lightgrey")) +
  labs(title = "RMSE boxplots", x = "model", y = "RMSE") +
  theme_light() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "none"
  )
png(paste0(WD, "/charts/", "RMSE_boxplot.png"), width = 8, height = 6, units = 'in', res = 300)
print(boxplot)
dev.off()


# lineplot
rmseplot <- ggplot(rmse_long, aes(x = iteration, y = RMSE, color = Model)) +
  geom_line(linewidth = 1) +                       
  geom_point(aes(fill = Model), size = 3, shape = 21) +                
  scale_color_manual(values = c("DFM" = "skyblue", "ARMA" = "lightgrey")) +
  scale_fill_manual(values = c("DFM" = "darkblue", "ARMA" = "darkgrey")) +
  labs(title = "RMSE across iterations", x = "iterations", y = "RMSE", color = "Model") +
  theme_light() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_labels = "%b %Y", breaks = rmse$iteration) 
png(paste0(WD, "/charts/", "RMSE_evolution.png"), width = 10, height = 5, units = 'in', res = 300)
print(rmseplot)
dev.off()


