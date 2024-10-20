## Dynamic Factor Model
# ==========================================================

## Data
# ----------------------------------------------------------

# load housing data
full_data <- ihpdr::ihpd_get()
#cnames <- ihpdr::ihpd_countries()
rhpi <- select(full_data, Date, country, rhpi) %>% 
  filter(country == "US" & Date >= start_date) %>% 
  pivot_wider(names_from = country, values_from = rhpi) 
rhpi <- xts(rhpi["US"], order.by=as.Date(rhpi[["Date"]]))

# convert quarterly target to monthly series with NaN  
monthly_dates <- seq(from=start_date, to=end_date, by="month")
target <- xts(rep(NA, length(monthly_dates)), order.by=monthly_dates)  # fill

indices <- which(index(target) %in% index(rhpi))  # filter quarterly dates in monthly series
target[indices] <- rhpi  # include observations in quarters
rawtarget <- as.data.frame(target)
rawtarget[,1] <- as.numeric(rawtarget[,1])  # make sure values are numeric

#  load raw data with FRED
series_id_list <- read.csv(file="data/FRED_tickers.csv", header=FALSE)  # put interest rate variables at the end

data_list <- list()
for (series_id in series_id_list[[1]]) {
  data <- fredr(
    series_id = series_id,
    observation_start = start_date,
    observation_end = end_date,
    frequency = "m")
  data_list[[series_id]] <- data
}
rawdata <- bind_rows(data_list)
rawdf <- pivot_wider(rawdata, names_from = series_id, values_from = value)
rawdf <- bind_cols(rawdf, rawtarget)
rawdf <- rawdf[, !(names(rawdf) %in% c("realtime_start", "realtime_end"))]
rawdf <- rawdf %>% select(1, last_col(), everything())  # reorder target as the first variable

names(rawdf) <- c("dates", "RHPI", series_id_list[[2]])
print(names(rawdf))

# filter out explanatory variables
varFilter <- c("INF", "EMP", "IPT", "MRF", "SPR")  # strong drop during COVID-19 not seen in house prices, and slow-moving interest rate variables 
rawdf <- rawdf %>% 
  select(!all_of(varFilter))

# wrangle data
Nraw <- dim(rawdf)[2]
Traw <- dim(rawdf)[1]
varlabel <- names(rawdf)[2:Nraw]

dataMatrix <- as.matrix(rawdf[,2:length(rawdf)])
class(dataMatrix)
N <- dim(dataMatrix)[2]
T <- dim(dataMatrix)[1]
#NonMissIdx <-#complete.cases(dataMatrix[,1])
#TQ <- tail(which(NonMissIdx),1)  # get last quarterly data available
#lastNonNan <- T - tail(which(!is.na(dataMatrix[,1])), 1)

# compute growth rates
dataGrowthQuarter <- c(rep(NA, 3), log(dataMatrix[4:T, 1] / dataMatrix[1:(T-3), 1])*100)  # preallocate NA due to loosing observations
dataGrowthMonth <- rbind(rep(NA, length(2:(N))), log(dataMatrix[2:T, 2:(N)] / dataMatrix[1:T-1, 2:(N)])*100)  
dataTransf <- cbind(dataGrowthQuarter, dataGrowthMonth)
#dataGrowthMonth <- rbind(rep(NA, length(2:(N-2))), log(dataMatrix[2:T, 2:(N-2)] / dataMatrix[1:T-1, 2:(N-2)])*100)  
#dataLevelMonth <- dataMatrix[1:T, (N-1):N]  
#dataTransf <- cbind(dataGrowthQuarter, dataGrowthMonth, dataLevelMonth)


dates <- rawdf$dates
datesQ <- rawdf$dates[!is.na(dataTransf[,1])]

# plot model variables 
plot(datesQ, dataTransf[,1][!is.na(dataTransf[,1])], type = "l", lwd=2, col = "black",
     xlab="quarters", ylab = "growth rates", main = "")
title(varlabel[1])

for (i in 2:N) {
plot(dates, dataTransf[,i], type = "l", lwd=2, col = "blue",
     xlab="months", ylab = "growth rates", main = "")
title(varlabel[i])
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
Q <- 1  # number of quarterly variables
M <- N-Q  # number of monthly variables 
AR <- 2  # autoregressive order
m2q <- c((1/3),(2/3),1,(2/3),(1/3))  # conversion coeff for month2quarter (Mariano-Murasawa)
L <- length(m2q)

vfact <- 1  # variance of factor (normalized to 1 for model identification)
latentlen <- L + L*Q + AR*M  # length of state/latent vector  (see matrices.pdf) 

# set init vals for KF and ML
factorLag <- 2  # factor lags
idiosynLag <- 2  # idiosyn components lags

B <- 0.5*rep(1, N)  # coeff of factor in baseline equation (factor + idio) #c(0.9, 0.8, 0.7, 0.6, 0.5)
phif <- 0.3*rep(1, factorLag)  # autoreg coeff in factor equation 
phiy <- 0.3*rep(1, N*idiosynLag)  # autoreg coeff in idio equation (for each variable)
v <- apply(dataFilled, 2, sd)  # axis=2 -> cols; error variance in idio equation (for each variable -> std of observables)

startval = c(B, phif, phiy, v)  # all params
print(length(startval))  # number of params to estimate

# run Kalman filter and evaluate likelihood with initial values
# latentfilled <- kalman(startval, N, vfact, m2q, dataFilled, T, index, latentlen, Q, M, L, AR)
# like <- kalmanhood(startval, N, vfact, m2q, dataFilled, T, index, latentlen, Q, M, L, AR)  # only returns single output of evaluated likelihood needed for optimization function

# estimate optimal param with maximum likelihood
maxLike <- optim(par=startval, fn=kalmanhood, N=N, vfact=vfact, m2q=m2q, data=dataFilled, T=T, index=index, latentlen=latentlen, Q=Q, M=M, L=L, AR=AR, method="Nelder-Mead", hessian=T)

# get optimal param estimates and standard errors
coeff <- maxLike$par 
cramerrao <- solve(maxLike$hessian)
se <- sqrt(abs(diag(cramerrao)))

# use optimal param values to get state vector
coeff <- c(0.77690446, 1.08394332, 0.95181087, 0.24169396, 0.07025002, 0.34565148, -0.07310172, 0.84278041, 0.19120364, -0.05345543, -0.22799343,  0.04654695,  0.11864408,  0.01057419,  0.48757778,  0.76277815,  0.87935782,  1.08274710)
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


## forecasting
# ----------------------------------------------------------

# focus on target variable and factor
yPredicted <- filled[,1]  # predicted target (filled using transition equation in ss)
yPredictedCommon <- filledCommon[,1]  # predicted target (filled using common part of transition equation in ss)
print(cor(yPredicted, yPredictedCommon))

# compare monthly target variable with monthly factor
png(paste0(WD, "/charts/", "Compare_factor_esimatedTarget.png"), width = 12, height = 10, units = 'in', res = 300)
plot(dates, yPredicted, type = "l", col = "blue",  
     xlab = "dates", ylab = "percent", 
     main = "Monthly target variable and common factor", lwd = 2)
lines(rawdf$dates, yPredictedCommon, type = "l", col = "red", lwd = 2)
legend(x = "topright", legend = c("Estimated target growth", "Common component index"), 
       col = c("blue", "red"), lwd = 2)
dev.off()

# counterpart for target frequency (quarterly)
yTargetObs <- dataTransf[,1][!is.na(dataTransf[,1])]
yPredictedCommonQ <- yPredictedCommon[!is.na(dataTransf[,1])]
print(cor(yTargetObs, yPredictedCommonQ))

# compare quarterly target variable with quarterly factor
png(paste0(WD, "/charts/", "Compare_index_observedTarget.png"), width = 12, height = 10, units = 'in', res = 300)
plot(datesQ, yTargetObs, type = "l", col = "blue",  
     xlab = "dates", ylab = "percent", 
     main = "Quartetly target variable and common factor", lwd = 2)
lines(datesQ, yPredictedCommonQ, type = "l", col = "red", lwd = 2)
legend(x = "topright", legend = c("Observed target growth", "Common component index"), 
       col = c("blue", "red"), lwd = 2)
dev.off()