# ==========================================================
## Data loading and wrangling
# ==========================================================

## load housing data
raw <- ihpdr::ihpd_get()

# set start date
start_date <- min(raw$Date)

# make sure observations are numerical values
df <- raw %>%
  mutate(
    hpi = as.numeric(hpi),
    rhpi = as.numeric(rhpi),
    pdi = as.numeric(pdi),
    rpdi = as.numeric(rpdi)
  )  

# countries/regions
ctry <- unique(df["country"]) 
names(df)

# compute affordability and difference data
df <- df %>% 
  group_by(country) %>% 
  mutate(
    afi = (hpi/pdi)*100,
    rafi = (rhpi/rpdi)*100,
    rhpi_qoq = c(rep(NA, each=1), diff(log(rhpi), lag=1, difference=1)*100), # add new column with diff data preserving length 
    rhpi_yoy = c(rep(NA, each=4), diff(log(rhpi), lag=4, difference=1)*100),
    rpdi_qoq = c(rep(NA, each=1), diff(log(rpdi), lag=1, difference=1)*100),
    rpdi_yoy = c(rep(NA, each=4), diff(log(rpdi), lag=4, difference=1)*100),
    rafi_qoq = c(rep(NA, each=1), diff(log(rafi), lag=1, difference=1)*100),
    rafi_yoy = c(rep(NA, each=4), diff(log(rafi), lag=4, difference=1)*100)
    ) %>%  
  ungroup()

# single out country for modelling
dfCC <- df %>% 
  filter(country == c("US"))

# fill NaN in differenced data with mean
cols <- c("rhpi_qoq", "rhpi_yoy", "rpdi_qoq", "rpdi_yoy", "rafi_qoq", "rafi_yoy") 
initial_elements <- c(1, 4, 1, 4, 1, 4)  # number of elements to replace in each column

for (i in seq_along(cols)) {
  col_name <- cols[i]
  n <- initial_elements[i]
  # compute the mean of the rest of the column, excluding the first n elements
  column_mean <- mean(dfCC[[col_name]][(n + 1):length(dfCC[[col_name]])], na.rm = TRUE)
  # assign the computed mean to the first n elements
  dfCC[[col_name]][1:n] <- column_mean
}

# specs
ctryTarget <- unique(dfCC[["country"]]) 
varNames <- names(dfCC)

varTarget <- "RHPI"
lvlTarget <- dfCC$rhpi 
modTarget <- "rhpi_qoq"
obsTarget <- dfCC$rhpi_qoq 


## load macro data
# FRED
series_id_list <- read.csv(file="data/FRED_tickers.csv", header=FALSE)  # put interest rate variables at the end

macro_list <- list()
for (j in series_id_list[[1]]) {
  if (j == "GDPC1"){
    dataQ <- fredr(
      series_id = j,
      observation_start = start_date,
      #observation_end = end_date, # get all available data
      frequency = "q")
    macro_list[[j]] <- dataQ
  } else {
  data <- fredr(
    series_id = j,
    observation_start = start_date,
    #observation_end = end_date, # get all available data
    frequency = "m")
  macro_list[[j]] <- data
  }
}

rawMacro <- bind_rows(macro_list)
rawMacro <- rawMacro %>% select(-realtime_start, -realtime_end)
#rawMacro <- rawMacro[, -((ncol(rawMacro) - 1):ncol(rawMacro))]  # delete last two columns directly in case calling them does not work


# combine quarterly and monthly series in one monthly dataframe
rawMacro <- rawMacro %>%
  mutate(date = as.Date(date))
monthly_dates <- seq.Date(from = min(rawMacro$date), to = max(rawMacro$date), by = "month")

dfMacro_long <- rawMacro %>%
  complete(date = monthly_dates, series_id = series_id_list[[1]])
dfMacro_long <- dfMacro_long %>%
  mutate(value = if_else(series_id == "GDPC1" & !(month(date) %in% c(1, 4, 7, 10)), NA_real_, value)) # will with NaN non-quarterly months
dfMacro <- pivot_wider(dfMacro_long, names_from = series_id, values_from = value)

# set final date
end_date <- max(dfMacro$date)

# set train start and test end dates in case of redoing the pseudo out-of-sample exercise
dateStart <- as.Date("2021-01-01")
dateEnd <- as.Date("2024-01-01")

# ---
# # alternative in case data fetching fails
# dateSuffix <- 2401
# raw <- read_excel(path = paste0("data/", "hp",dateSuffix,".xlsx"), sheet = "RHPI")
# # clean and reframe
# data <- raw[2:nrow(raw),]  # drop empty row
# data <- data %>%
#   select(where(~any(!is.na(.))))  # drop na columns
# # rename colnames
# colnames(data)[1] <- "date"
# colnames(data)[ncol(data)-1] <- "Aggregate_fix"
# colnames(data)[ncol(data)] <- "Aggregate_dyn"
# # replace original date format by R dates
# tail(data["date"])
# dateSeq <- seq(as.Date("1975-01-01"), as.Date("2024-03-01"), by = "3 months")
# data$date <- dateSeq
# # reshape into long format
# df <- gather(data, key = 'cc', value = "obs", -date)  # key = colnames, value = values of matrix
# df$obs <- as.numeric(df$obs)  # make sure observations are numerical values
# ---
