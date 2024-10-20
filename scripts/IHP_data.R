
# load raw data
raw <- read_excel(path = paste0("data/", "hp",dateSuffix,".xlsx"), sheet = "RHPI")  

# clean and reframe
data <- raw[2:nrow(raw),]  # drop empty row
data <- data %>%
  select(where(~any(!is.na(.))))  # drop na columns

# rename colnames
colnames(data)[1] <- "date"
colnames(data)[ncol(data)-1] <- "Aggregate_fix"
colnames(data)[ncol(data)] <- "Aggregate_dyn"

# replace original date format by R dates
tail(data["date"])
dateSeq <- seq(as.Date("1975-01-01"), as.Date("2024-03-01"), by = "3 months")
data$date <- dateSeq

# reshape into long format
df <- gather(data, key = 'cc', value = "obs", -date)  # key = colnames, value = values of matrix 
df$obs <- as.numeric(df$obs)  # make sure observations are numerical values

# interquartile range
dfFiltered <- df %>% 
  filter(!cc %in% c("Aggregate_fix", "Aggregate_dyn"))  # filter out aggregates
dfTemp <- dfFiltered %>% 
  group_by(date) %>% 
  mutate(intquart = quantile(obs, 0.75) - quantile(obs, 0.25),
         quant3 = quantile(obs, 0.75),
         quant1 = quantile(obs, 0.25)) %>% 
  ungroup()

dfAggregates <- df %>% 
  filter(cc %in% c("Aggregate_fix", "Aggregate_dyn"))  # add aggregates back
df <- merge(x=dfTemp, y=dfAggregates, all=TRUE) 

# difference data
df <- df %>% 
  group_by(cc) %>% 
  mutate(qoq = c(rep(NA, each=1), diff(log(obs), lag=1, difference=1)*100), # add new column with diff data preserving length 
         yoy = c(rep(NA, each=4), diff(log(obs), lag=4, difference=1)*100)) %>%  
  ungroup()

# focus on US
dfCCJoint <- df %>% 
  filter(cc %in% c("US", "Aggregate_fix", "Aggregate_dyn"))
dfCC <- df %>% 
  filter(cc == c("US"))

# fill NaN in differenced data with mean
dfCC$qoq[1] <- mean(dfCC$qoq[2:length(dfCC$qoq)])  # skip first obs to compute mean (otherwise NaN)
dfCC$yoy[1:4] <- mean(dfCC$yoy[5:length(dfCC$yoy)])  # skip first obs to compute mean (otherwise NaN)
