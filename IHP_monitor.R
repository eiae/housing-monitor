# ========================================
# International housing price monitor
# ========================================

# Erik Andres Escayola
# Housing observatory: https://int.housing-observatory.com
# Data source: https://www.dallasfed.org/research/international/houseprice#tab2
# TimeSeries library: https://cran.r-project.org/web/packages/tseries/tseries.pdf
# Exuberance paper: https://link.springer.com/article/10.1007/s11146-015-9531-2
# Exuberance library: https://cran.r-project.org/web/packages/exuber/exuber.pdf
# I acknowledge use of the dataset described in Mack and Martinez-Garcia (2011)

## PREAMBLE
# housekeeping
rm(list = ls())

# set working directory
WD <- "E:/GITHUB/housing-monitor"
setwd(WD)
getwd()

# load libraries
library(fredr)
library(flow)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readxl)
library(stats)
library(moments)
library(tseries)
library(xts)
library(forecast)
library(openxlsx)
library(remotes)
library(purrr)
library(ihpdr) #install_github("kvasilopoulos/ihpdr")

# import functions
source("functions/standard.R")
source("functions/filler.R")
source("functions/statespace.R")
source("functions/alternateMat.R")
source("functions/alternateIdioMat.R")
source("functions/kalmanhood.R")
source("functions/kalman.R")
source("functions/forecastPseudoOut.R")

# set FRED key (if needed)
# fred_key <- read.table("data/FRED_API_key.txt", header = FALSE)
# fredr_set_key(as.character(fred_key))


## DATA WRANGLING
source("scripts/IHP_data.R")

## DATA MODELLING
source("scripts/IHP_model_boxjenkins.R")
source("scripts/IHP_model_dfm.R")

## REPORT
rmarkdown::render("reports/IHP_forecast_report.Rmd")

