###################################################################################################
### R scripts for symmetric bidirectional case-crossover design for intermediate-term exposure ###
### - Exposure: monthly wildfire-specific PM2.5                                                ###
### - Outcome: risk of same-month hospitalization for hypertension                             ###
### - Confounders: temperature, calendar month                                                 ###
### - For more details, see: "Wei Y, et al. Medium-term exposure to wildfire smoke PM2.5 and   ###
###   cardiorespiratory hospitalization risks. Epidemiology. 2025:10-97."                      ###
##################################################################################################

rm(list=ls())
gc()

library(survival)
library(data.table)
library(lubridate)
library(magrittr)

### Load case-only dataset ###
# Here each row represents a hospitalization record for pneumonia, with columns for hospitalization 
# month, hospitalization year, and the patient’s residential ZIP code
pneumonia <- readRDS('~/Case_pneumonia_monthly.rds')
pneumonia$case <- 1 # Indicator for case: 1 = event
pneumonia$id <- 1:dim(pneumonia)[1] # Unique ID for each hospitalization

### Select controls from one month prior to the case month ###
control_back_cvd1 <- pneumonia
control_back_cvd1$MONTH_tmp <- control_back_cvd1$AMONTH
control_back_cvd1$AMONTH <- ifelse(control_back_cvd1$MONTH_tmp==1,12,control_back_cvd1$MONTH_tmp-1)
control_back_cvd1$AYEAR <- ifelse(control_back_cvd1$MONTH_tmp==1,control_back_cvd1$AYEAR-1,control_back_cvd1$AYEAR)
control_back_cvd1$MONTH_tmp <- NULL
control_back_cvd1$case <- 0

### Select controls from one month after the case month ###
control_forw_cvd1 <- pneumonia
control_forw_cvd1$MONTH_tmp <- control_forw_cvd1$AMONTH
control_forw_cvd1$AMONTH <- ifelse(control_forw_cvd1$MONTH_tmp==12,1,control_forw_cvd1$MONTH_tmp+1)
control_forw_cvd1$AYEAR <- ifelse(control_forw_cvd1$MONTH_tmp==12,control_forw_cvd1$AYEAR+1,control_forw_cvd1$AYEAR)
control_forw_cvd1$MONTH_tmp <- NULL
control_forw_cvd1$case <- 0

### Combine cases and controls ###
pneumonia_CCO_sb <- rbind(pneumonia,control_back_cvd1,control_forw_cvd1)

### Merge in monthly wildfire-specific PM2.5 and temperature based on each patient's residential ZIP code and hospitalization date ###
smoke_monthly <- readRDS('~/smoke_monthly.rds')
pneumonia_CCO_sb <- left_join(pneumonia_CCO_sb,smoke_monthly,by=c('ZIP'='ZIP','AYEAR'='Year','AMONTH'='Month'))

temp_monthly <- readRDS('~/temp_monthly.rds')
pneumonia_CCO_sb <- left_join(pneumonia_CCO_sb,temp_monthly,by=c('ZIP'='ZIP','AYEAR'='Year','AMONTH'='Month'))

rm(smoke_monthly,temp_monthly)
gc()

### Fit a conditional logistic regression, coefficients are relative risks ###
mod_pneumonia_sb <- clogit(case ~ smokePM + temp + +as.factor(AMONTH) + strata(id), data=pneumonia_CCO_sb, method="approximate")
