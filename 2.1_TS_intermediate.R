###############################################################################################
### R scripts for time-stratified case-crossover design for intermediate-term exposure      ###
### - Exposure: monthly wildfire-specific PM2.5                                             ###
### - Outcome: risk of same-month hospitalization for hypertension                          ###
### - Confounders: temperature, calendar month                                              ###
### - For data sources, please refer to "Wei Y, et al. Medium-term exposure to wildfire     ###
###   smoke PM2.5 and cardiorespiratory hospitalization risks. Epidemiology. 2025:10-97."   ###
###   The publication used a symmetric bidirectional design, which is covered in            ###
###   "2.2_SB_intermediate.R"                                                               ###
###############################################################################################

rm(list=ls())
gc()

library(survival)
library(data.table)
library(lubridate)
library(magrittr)

### Load case-only dataset ###
# Here each row represents a hospitalization record for hypertension, with columns for hospitalization 
# month, hospitalization year, and the patient’s residential ZIP code
hypertension <- readRDS('~/Case_hypertension_monthly.rds')
hypertension$case <- 1 # Indicator for case: 1 = event
hypertension$id <- 1:dim(hypertension)[1] # Unique ID for each hospitalization

### Assign season of case month (1: spring, 2: summer, 3: fall, 4: winter) ###
hypertension$season <- ifelse(hypertension$AMONTH%in%c(2:4),1,
                              ifelse(hypertension$AMONTH%in%c(5:7),2,
                                     ifelse(hypertension$AMONTH%in%c(8:10),3,4)))

### Extract ID and season for each case ###
hypertension_season <- hypertension[,c('id','season')]
names(hypertension_season)[2] <- 'season_case'

### Select controls from one month prior to the case month ###
control_back_1 <- hypertension
control_back_1$AMONTH_tmp <- control_back_1$AMONTH # Preserve original case month for reference
control_back_1$AMONTH <- ifelse(control_back_1$AMONTH_tmp==1,12,control_back_1$AMONTH_tmp-1) # Shift one month back
control_back_1$AYEAR <- ifelse(control_back_1$AMONTH_tmp==1,control_back_1$AYEAR-1,control_back_1$AYEAR) # Adjust year if original month is January
# Assign season of control month
control_back_1$season <- ifelse(control_back_1$AMONTH%in%c(2:4),1,
                                ifelse(control_back_1$AMONTH%in%c(5:7),2,
                                       ifelse(control_back_1$AMONTH%in%c(8:10),3,4))) 
control_back_1$case <- 0 # Indicator for control
# Keep controls within the same season as the case
control_back_1 <- left_join(control_back_1,hypertension_season,by='id')
control_back_1 <- control_back_1[control_back_1$season==control_back_1$season_case,]

### Select controls from two months prior to the case month ###
control_back_2 <- hypertension
control_back_2$AMONTH_tmp <- control_back_2$AMONTH
control_back_2$AMONTH <- ifelse(control_back_2$AMONTH_tmp==1,11,
                                ifelse(control_back_2$AMONTH_tmp==2,12,control_back_2$AMONTH_tmp-2))
control_back_2$AYEAR <- ifelse(control_back_2$AMONTH_tmp%in%c(1,2),control_back_2$AYEAR-1,control_back_2$AYEAR)
control_back_2$season <- ifelse(control_back_2$AMONTH%in%c(2:4),1,
                                ifelse(control_back_2$AMONTH%in%c(5:7),2,
                                       ifelse(control_back_2$AMONTH%in%c(8:10),3,4)))
control_back_2$case <- 0
control_back_2 <- left_join(control_back_2,hypertension_season,by='id')
control_back_2 <- control_back_2[control_back_2$season==control_back_2$season_case,]

### Select controls from one month after to the case month ###
control_forw_1 <- hypertension
control_forw_1$AMONTH_tmp <- control_forw_1$AMONTH
control_forw_1$AMONTH <- ifelse(control_forw_1$AMONTH_tmp==12,1,control_forw_1$AMONTH_tmp+1)
control_forw_1$AYEAR <- ifelse(control_forw_1$AMONTH_tmp==12,control_forw_1$AYEAR+1,control_forw_1$AYEAR)
control_forw_1$season <- ifelse(control_forw_1$AMONTH%in%c(2:4),1,
                                ifelse(control_forw_1$AMONTH%in%c(5:7),2,
                                       ifelse(control_forw_1$AMONTH%in%c(8:10),3,4)))
control_forw_1$case <- 0
control_forw_1 <- left_join(control_forw_1,hypertension_season,by='id')
control_forw_1 <- control_forw_1[control_forw_1$season==control_forw_1$season_case,]

### Select controls from two months after to the case month ###
control_forw_2 <- hypertension
control_forw_2$AMONTH_tmp <- control_forw_2$AMONTH
control_forw_2$AMONTH <- ifelse(control_forw_2$AMONTH_tmp==11,1,
                                ifelse(control_forw_2$AMONTH_tmp==12,2,control_forw_2$AMONTH_tmp+2))
control_forw_2$AYEAR <- ifelse(control_forw_2$AMONTH_tmp%in%c(11,12),control_forw_2$AYEAR+1,control_forw_2$AYEAR)
control_forw_2$season <- ifelse(control_forw_2$AMONTH%in%c(2:4),1,
                                ifelse(control_forw_2$AMONTH%in%c(5:7),2,
                                       ifelse(control_forw_2$AMONTH%in%c(8:10),3,4)))
control_forw_2$case <- 0
control_forw_2 <- left_join(control_forw_2,hypertension_season,by='id')
control_forw_2 <- control_forw_2[control_forw_2$season==control_forw_2$season_case,]

### Combine cases and controls ###
hypertension_CCO_ts <- bind_rows(hypertension,control_back_1,control_back_2,control_forw_1,control_forw_2)

### Merge in monthly wildfire-specific PM2.5 and temperature based on each patient's residential ZIP code and hospitalization date ###
smoke_monthly <- readRDS('~/smoke_monthly.rds')
hypertension_CCO_ts <- left_join(hypertension_CCO_ts,smoke_monthly,by=c('ZIP'='ZIP','AYEAR'='Year','AMONTH'='Month'))

temp_monthly <- readRDS('~/temp_monthly.rds')
hypertension_CCO_ts <- left_join(hypertension_CCO_ts,temp_monthly,by=c('ZIP'='ZIP','AYEAR'='Year','AMONTH'='Month'))

rm(smoke_monthly,temp_monthly)
gc()

### Fit a conditional logistic regression, coefficients are relative risks ###
mod_hypertension_ts <- clogit(case ~ smokePM + temp + +as.factor(AMONTH) + strata(id), data=hypertension_CCO_ts, method="approximate")
