###############################################################################################
### R scripts for symmetric bidirectional case-crossover design for long-term exposure      ###
### - Exposure: annual wildfire-specific PM2.5                                              ###
### - Outcome: risk of same-year hospitalization for hypertension                           ###
### - Confounders: temperature, calendar year, age, chronic health conditions, and          ###
###   neighborhood-level characteristics                                                    ###
### - For more details, see: Zhang M, et al. Cardiopulmonary hospitalization risks from     ###
###   wildfire and non-wildfire PM2.5 in 20 US states. medRxiv.                             ###
###   doi: https://doi.org/10.1101/2025.07.15.25331618                                      ###
###############################################################################################

rm(list=ls())
gc()

library(survival)
library(data.table)
library(lubridate)
library(magrittr)

### Load case-only dataset ###
# Here each row represents a hospitalization record for hypertension, with columns for the hospitalization 
# year, patient’s age, whether or not have diabetes, whether or not have obesity, and residential ZIP code
hypertension <- readRDS('~/Case_hypertension_annually.rds')
hypertension$case <- 1 # Indicator for case: 1 = event
hypertension$id <- 1:dim(hypertension)[1] # Unique ID for each hypertension

### Select controls from one year prior to the case year ###
control_back_cvd1 <- hypertension
control_back_cvd1$YEAR <- control_back_cvd1$YEAR - 1
control_back_cvd1$case <- 0

### Select controls from one year after to the case year ###
control_forw_cvd1 <- hypertension
control_forw_cvd1$YEAR <- control_forw_cvd1$YEAR + 1
control_forw_cvd1$case <- 0

### Combine cases and controls ###
hypertension_CCO_sb <- rbind(hypertension,control_back_cvd1,control_forw_cvd1)

### Merge in annually wildfire-specific PM2.5, temperature and neighborhood-level characteristics based on each patient's ###
### residential ZIP code and hospitalization year ###
smoke_annually <- readRDS('~/smoke_annually.rds')
hypertension_CCO_sb <- left_join(hypertension_CCO_sb,smoke_annually,by=c('ZIP'='ZIP','YEAR'='Year'))

temp_annually <- readRDS('~/temp_annually.rds')
hypertension_CCO_sb <- left_join(hypertension_CCO_sb,temp_annually,by=c('ZIP'='ZIP','YEAR'='Year'))

Char_annually <- readRDS('~/Char_annually.rds')
hypertension_CCO_sb <- left_join(hypertension_CCO_sb,Char_annually,by=c('ZIP'='ZIP','YEAR'='Year'))

rm(smoke_annually,temp_annually,Char_annually)
gc()

### Fit a conditional logistic regression, coefficients are relative risks ###
mod_hypertension_sb <- clogit(case ~ smokePM + temp + age + diabetes + obesity + as.factor(YEAR) + pct_white + pct_black +
                                pct_high_school + med_household_income + townsend_index + strata(id), 
                              data=hypertension_CCO_sb, method="approximate")


