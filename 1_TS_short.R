################################################################################################
### R scripts for time-stratified case-crossover design for short-term exposure              ###
### - Exposure: daily total PM2.5 mass                                                       ###
### - Outcome: same-day asthma hospitalization risk                                          ###
### - Confounder: temperature                                                                ###
### - For more details, see: "Wei Y, et al. Air pollutants and asthma hospitalization in the ### 
###   Medicaid population. American Journal of Respiratory and Critical Care Medicine.       ###
###   2022;205(9):1075-83."                                                                  ###
################################################################################################

rm(list=ls())
gc()

library(survival)
library(data.table)
library(lubridate)
library(magrittr)

### Load case-only dataset ###
# In the dataset, each row represents an asthma hospitalization, with columns for the hospitalization 
# date and the patient’s residential ZIP code
asthma <- read.csv('~/asthma_medicaid.csv')
asthma$case <- 1 # Indicator for case: 1 = event
asthma$id <- 1:dim(asthma)[1] # Unique ID for each hospitalization
asthma$adate <- as.Date(asthma$adate,"%d%b%Y") # adate: date of hospitalization
asthma$amonth <- month(asthma$adate) # Extract calendar month for control selection

### Selection of controls: +/- 1-5 weeks ###
for (i in 1:5){
  tmp_back <- asthma
  tmp_forw <- asthma
  
  # Shift hospitalization dates backward and forward by i weeks
  tmp_back$adate <- tmp_back$adate - 7*i
  tmp_forw$adate <- tmp_forw$adate + 7*i
  
  # Keep only controls in the same calendar month as their matched case
  tmp_back$amonth_new <- month(tmp_back$adate)
  tmp_back <- tmp_back[tmp_back$amonth == tmp_back$amonth_new, ]
  tmp_back$amonth_new <- NULL # Trash it
  
  tmp_forw$amonth_new <- month(tmp_forw$adate)
  tmp_forw <- tmp_forw[tmp_forw$amonth == tmp_forw$amonth_new, ]
  tmp_forw$amonth_new <- NULL # Trash it
  
  # Combine forward and backward controls
  tmp <- bind_rows(tmp_back, tmp_forw)
  if (i==1){
    control <- tmp
  }else{
    control <- bind_rows(control, tmp)
  }
  rm(tmp_back);rm(tmp_forw); rm(tmp);gc()
  print(i)
}
control$case <- 0 # Indicator for control

### Combine cases and matched controls to construct the final time-stratified case-crossover dataset ###
asthma_CCO <- bind_rows(asthma, control)

### Merge in daily total PM2.5 mass and temperature based on each patient's residential ZIP code and hospitalization date ###
pm25 <- readRDS('~/exp_pm.rds')
asthma_CCO <- left_join(asthma_CCO, pm25, by=c('zipcode'='ZIP','adate'='date'))

temp <- readRDS('~/temp.rds')
asthma_CCO <- left_join(asthma_CCO, temp, by=c('zipcode'='ZIP','adate'='date'))

rm(pm,temp)
gc()

### Fit a conditional logistic regression, coefficients are relative risks ###
mod_asthma_ts <- clogit(case ~ pm25 + temp + strata(id), data=asthma_CCO, method="approximate")

