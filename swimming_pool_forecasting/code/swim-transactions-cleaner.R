
############################################################
# ASSIGNMENT 3
# USING IMPLEMENTATION FROM:
# DATA ANALYSIS TEXTBOOK
# Chapter 18 time series - swim ticket sales

#
# WHAT THIS CODES DOES:
# cleans data and aggregates to daily. 

# MY MODIFICATIONS: gets all outdoor pools

#
###########################################################


# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(Hmisc)
library(timeDate)
library(caret)

# Change working directory -------------------------------------------
# Make sure you are set up such that 
# 1. you are in the folder designated for Data Analysis.
# 2. You have a folder called da_case_studies, with the saved folder for this case study
# 3. You have a folder called da_data_repo, with data for this case study on it.

# if you opened directly the code, you may need this
# setwd("../..")   # go up two levels
getwd()
setwd("/Users/utassydv/Documents/workspaces/CEU/my_repos/data_analysis_prediction/swimming_pool_forecasting")

data_in   <- "data/raw/"
data_out  <- "data/clean/"



#############################################
# DATA CLEANING
#############################################

# Load raw data ------------------------------------------------------

raw <- as.data.frame(read.table(paste0(data_in,"SwimmingPoolAdmissionsCABQ-en-us.csv"),
                                sep = "\t",
                                header = TRUE,
                                fileEncoding = "UCS-2LE",
                                strip.white = TRUE))

# Filter data, create workfile --------------------------------------------------------

data <- raw

data <- data %>%
  filter(Location %in% c("AQSP01","AQEI01","AQEJ01","AQMP01","AQRG01", "AQSV01", "AQWP01")) %>% #outdoorpools
  filter(Category %in% c("ADMISTIER1","ADMISTIER2","ADMISTIER3")) %>%
  mutate(date = as.Date(Date_Time, format = "%Y-%m-%d")) 
Hmisc::describe(data$ITEM)

data <- data %>%
  mutate(core1 =  (ITEM %in%  c("ADULT" , "SENIOR" ,"TEEN" ,"CHILD", "TOT"))) %>%
  mutate(core2 =  (ITEM %in%  c("CHILD PM","ADULT PM","SENIOR PM", "TOT PM", "TEEN PN"))) %>%
  filter(core1 | core2) %>%
  mutate(date = as.Date(Date_Time, format = "%Y-%m-%d")) 

# Agrregate date to daily freq --------------------------------------

daily_agg <- aggregate(QUANTITY ~ date, data = data, sum)

# replace missing days with 0 
daily_agg <- daily_agg %>% 
  merge(data.frame(date = seq(from = min(daily_agg[,"date"]), to = max(daily_agg[,"date"]), by = 1)),
        all = TRUE) %>% 
  mutate(QUANTITY = ifelse(is.na(QUANTITY),0,QUANTITY))

# Create date/time variables ----------------------------------------

# 2010-2016 only full years used. 
daily_agg <- daily_agg %>%
  filter(date >= as.Date("2010-01-01")) %>%
  filter(date < as.Date("2017-01-01"))
Hmisc::describe(daily_agg)

# Save workfile
write.csv(daily_agg,paste(data_out,"outdoor_pools_work.csv",sep=""), row.names = FALSE)               
