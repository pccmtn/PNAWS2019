######################## PSYCH NETWORK WINTER CAMP 2019 ############################

setwd("/Users/martinapocchiari/Dropbox (RSM)/PNWC2019/PNWC2019/")
library(data.table)
library(dplyr)
library(foreign)

######################## DAY 1 ############################

job_performance <- foreign::read.spss("Practical_1.1/job_performance.sav") %>% dplyr::bind_rows()
