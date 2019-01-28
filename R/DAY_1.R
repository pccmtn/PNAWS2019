######################## PSYCH NETWORK WINTER CAMP 2019 ############################

setwd("/Users/martinapocchiari/Dropbox (RSM)/PNWC2019/PNAWS2019/")
library(data.table)
library(dplyr)
library(foreign)
library(qgraph)

######################## DAY 1 ############################

######################## SESSION 1-1 ############################

### Exercises 1-1

## Q1

a <- c(1,6,NA,8,9,10)
mean(a, na.rm = T)
mean(a[!is.na(a)])

## Q2

mtrix <- matrix(data = c(0,1,1,1,
                         0,0,0,0,
                         0,0,0,0,
                         0,0,0,0), nrow = 4, ncol = 4, byrow = T)
qgraph(mtrix)


######################## SESSION 1-2 ############################

Data <- read.csv("Practical_1.2/stormofswords.csv", stringsAsFactors = F)



