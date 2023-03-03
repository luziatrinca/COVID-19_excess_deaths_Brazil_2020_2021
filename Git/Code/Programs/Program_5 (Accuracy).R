################################################################################
# Prediction accuracy: 

library(tidyverse)
library(geepack)
library(nlme)

# Set the path where the files are saved
setwd("C:/Review2/Code/")

source("Programs/Rfunctions.R") # Convenience functions for all the calculations
# required.

# Prepare the data

load("Data/brall.RData")

dataprev <- brall %>% filter(scope=="Brazil") %>% 
            filter(yearepi>=2010&yearepi<2020|(yearepi==2020&week<=9)) %>% 
            filter(week<=52)

# Covariates for the modeling without estimating omega
# used in model 10 below only
dataprev <- dataprev %>% mutate(S11=sin(2*pi*week/52), 
                                C11=cos(2*pi*week/52), S22=sin(2*2*pi*week/52), 
                                C22=cos(2*2*pi*week/52))

# Data for gee model
# Selecting the data for the Brazilian Total Deaths by epidemiological week
# and the years for fitting the baseline mode (2015-2020 up to week 9)
datagee <- brall %>% filter(scope=="Cause") %>% 
           filter((yearepi>=2010&yearepi<2020)|(yearepi==2020&week<=9)) %>% 
           filter(week<=52) %>% 
  droplevels() %>% mutate(idcause=factor(category))


levels(datagee$idcause) <- c(
  "7",
  "3",
  "1",
  "4",
  "6",
  "5",
  "2"
)


# Model 0
M0acc <- accuracy(dataprev, model_type="null", model="m0")
m0 <- round(apply(M0acc,2,Summ),2)

# Model 1
M1acc <- accuracy(dataprev, model_type="mixed", model="m1")
m1 <- round(apply(M1acc,2,Summ),2)

# Model 5
M5acc <- accuracy(dataprev, model_type = "mixed", model="m5")
m5 <- round(apply(M5acc,2,Summ),2)

# Model 6
M6acc <- accuracy(dataprev, model_type = "mixed", model="m6")
m6 <- round(apply(M5acc,2,Summ),2)

# Model 9
M9acc <- accuracy(dataprev, model_type ="mixed", model="m9")
m9 <- round(apply(M9acc,2,Summ),2)

# Model 10 = model 9 with T = 52
M10acc <- accuracy(dataprev, model_type ="mixed", model="lme2") 
m10 <- round(apply(M10acc,2,Summ),2)

# Model 11
M11acc <- accuracy(dataprev, dataprevC=datagee, model_type ="mixed", model="gee") 
m11 <- round(apply(M11acc,2,Summ),2)

avr <- accuracy(dataprev, model_type ="null", model="5year")
avrf <- round(apply(avr,2,Summ),2)

A <- rbind(m0,m1,m5,m6,m9,m10, m11, avrf)

row.names(A) <- c("m0.1", "m0.m", "m0.md", "m0.2", 
                  "m1.1", "m1.m", "m1.md", "m1.2", 
                  "m5.1", "m5.m", "m5.md", "m5.2", 
                  "m6.1", "m6.m", "m6.md", "m6.2", 
                  "m9.1", "m9.m", "m9.md", "m9.2",
                  "m10.1", "m10.m", "m10.md", "m10.2", 
                  "gee.1", "gee.m", "gee.md", "gee.2", 
                  "a.1", "a.m", "a.md", "a.2")

library(xtable)
xtable(A, type = "latex", file = "Accuracy.tex")
