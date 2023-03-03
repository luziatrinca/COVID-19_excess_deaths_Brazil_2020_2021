# Title: Code for results reproduction presented in the paper: 
# "Assessing COVID-19 pandemic excess deaths in Brazil: years 2020 and 2021"
# Authors: Saditt Rocio Robles Colonia
#          Lara Morena Cardeal
#          Rogério Antonio de Oliveira
#          Luzia Aparecida Trinca

# All-cause deaths in Brazil
# Packages required
library(tidyverse)
library(nlme)
library(varTestnlme)
library(geepack)

# Set the path where the files are saved
setwd("C:/Review2/Code/")

# Load the R object brall.RData
# NOTE 1:
# brall.RData is a processed dataset, source was downloaded in 27 SEPTEMBER 2022 
# from: 
# 1) \url{https://opendatasus.saude.gov.br/dataset/sim-1979-2019}
# 2) \url{https://opendatasus.saude.gov.br/dataset/sim-2020-2021} - preliminary:
# updated in 26 SEPTEMBER 2022, 15:45 (UTC-03:00)

# NOTE 2: 
# If desired, for updating the dataset, check and change accordingly the link 
# in 2). For creating brall.RData with updated data, one can follow parts of the 
# code in Santos et al. (2021) available at 
# https://github.com/aamouradasilva/excess_mortality.git
# After creating the coded, aggregated data according to the strata desired just
# join them in one file, example below (each dataframe within rbind() should 
# include the columns named "scope" and "category"): 
# brall <- rbind(brtotal, brcause, brstate, brsex, brage, brrace)
# 
################################################################################
# Linear Mixed Model
################################################################################

load(file = "Data/brall.RData")

# Inspect the data
tibble(brall)

# NOTE 3: 
# The "brall.RData" dataframe has 30,027 rows and 8 columns including historical 
# deaths for years 2010-2022 up to week 25.

# We will use data up to year 2021.
# Data up to 2020 are consolidated

# Our analysis consider total deaths and deaths dis-aggregated by several factors
# identified by the variable "scope": 
# Brazil (all-cause in Brazil), State (all-cause by state), Age (all-case by age 
# group), Cause (by cause of death), Sex (all-cause by sex), Race (all-cause 
# by race/color).

# scope=="Brazil" selects total deaths for the country as a whole

## The following command lines produce the 2020/2021 baseline for all-cause deaths 
# in the country (presented in Eq (4) and Figure 1 of the paper.
# The final model follows the descirbed step-by-step selection approach, thus, 
# at each fiting, results are checked and the model is updated until the final 
## parsimonious fitting.

# For model stratification (state, cause, sex, age, etc), similar 
# steps are followed, just change the "scope" level and "category" level
# as required to subset the relevant data and apply the modeling procedures in 
# that subset.

# Check brall names and scope coding
colnames(brall)
unique(brall$scope) 

# Check category coding in scope coding.
unique(brall$category) 

# To model for a scope, stratified by category, select the appropriated rows 
# using scope and category as exemplified below.

# Selecting the data for the Brazilian Total Deaths by epidemiological week
# and the years for fitting the baseline mode (2015-2020 up to week 9)
dados <- brall %>% filter(scope=="Brazil") %>% filter(yearepi>=2015&yearepi<=2020)
dados <- dados %>% filter(yearepi<=2019|(yearepi==2020&week<=9))

# Model 0: Linear fixed effects model with main effects of year and week both 
# declared as factors.
set.seed(4560)  # for reproducibility

dados$weekF <- factor(dados$week)

# Model 0
br0 <- lm(outcome~factor(yearepi) + weekF, data=dados)
anova(br0)

# Non-linear model to estimate the frequency of cyclic pattern (used in the FS 
# terms).
# Need starting value for omega
X <- model.matrix(outcome~factor(yearepi), data=dados)
T <- 26       # prior value for period 
wo <- 2*pi/T  # prior value for the frequency omega

n <- nrow(dados)       # n =  5*52+9 
dados$t <- c(1:n)/n    # time variable scaled for numerical convenience
outcome.nls <- nls(outcome~cbind(X, t, sin(w*week), cos(w*week),
                                 sin(2*w*week), cos(2*w*week)),
                   algorithm="plinear", start=list(w=wo), data=dados)
summary(outcome.nls)
coef(outcome.nls)[1]  # omega estimate
# Estimated period
Te <- 2*pi/coef(outcome.nls)[1]
Te

# Creating the columns for the Fourier Series terms (starting with two waves, 
# in case that is not enough, go back to line 96 and add another term for sin 
# and cos). We also checked what would be better: using the approch of estimating
# the frequency (or period of the FS) or using the usual period (T=52). So, below  
# S1, S2, C1, C2 refer to covariates with omega estimated and S11, S22, C11, C22 
# refer to period equal to 52 (the GEE uses this approach).

dados$omega <- coef(outcome.nls)[1]
d <- dados %>% 
  mutate(S1=sin(omega*week), 
         C1=cos(omega*week), S2=sin(2*omega*week), C2=cos(2*omega*week),
         S11=sin(2*pi*week/52), 
         C11=cos(2*pi*week/52), S22=sin(2*2*pi*week/52), C22=cos(2*2*pi*week/52))

# Linear regression model (Model 1)
br1 <- lm(outcome~factor(yearepi)+t+S1+C1+S2+C2, data=d)

# Test of lack-of-fit of Model 1 compared to Model 0
anova(br0,br1)

# basic diagnostics: all std.residuals between -3 and 3, reasonable variance 
# homogeneity
x11()
par(mfrow=c(2,2))
plot(br1)

# Selection of the mixed model
d <- groupedData(outcome ~  week|yearepi, data=d, order.groups=FALSE)

# Linear Mixed Model
# Model 2: mean model as in Model 1. Random effects of year and all regression 
# terms with unstructured D and R=sigma*I
br.LME2 <- lme(outcome~t+S1+C1+S2+C2, random=~S1+C1+S2+C2|yearepi, method="REML", 
               control=lmeControl(opt='optim',optimMethod = "BFGS"), data=d)
summary(br.LME2)

# Model 3: simplifying D to diagonal
br.LME3 <- update(br.LME2, random=pdDiag(~S1+C1+S2+C2), data=d, method="REML")

# Testing all covariances in D being null. AIC, BIC smaller for simpler model
# No correction on p-values since covariance has support on the real set
anova(br.LME2, br.LME3)

# The estimate of var component for S2 is small compared to the others.
summary(br.LME3)

# Checking its relevance 
# Model 4
br.LME4 <- update(br.LME3, random=pdDiag(~S1+C1+C2), data=d, method="REML")

# LR test H0: sigma2(S2)=0  (no p-value correction)
anova(br.LME3, br.LME4)

# test H0: sigma2(S2)=0  p-value correction (chi-square mixture)
testS2 <- varCompTest(br.LME3, br.LME4)

# Final Model 4
# Confidence Intervals for Model 4
intervals(br.LME4)

# Model 5: selecting R structure (serial correlation)
# AR(1)
br.LMEar1_1 <- update(br.LME4, correlation=corAR1(form=~week), 
                      control=lmeControl(opt='optim',optimMethod = "BFGS"),
                      data=d)
# Gauss
br.LMEG <- update(br.LME4, correlation=corGaus(form=~week), data=d)
# Spherical
br.LMES <- update(br.LME4, correlation=corSpher(form=~week), data=d)

# Comparing different structures for R
anova(br.LME4, br.LMEar1_1, br.LMEG, br.LMES)

# Best R structure is AR(1)
# Model 5
summary(br.LMEar1_1)

# Model 6 removing C2
br.LMEar1_2 <- update(br.LMEar1_1, random=pdDiag(~S1+C1), data=d)
anova(br.LMEar1_1, br.LMEar1_2)
varCompTest(br.LMEar1_1, br.LMEar1_2)

# The fixed effect part may be simplified.
# Model 7: refitting Model 6 by ML to test fixed effects
br.LMEar1_2ML <- update(br.LMEar1_2, method="ML", data=d)
summary(br.LMEar1_2ML)

# Simplifying
# Model 8
br.LMEar1_2ML_1 <- update(br.LMEar1_2ML, .~. -S2 - C2)

# Test for beta3=beta4=0
anova(br.LMEar1_2ML, br.LMEar1_2ML_1)

# Refit Model 8 by REML => Model 9 (final model)

br.LMEar1 <- lme(outcome~t+S1+C1, random=pdDiag(~S1+C1), 
                 correlation=corAR1(form=~week), method="REML", data=d)

# Confidence intervals
intervals(br.LMEar1)
summary(br.LMEar1)

# The final model
# save(br.LMEar1, file="FittedModels/br.LMEar1.RData")

################################################################################
# GEE Model
################################################################################
# Selection of the years for fitting the baseline mode (2015-2020 up to week 9)
# scope = cause

dadosgee <- brall %>% filter(scope=="Cause") %>% filter(yearepi>=2015&yearepi<=2020)
dadosgee <- dadosgee %>% filter(yearepi<2020|(yearepi==2020&week<=9)) %>% 
  droplevels() %>% mutate(idcause=factor(category))

levels(dadosgee$idcause) <- c(
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7"
)


# Including variable time (scaled)
n <- nrow(dadosgee)  # n = (5*52+9)*7  (7 cause groups)
dadosgee$t <- rep((1:(5*52+9)), 7)/(5*52+9)

Xcause <- model.matrix(outcome~-1+idcause, data=dadosgee)
colnames(Xcause) <- c("ca1", "ca2", "ca3", "ca4", "ca5", "ca6", "ca7")
dadosgee <- cbind(dadosgee, Xcause)
dadosgee <- dadosgee %>% mutate(S1=sin(2*pi*week/52),C1=cos(2*pi*week/52))
dadosgee$wave <- rep(1:269, 7)
CA.gee <- geeglm(outcome~-1+ca1+ca2+ca3+ca4+ca5+ca6+ca7+I(ca1*t)+I(ca2*t)+I(ca3*t)+
                   I(ca4*t)+I(ca5*t)+I(ca6*t)+I(ca7*t) +
                   I(ca1*S1)+I(ca2*S1)+I(ca3*S1)+I(ca4*S1)+
                   I(ca5*S1)+I(ca6*S1)+I(ca7*S1) +
                   I(ca1*C1)+I(ca2*C1)+I(ca3*C1)+I(ca4*C1)+
                   I(ca5*C1)+I(ca6*C1)+I(ca7*C1), id=idcause, waves=wave,
                 family=poisson(link="log"),
                 corstr = "ar1", scale.fix=FALSE, data=dadosgee)
summary(CA.gee)

# Estimating baseline total deaths from the gee
FITgee <- c(apply(matrix(fitted(CA.gee, type="response"),nc=7),1,sum))

################################################################################
# Comparing fitted curves by LMM and GEE: Fig 1
################################################################################

# load("FittedModels/br.LMEar1.RData")

# Fig 1
x11()
plot(fitted(br.LMEar1)[1:269], type="l", ylim=c(22000, 29000), col=2, bty="n",
     ylab="Total deaths", lwd=2, xlab="Time", xaxt="n")
lines(d$outcome[1:269], col=1)
lines(FITgee, col=4, lwd=2)
axis(side=1, at=seq(1,(269+6), by=6), cex.axis=.7)
abline(v=52, col="gray")
abline(v=(2*52), col="gray")
abline(v=(3*52), col="gray")
abline(v=(4*52), col="gray")
abline(v=(5*52), col="gray")
legend(1,29000, legend="Observed", lty=1, col=1, bty="n")
legend(1,28600, legend="Fitted LMM", lty=1, col=2, bty="n", lwd=2)
legend(1,28200, legend="Fitted GEE", lty=1, col=4, bty="n", lwd=2)
mtext(side=1, at=26, "2015", line=2)
mtext(side=1, at=79, "2016", line=2)
mtext(side=1, at=130, "2017", line=2)
mtext(side=1, at=184, "2018", line=2)
mtext(side=1, at=235, "2019", line=2)



