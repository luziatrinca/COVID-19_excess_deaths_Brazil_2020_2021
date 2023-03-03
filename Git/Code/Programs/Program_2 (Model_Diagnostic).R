# This code provides the graph diagnostics for LMM
# Download the required function from:
# http://www.ime.usp.br/~jmsinger/lmmdiagnostics.zip
# NOTE:
# Before loading it to R, make the following change (required because other  
# packages were updated): remove the option "LINPACK=F" in function svd within 
# sqrt.matrix() function (here the corrected code was renamed residdiag_nlmeM.R)

# Set the path where the files are saved
setwd("C:/Review2/Code/")
source("residdiag_nlmeM.R", encoding = 'UTF-8')

# Packages required
library(tidyverse)
library(nlme)
library(ggplot2)
library(qqplotr)
library(ggpubr)
library(gridExtra)

# load the object from the model fitting
load("FittedModel/br.LMEar1.RData")

br.final <- residdiag.nlme(br.LMEar1, limit=2, plotid=1:16)

# For personalized plots
# Marginal Residuals
d1 <- as.data.frame(br.final$std.marginal.residuals)
d1$fitted <- fitted(br.LMEar1, level=0)
g1 <- ggplot(d1) +
  geom_point(aes(y=Predicted, x=fitted), size=0.8) + 
  labs(x = "Marginal fitted response", y = "Standardized marginal residuals") +
  geom_abline(intercept=-2,slope=0, linetype=3) +
  geom_abline(intercept= 2,slope=0, linetype=3) +
  geom_abline(intercept= 0,slope=0, linetype=1) +
  theme_bw() +
  theme(axis.title = element_text(size = 8), panel.border = element_blank(), 
        axis.line = element_line(color = 'black'),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

# Conditional Residuals
d1 <- as.data.frame(br.final$std.conditional.residuals)
d1$fitted <- fitted(br.LMEar1, level=1)
g2 <- ggplot(d1)+ geom_point(aes(y=Predicted, x=fitted), size=0.8) + 
  labs(x = "Conditional fitted response", y = "Standardized conditional residuals") +
  geom_abline(intercept=-2,slope=0, linetype=3) +
  geom_abline(intercept= 2,slope=0, linetype=3) +
  geom_abline(intercept= 0,slope=0, linetype=1) +
  theme_bw() +
  theme(axis.title = element_text(size = 8), panel.border = element_blank(), 
        axis.line = element_line(color = 'black'),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

# QQ-plot for least confounded residuals
d1 <- as.data.frame(br.final$least.confounded.residuals)
g3 <- ggplot(data = d1, mapping = aes(sample = l.c.r)) +
  stat_qq_band(alpha=0.4) +
  stat_qq_point(size=0.8) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Standard Normal quantiles", y = "Std. least conf. cond. residuals")
g3

# QQ-plot Mahalanobis distance
di <- "chisq"
dp <- list(df=5)
d1 <- as.data.frame(br.final$mahalanobis.distance)
g4 <- ggplot(data = d1, mapping = aes(sample = md)) +
  stat_qq_band(distribution = di, dparams = dp, alpha=0.4) +
  stat_qq_point(distribution = di, dparams = dp, size=0.8) +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Chi-squared quantiles", y = "Mahalanobis distance quantiles")
g4

# QQ-plot Lesaffre & Verbeke measure
q1 <- quantile(br.final$lesaffreverbeke.measure[,2],.25)
q3 <- quantile(br.final$lesaffreverbeke.measure[,2],.75)
dq <- q3-q1

d1 <- as.data.frame(br.final$lesaffreverbeke.measure)
d1 <- d1 %>% mutate(Year=Subject+2014)
g5 <- ggplot(d1) +
  geom_point(aes(y=LV.m, x=Year), size=0.8) + ylim(0, 2) +
  labs(x = "Year", y = "Modified Laseffre-Verbeck measure") +
  scale_x_continuous(breaks=seq(2015,2020,1)) + 
  geom_abline(intercept=q3+1.5*dq,slope=0, linetype=3) +
  theme(axis.title = element_text(size = 8), panel.border = element_blank(), 
        axis.line = element_line(color = 'black'), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
g5

# Cook's distance
d1 <- as.data.frame(RD_AR1$cook.conditional.distance)
d1$x <- 1:nrow(d1)
g6 <- ggplot(d1)+ geom_point(aes(y=DCCond, x=x), size=0.8) + 
  labs(x = "Observation index", y = "Cook's conditional distance") +
  theme_bw() +
  theme(axis.title = element_text(size = 8), panel.border = element_blank(), 
        axis.line = element_line(color = 'black'), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

# There are other graphs available, these 5 are the essentials.

library(ggpubr)
library(gridExtra)

S1Fig = ggarrange(g1, g2, g3, g4, g5, g6, ncol=2, nrow=3, common.legend = FALSE)
grid.arrange(S1Fig)

# For saving the graphs
#dev.off()
#ggsave(filename = "S1Fig.eps", width = 210, height = 150, units = "mm",
#       plot = print(S1Fig),
#       device = cairo_ps)

#ggsave(filename = "S1Fig.pdf", width = 210, height = 150, units = "mm",
#       plot = print(S1Fig))
