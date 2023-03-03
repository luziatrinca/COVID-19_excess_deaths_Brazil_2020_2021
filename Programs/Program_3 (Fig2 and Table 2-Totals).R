################################################################################
# 2020/2021 Excess deaths - all-cause deaths in Brazil - Table 2 and Fig 2
library(tidyverse)
library(readxl)
library(nlme)

# Set the path where the files are saved
setwd("C:/Review2/Code/")
source("Programs/Rfunctions.R") # Convenience functions for all the calculations
                                # required.

# load the data and the model fit object
load("Data/brall.RData")
load("FittedModels/br.LMEa1.RData")

# Subset appropriate rows
brtotal <- brall %>% filter(scope=="Brazil")
d <- brtotal %>% filter(category == "Brazil")
d <- d %>% filter(yearepi>=2015&yearepi<=2021)
dim(d)  # should have 365 rows and 8 columns

n <- nrow(d)   # n =  6*52+53 (215-219 and 2021 = 52 weeks 2020 = 53 weeks)
d$t <- c(1:n)/(5*52+9) # denominator for scaling time as in the modeling (Program1)

preddata <- utilityF(d, omega=0.1598, br.LMEar1)
preddata2020_br <- preddata$pred[1:53,]
preddata2021_br <- preddata$pred[-c(1:53),]

# Accumulated statistics
Total <- preddata$totals

# Data frame for the graph
dgraph <- preddata$d

# Colors
colors <- c("2015-2019 observed "="grey55", 
            "                   "="white",  
            "2020 forecast"="purple", 
            "2021 forecast"="red", 
            "2020 observed"="purple", 
            "2021 observed"="red",
            "2020 forecast+covid"="black",
            "2021 forecast+covid"="blue",
            "2020 95% PI"="gray20",
            "2021 95% PI"="green")

# Lines and text
legsize <- 10 
titsize <- 8
linesize <- .7
linesizeh <- .3
axsize <- 10
axtitlesize <- 11
guidesize <- c(linesizeh,1,1,1,rep(linesize,4),7,7)
a <- 0.30
guidealpha <- c(rep(1,8),.08,.08)

Fig2 = ggplot(dgraph, aes(x=week))+
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2020_br,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="grey20"     #fill color
  )  + geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2021_br,
                   alpha=a,       #transparency
                   linetype=1,      #solid, dashed or other line types
                   size=0,          #border line size
                   fill="green"     #fill color
  )+
  
  geom_line(aes(y=outcome, color="2015-2019 observed ", group=yearepi), size=linesizeh, linetype=1) +
  geom_line(aes(x=week, color="2020 forecast", y=predM), data=preddata2020_br, size=1, linetype=3) + 
  geom_line(aes(x=week, color="2021 forecast", y=predM), data=preddata2021_br, size=1, linetype=3) + 
  geom_line(aes(x=week, color="2020 observed", y=outcome), data=preddata2020_br, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2021 observed", y=outcome), data=preddata2021_br, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2020 forecast+covid", y=deathP), data=preddata2020_br, size=linesize, linetype=6)+
  geom_line(aes(x=week, color="2021 forecast+covid", y=deathP), data=preddata2021_br, size=linesize, linetype=6) +
  labs(x = "Epidemiological week", y = "Number of deaths") +
  scale_x_continuous(breaks=seq(1,53,2)) +
  scale_y_continuous(breaks=seq(22500,53000,length=6)) +
  guides(color=guide_legend(override.aes = list(linetype=c(1,1,3,3,1,1,6,6,1,1),size=guidesize, alpha=guidealpha)))+
  theme_bw() +
  theme(axis.text = element_text(size = axsize), 
        axis.title=element_text(size=axtitlesize),
        panel.border = element_blank(), 
        plot.title = element_text(hjust=0.5, size = titsize), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legsize), legend.title = element_blank(),
        legend.position="bottom", legend.box = "", 
        axis.line = element_line(color = 'black')) +
  scale_color_manual(values = colors)+ggtitle("") 

x11()
Fig2

# Fig2

# Saving the graph
#ggsave(filename = "Fig2.eps", width = 210, height = 130, units = "mm",
#       plot = print(Fig2), dpi=300,
#       device = cairo_ps)
#ggsave(filename = "Fig2.pdf", width = 210, height = 130, units = "mm", Fig2)
