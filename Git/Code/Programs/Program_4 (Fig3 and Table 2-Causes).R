# 2020/2021 Excess deaths by Cause - Table 2 and Fig 3

library(tidyverse)
library(readxl)
library(nlme)
library(ggpubr)
library(gridExtra)

# Set the path where the files are saved
setwd("C:/Review2/Code/")
source("Programs/Rfunctions.R") # Convenience functions for all the calculations
                                # required.

# Get model info for the category desired
fitinfo <- read_excel("FittedModels/Fit_Info.xlsx")
i <- 2 # row 2 refers to Cardiovascular diseases
fitinfo[2,]  # Just checking
# load the model fit object (name as in last column of fitinfo file)
load("FittedModels/Causes/CAR.LMEar1.RData")

# load the data and the model fit object
load("Data/brall.RData")

# Subset appropriate rows
cause <- fitinfo$category[2]
cause
dados <- brall %>% filter(scope=="Cause"&category==cause) %>% filter(yearepi>=2015&yearepi<=2021) 
dim(dados)    # should have 365 ros and 8 columns

n <- nrow(dados)   
dados$t <- c(1:n)/(5*52+9)  # scaling time

preddata <- utilityF(dados, omega=fitinfo$omega[2], CAR.LMEar1)
preddata2020_Cardio <- preddata$pred[1:53,]
preddata2021_Cardio <- preddata$pred[-c(1:53),]

# Statistics for Table 2
TotalCauses <- preddata$totals

# Data frame for the graph
dgraph <- preddata$d

# Colors
colors <- c("2015-2019 observed "="grey55", 
            "                   "="white",  
            "2020 forecast"="purple", 
            "2021 forecast"="red", 
            "2020 observed"="purple", 
            "2021 observed"="red",
            "2020 95% PI"="grey20",
            "2021 95% PI"="green")

# Lines and text
legsize <- 10 
titsize <- 10
linesize <- .7
linesizeh <- .3
axsize <- 7
axtitlesize <- 11
guidesize <- c(linesizeh,1,1,1,rep(linesize,2),7,7)
a <- 0.30
guidealpha <- c(rep(1,6),a,a)

p_Cardio  = ggplot(dgraph, aes(x=week))+
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2021_Cardio,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line type
              size=0,          #border line size
              fill="green") +   #fill color
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2020_Cardio,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="grey20"     #fill color
  )  +  
  geom_line(aes(y=outcome, color="2015-2019 obs ", group=yearepi), size=linesizeh, linetype=1) +
  geom_line(aes(x=week, color="2020 observed", y=outcome), data=preddata2020_Cardio, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2020 forecast", y=predM), data=preddata2020_Cardio, size=linesize, linetype=3) + 
  geom_line(aes(x=week, color="2021 observed", y=outcome), data=preddata2021_Cardio, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2021 forecast", y=predM), data=preddata2021_Cardio, size=linesize, linetype=3) + 
  labs(x = "Epidemiological week", y = "Number of deaths") +
  scale_x_continuous(breaks=seq(1,53,2)) +
  scale_y_continuous(breaks=seq(4000,9000,450)) +
  guides(color=guide_legend(override.aes = list(linetype=c(1,1,3,3,1,1,1,1),size=guidesize, alpha=guidealpha)))+
  theme_bw() +
  theme(axis.text = element_text(size = axsize), 
        axis.title=element_text(size=axtitlesize),
        panel.border = element_blank(), 
        plot.title = element_text(hjust=0.5, size = titsize), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legsize), legend.title = element_blank(),
        legend.position="", legend.box = "", 
        axis.line = element_line(color = 'black')) +
  scale_color_manual(values = colors)+ 
  ggtitle("Cardiovascular Diseases")
p_Cardio

###############################################################################
# Others diseases
cause <- fitinfo$category[8]
cause
dados <- brall %>% filter(scope=="Cause"&category==cause) %>% filter(yearepi>=2015&yearepi<=2021) 
dim(dados)    # should have 365 ros and 8 columns

n <- nrow(dados)   
dados$t <- c(1:n)/(5*52+9)  # scaling time

# Load the model fit object
load("FittedModels/Causes/od.LMEar1.RData")
preddata <- utilityF(dados, omega=fitinfo$omega[8], od.LMEar1)
preddata2020_others <- preddata$pred[1:53,]
preddata2021_others <- preddata$pred[-c(1:53),]

# Statistics for Table 2
TotalCauses <- rbind(TotalCauses,preddata$totals)

# Data frame for the graph
dgraph <- preddata$d

p_others = ggplot(dgraph, aes(x=week))+
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2021_others,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="green") +   #fill color
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2020_others,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="grey20"     #fill color
  )  +  
  geom_line(aes(y=outcome, color="2015-2019 obs ", group=yearepi), size=linesizeh, linetype=1) +
  geom_line(aes(x=week, color="2020 observed", y=outcome), data=preddata2020_others, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2020 forecast", y=predM), data=preddata2020_others, size=linesize, linetype=3) + 
  geom_line(aes(x=week, color="2021 observed", y=outcome), data=preddata2021_others, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2021 forecast", y=predM), data=preddata2021_others, size=linesize, linetype=3) + 
  scale_x_continuous(breaks=seq(1,53,2)) +
  scale_y_continuous(breaks=seq(4840,7400,length=6)) +
  labs(x = "Epidemiological week", y = "Number of deaths") +
  guides(color=guide_legend(override.aes = list(linetype=c(1,1,3,3,1,1,1,1),size=guidesize, alpha=guidealpha)))+
  theme_bw() +
  theme(axis.text = element_text(size = axsize), 
        axis.title=element_text(size=axtitlesize),
        panel.border = element_blank(), 
        plot.title = element_text(hjust=0.5, size = titsize), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legsize), legend.title = element_blank(),
        legend.position="", legend.box = "", 
        axis.line = element_line(color = 'black')) +
  scale_color_manual(values = colors) + ggtitle("Other Diseases")
p_others

#################################
# Neoplasms
cause <- fitinfo$category[4]
dados <- brall %>% filter(scope=="Cause"&category==cause) %>% filter(yearepi>=2015&yearepi<=2021) 
dim(dados)    # should have 365 ros and 8 columns

n <- nrow(dados)   
dados$t <- c(1:n)/(5*52+9)  # scaling time

# Load the model fit object
load("FittedModels/Causes/neo.LMEar1FS.RData")
preddata <- utilityF(dados, omega=fitinfo$omega[4], neo.LMEar1FS)
preddata2020_Neoplasms <- preddata$pred[1:53,]
preddata2021_Neoplasms <- preddata$pred[-c(1:53),]

# Statistics for Table 2
TotalCauses <- rbind(TotalCauses,preddata$totals)

# Data frame for the graph
dgraph <- preddata$d

p_Neoplasms = ggplot(dgraph, aes(x=week))+
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2021_Neoplasms,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="green") +  
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2020_Neoplasms,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="grey20"     #fill color
  )  +   #fill color
  geom_line(aes(y=outcome, color="2015-2019 obs ", group=yearepi), size=linesizeh, linetype=1) +
  geom_line(aes(x=week, color="2020 observed", y=outcome), data=preddata2020_Neoplasms, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2020 forecast", y=predM), data=preddata2020_Neoplasms, size=linesize, linetype=3) +
  geom_line(aes(x=week, color="2021 observed", y=outcome), data=preddata2021_Neoplasms, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2021 forecast", y=predM), data=preddata2021_Neoplasms, size=linesize, linetype=3) + 
  scale_x_continuous(breaks=seq(1,53,2)) +
  scale_y_continuous(breaks=seq(3800,5050,length=6)) +
  labs(x = "Epidemiological week", y = "Number of deaths") +
  guides(color=guide_legend(override.aes = list(linetype=c(1,1,3,3,1,1,1,1),size=guidesize, alpha=guidealpha)))+
  theme_bw() +
  theme(axis.text = element_text(size = axsize), 
        axis.title=element_text(size=axtitlesize),
        panel.border = element_blank(), 
        plot.title = element_text(hjust=0.5, size = titsize), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legsize), legend.title = element_blank(),
        legend.position="", legend.box = "", 
        axis.line = element_line(color = 'black')) +
  scale_color_manual(values = colors) + ggtitle("Neoplasms")
p_Neoplasms



###############################################################################
# Respiratory
cause <- fitinfo$category[5]
dados <- brall %>% filter(scope=="Cause"&category==cause) %>% filter(yearepi>=2015&yearepi<=2021) 
dim(dados)    # should have 365 ros and 8 columns

n <- nrow(dados)   
dados$t <- c(1:n)/(5*52+9)  # scaling time

# Load the model fit object
load("FittedModels/Causes/respd.LMEar1.RData")
preddata <- utilityF(dados, omega=fitinfo$omega[5], respd.LMEar1)
preddata2020_Respiratory <- preddata$pred[1:53,]
preddata2021_Respiratory <- preddata$pred[-c(1:53),]
# Statistics for Table 2
TotalCauses <- rbind(TotalCauses,preddata$totals)

# Data frame for the graph
dgraph <- preddata$d

p_Respiratory = ggplot(dgraph, aes(x=week))+
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2021_Respiratory,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="green") +   #fill color
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2020_Respiratory,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="grey20"     #fill color
  )  + 
  geom_line(aes(y=outcome, color="2015-2019 obs ", group=yearepi), size=linesizeh, linetype=1) +
  geom_line(aes(x=week, color="2020 observed", y=outcome), data=preddata2020_Respiratory, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2020 forecast", y=predM), data=preddata2020_Respiratory, size=linesize, linetype=3) + 
  geom_line(aes(x=week, color="2021 observed", y=outcome), data=preddata2021_Respiratory, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2021 forecast", y=predM), data=preddata2021_Respiratory, size=linesize, linetype=3) + 
  scale_x_continuous(breaks=seq(1,53,2)) +
  scale_y_continuous(breaks=seq(2200,4700,length=6)) +
  labs(x = "Epidemiological week", y = "Number of deaths") +
  guides(color=guide_legend(override.aes = list(linetype=c(1,1,3,3,1,1,1,1),size=guidesize, alpha=guidealpha)))+
  theme_bw() +
  theme(axis.text = element_text(size = axsize), 
        axis.title=element_text(size=axtitlesize),
        panel.border = element_blank(), 
        plot.title = element_text(hjust=0.5, size = titsize), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legsize), legend.title = element_blank(),
        legend.position="", legend.box = "", 
        axis.line = element_line(color = 'black')) +
  scale_color_manual(values = colors) + ggtitle("Respiratory Diseases")
p_Respiratory

################################################################################
#External
cause <- fitinfo$category[7]
dados <- brall %>% filter(scope=="Cause"&category==cause) %>% filter(yearepi>=2015&yearepi<=2021) 
dim(dados)    # should have 365 ros and 8 columns

n <- nrow(dados)   
dados$t <- c(1:n)/(5*52+9)  # scaling time

# Load the model fit object
load("FittedModels/Causes/ext.LMEar1.RData")
preddata <- utilityF(dados, omega=fitinfo$omega[7], ext.LMEar1)
preddata2020_External <- preddata$pred[1:53,]
preddata2021_External <- preddata$pred[-c(1:53),]

# Statistics for Table 2
TotalCauses <- rbind(TotalCauses,preddata$totals)

# Data frame for the graph
dgraph <- preddata$d

p_External =  ggplot(dgraph, aes(x=week))+
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2021_External,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="green") +  
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2020_External,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="grey20"     #fill color
  )  +  #fill color
  geom_line(aes(y=outcome, color="2005-2019 obs ", group=yearepi), size=linesizeh, linetype=1) +
  geom_line(aes(x=week, color="2020 observed", y=outcome), data=preddata2020_External, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2020 forecast", y=predM), data=preddata2020_External, size=linesize, linetype=3) + 
  geom_line(aes(x=week, color="2021 observed", y=outcome), data=preddata2021_External, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2021 forecast", y=predM), data=preddata2021_External, size=linesize, linetype=3) + 
  labs(x = "Epidemiological week", y = "Number of deaths")+
  scale_x_continuous(breaks=seq(1,53,2)) +
  scale_y_continuous(breaks=seq(2500,3500,length=6)) +
  guides(color=guide_legend(override.aes = list(linetype=c(1,1,3,3,1,1,1,1),size=guidesize, alpha=guidealpha)))+
  theme_bw() +
  theme(axis.text = element_text(size = axsize), 
        axis.title=element_text(size=axtitlesize),
        panel.border = element_blank(), 
        plot.title = element_text(hjust=0.5, size = titsize), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legsize), legend.title = element_blank(),
        legend.position="", legend.box = "", 
        axis.line = element_line(color = 'black')) +
  scale_color_manual(values = colors)+  ggtitle("External Causes")
p_External

########################################################
#Ill-defined causes
cause <- fitinfo$category[6]
cause
dados <- brall %>% filter(scope=="Cause"&category==cause) %>% filter(yearepi>=2015&yearepi<=2021) 
dim(dados)    # should have 365 ros and 8 columns

n <- nrow(dados)   
dados$t <- c(1:n)/(5*52+9)  # scaling time

# Load the model fit object
load("FittedModels/Causes/id.LMEar1.RData")
preddata <- utilityF(dados, omega=fitinfo$omega[6], id.LMEar1)
preddata2020_ILLD <- preddata$pred[1:53,]
preddata2021_ILLD <- preddata$pred[-c(1:53),]

# Statistics for Table 2
TotalCauses <- rbind(TotalCauses,preddata$totals)

# Data frame for the graph
dgraph <- preddata$d

p_ILLD = ggplot(dgraph, aes(x=week))+
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2021_ILLD,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="green") +
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2020_ILLD,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="grey20"     #fill color
  )  +    #fill color
  geom_line(aes(y=outcome, color="2005-2019 obs ", group=yearepi), size=linesizeh, linetype=1) +
  geom_line(aes(x=week, color="2020 observed", y=outcome), data=preddata2020_ILLD, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2020 forecast", y=predM), data=preddata2020_ILLD, size=linesize, linetype=3) + 
  geom_line(aes(x=week, color="2021 observed", y=outcome), data=preddata2021_ILLD, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2021 forecast", y=predM), data=preddata2021_ILLD, size=linesize, linetype=3) + 
  scale_x_continuous(breaks=seq(1,53,2)) +
  scale_y_continuous(breaks=seq(1200,2500,length=6)) +
  labs(x = "Epidemiological week", y = "Number of deaths") +
  guides(color=guide_legend(override.aes = list(linetype=c(1,1,3,3,1,1,1,1),size=guidesize, alpha=guidealpha)))+
  theme_bw() +
  theme(axis.text = element_text(size = axsize), 
        axis.title=element_text(size=axtitlesize),
        panel.border = element_blank(), 
        plot.title = element_text(hjust=0.5, size = titsize), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legsize), legend.title = element_blank(),
        legend.position="", legend.box = "", 
        axis.line = element_line(color = 'black')) +
  scale_color_manual(values = colors) + ggtitle("Ill-defined Causes")
p_ILLD

#########################################################################
# Others Infectious
cause <- fitinfo$category[3]
cause
dados <- brall %>% filter(scope=="Cause"&category==cause) %>% filter(yearepi>=2015&yearepi<=2021) 
dim(dados)    # should have 365 ros and 8 columns

n <- nrow(dados)   
dados$t <- c(1:n)/(5*52+9)  # scaling time

# Load the model fit object
load("FittedModels/Causes/oid.LMEar1_no_t.RData")
preddata <- utilityF(dados, omega=fitinfo$omega[3], oid.LMEar1_no_t)
preddata2020_outras <- preddata$pred[1:53,]
preddata2021_outras <- preddata$pred[-c(1:53),]

# Statistics for Table 2
TotalCauses <- rbind(TotalCauses,preddata$totals)

# Data frame for the graph
dgraph <- preddata$d

p_outras = ggplot(dgraph, aes(x=week))+
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2021_outras,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="green") +   #fill color
  geom_ribbon(aes(ymin=deathP-2*se, ymax=deathP+2*se), data=preddata2020_outras,
              alpha=a,       #transparency
              linetype=1,      #solid, dashed or other line types
              size=0,          #border line size
              fill="grey20"     #fill color
  )  + 
  geom_line(aes(y=outcome, color="2005-2019 obs ", group=yearepi), size=linesizeh, linetype=1) +
  geom_line(aes(x=week, color="2020 observed", y=outcome), data=preddata2020_outras, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2020 forecast", y=predM), data=preddata2020_outras, size=linesize, linetype=3) +  
  geom_line(aes(x=week, color="2021 observed", y=outcome), data=preddata2021_outras, size=linesize, linetype=1) + 
  geom_line(aes(x=week, color="2021 forecast", y=predM), data=preddata2021_outras, size=linesize, linetype=3) + 
  labs(x = "Epidemiological week", y = "Number of deaths") +
  scale_x_continuous(breaks=seq(1,53,2)) +
  scale_y_continuous(breaks=seq(900,1380,length=6)) +
  guides(color=guide_legend(override.aes = list(linetype=c(1,1,3,3,1,1,1,1),size=guidesize, alpha=guidealpha)))+
  theme_bw() +
  theme(axis.text = element_text(size = axsize), 
        axis.title=element_text(size=axtitlesize),
        panel.border = element_blank(), 
        plot.title = element_text(hjust=0.5, size = titsize), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legsize), legend.title = element_blank(),
        legend.position="", legend.box = "", 
        axis.line = element_line(color = 'black')) +
  scale_color_manual(values = colors)+ ggtitle("Other Infectious Diseases")
p_outras

# COVID_19
dados <- brall %>% filter(scope=="Cause"&category=="COVID-19")
dim(dados)
dados2020 <- dados %>% filter(yearepi==2020)
dados2021 <- dados %>% filter(yearepi==2021)
p_covid = ggplot(dados, aes(x=week) )+
  geom_line(aes(x=week, colour="2020 observed",y=covid),data=dados2020, size=linesize, linetype=1) +
  geom_line(aes(x=week, colour="2021 observed", y=covid),data=dados2021, size=linesize, linetype=1) +
  labs(x = "Epidemiological week", y = "Number of deaths") +
  scale_x_continuous(breaks=seq(1,53,2)) +
  scale_y_continuous(breaks=seq(0,25000,length=6)) +
  guides(color=guide_legend(override.aes = list(linetype=c(1,1,3,3,1,1,1,1),size=guidesize, alpha=guidealpha)))+
  theme_bw() +
  theme(axis.text = element_text(size = axsize), 
        axis.title=element_text(size=axtitlesize),
        panel.border = element_blank(), 
        plot.title = element_text(hjust=0.5, size = titsize), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legsize), legend.title = element_blank(),
        legend.position="", legend.box = "", 
        axis.line = element_line(color = 'black')) +
  scale_color_manual(values = colors) + ggtitle("COVID - 19")
p_covid

# Organizing the Table

TotalStatsCause <- TotalCauses[order(TotalCauses$yearepi),]
TotalStatsCause <- TotalStatsCause %>% relocate(category,  .after = yearepi) 
TotalStatsCause <- TotalStatsCause %>% relocate(PScore,  .after = IPb)
TotalStatsCause <- TotalStatsCause %>% relocate(ratioEC,  .after = PScore)
TotalStatsCause <- TotalStatsCause %>% relocate(SEsum,  .after = ratioEC)

TotalStatsCause[,-c(2, 10:11)] <- round(TotalStatsCause[,-c(2, 10:11)],0) 
TotalStatsCause

#detach( "package:MASS", unload = TRUE )
TotalStatsCause <- TotalStatsCause %>% select(yearepi, category, out, predM, excessD, IPa, IPb, PScore,
                           ratioEC) 
library(xtable)
xtable(TotalStatsCause, type="latex")

# Fig 3
Fig3 = ggarrange(p_covid,p_Cardio,p_others,p_Neoplasms,p_Respiratory,
                   p_External,p_ILLD, p_outras,ncol=2, nrow=4, common.legend = TRUE, legend="bottom")

grid.arrange(Fig3)

#ggsave(filename = "Fig3.eps", width = 210, height = 297, units = "mm",
#       plot = print(Fig3), dpi=600, 
#       device = cairo_ps)

#ggsave(filename = "Fig3.pdf", width = 210, height = 297, units = "mm",
#       plot = print(Fig3))
