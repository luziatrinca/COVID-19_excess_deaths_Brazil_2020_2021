# This is a collection of functions useful for obtaining the statistics 
# presented in tables and figures in the paper

# Summaries of results from "accuracy" function
Summ <- function(x) {c(min(x), mean(x), median(x), max(x))}

# Function to calculate several accuracy measures for the models in Table 1
accuracy <- function(dataprev, dataprevC=NULL, model_type, model)
{
  year <- 2015:2019
  res <- matrix(0,nr=5,ncol=6)
  colnames(res) <- c("ME", "MAE","RMSE", "RMSE2", "MAPE", "MPE")
  for(i in 1:5)
  { 
    if(model=="gee")
    {
      dpi <- dataprevC %>% filter(yearepi>=(year[i]-5))
      dpif <- dpi %>% filter(yearepi<year[i]|(yearepi==year[i]&week<=9))
    } 
    if(model_type=="null")
    { 
      dpi <- dataprev %>% filter(yearepi>=(year[i]-5))
      dpif <- dpi %>% filter(yearepi<year[i]|(yearepi==year[i]&week<=9))
      if(model=="5year")
      {
        fore <- c(tapply(dpif$outcome[1:(5*52+9)],dpif$week[1:(5*52+9)],mean))[-c(1:9)]
        pred <- dataprev[dataprev$yearepi==year[i]&dataprev$week>9,]
        PE <- pred$outcome-fore
      }
      if(model=="m0") 
      {
        mf <- lm(outcome~factor(yearepi)+factor(week), data=dpif)
        pred <- dataprev[dataprev$yearepi==year[i]&dataprev$week>9,]
        fore <- predict(mf, pred)
        PE <- pred$outcome-fore
      }
    }
    if(model_type=="mixed"&model=="gee")
    {
      dpif$t <- rep((1:(5*52+9)), 7)/(5*52+9)
      Xcause <- model.matrix(outcome~-1+idcause, data=dpif)
      colnames(Xcause) <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7")
      Xfs <- cbind(sin(2*pi*dpif$week/52),cos(2*pi*dpif$week/52))
      colnames(Xfs) <- c("sin", "cos")
      dpif <- cbind(dpif, Xcause, Xfs)
      mf <- geeglm(outcome~-1 + idcause + I(c1*t)+I(c2*t)+I(c3*t)+
                     I(c4*t)+I(c5*t)+I(c6*t)+I(c7*t) +
                     I(c1*sin)+I(c2*sin)+I(c3*sin)+I(c4*sin)+
                     I(c5*sin)+I(c6*sin)+I(c7*sin) +
                     I(c1*cos)+I(c2*cos)+I(c3*cos)+I(c4*cos)+
                     I(c5*cos)+I(c6*cos)+I(c7*cos), id=idcause, 
                   family=poisson(link="log"), 
                   corstr = "ar1", scale.fix=FALSE, data=dpif)
      
      predC <- dataprevC %>% filter(yearepi==year[i]&week>9)
      predC$t <- rep(c(270:(269+43)), 7)/(5*52+9)
      Xcause <- model.matrix(outcome~-1+idcause, data=predC)
      colnames(Xcause) <- c("c1", "c2", "c3", "c4", "c5", "c6", "c7")
      Xfs <- cbind(sin(2*pi*predC$week/52),cos(2*pi*predC$week/52))
      colnames(Xfs) <- c("sin", "cos")
      predC <- cbind(predC, Xcause, Xfs)
      fore <- c(apply(matrix(predict(mf, predC, type="response"), nc=7),1, sum))
      pred <- dataprev %>% filter(yearepi==year[i]&week>9)
      PE <- pred$outcome-fore
    }
    if(model_type=="mixed"&(model %in% c("m1", "m2", "m5", "m6", "m9", "lme2"))) 
    {
      dpi <- dataprev %>% filter(yearepi>=(year[i]-5))
      dpif <- dpi %>% filter(yearepi<year[i]|(yearepi==year[i]&week<=9))
      X <- model.matrix(outcome~factor(yearepi), data=dpif)
      T <- 26       # prior value for period
      wo <- 2*pi/T  # prior value for the frequency omega

      outcome.nls <- nls(outcome~cbind(X,sin(w*week), cos(w*week),
                                       sin(2*w*week), cos(2*w*week)),
                         algorithm="plinear", start=list(w=wo), data=dpif)
      dataprev$omega <- coef(outcome.nls)[1]
      dataprev <- dataprev %>% mutate(S1=sin(omega*week), 
               C1=cos(omega*week), S2=sin(2*omega*week), C2=cos(2*omega*week))
      dpi <- dataprev %>% filter(yearepi>=(year[i]-5))
      dpif <- dpi %>% filter(yearepi<year[i]|(yearepi==year[i]&week<=9))
      dpif$t <- c(1:(5*52+9))/(5*52+9)
      dpif <- groupedData(outcome ~  week|yearepi, data=dpif, order.groups=FALSE)
      if(model=="m1") 
      {
        mf <- lm(outcome~ factor(yearepi) + t +S1+C1+S2+C2, data=dpif)
        pred <- dataprev[dataprev$yearepi==year[i]&dataprev$week>9,]
        pred$t <- c(270:(269+43))/(5*52+9)
        fore <- predict(mf, pred)
        PE <- pred$outcome - fore
      }
      dpif <- groupedData(outcome ~  week|yearepi, data=dpif, 
                          order.groups=FALSE)
      if(model=="m5")
      {
        # br.LMEar1_1
        mf <- lme(outcome~t+S1+C1+S2+C2, data=dpif, random=pdDiag(~S1+C1+C2), 
                  correlation=corAR1(form=~week), method="REML")
        pred <- dataprev %>% filter(yearepi==year[i]&week>9)
        pred$t <- c(270:(269+43))/(5*52+9)
        fore <- predict(mf, pred, level=1)
        PE <- pred$outcome - fore
      }
      if(model=="m6")
      {
        # br.LMEar1_2
        mf <- lme(outcome~t+S1+C1+S2+C2, data=dpif, random=pdDiag(~S1+C1), 
                  correlation=corAR1(form=~week), method="REML")
        pred <- dataprev %>% filter(yearepi==year[i]&week>9)
        pred$t <- c(270:(269+43))/(5*52+9)
        fore <- predict(mf, pred, level=1)
        PE <- pred$outcome - fore
      }
      if(model=="m9")
      {
        # br.LMEar1 final
        mf <- lme(outcome~t+S1+C1, data=dpif, random=pdDiag(~S1+C1), 
                  correlation=corAR1(form=~week), method="REML")
        pred <- dataprev %>% filter(yearepi==year[i]&week>9)
        pred$t <- c(270:(269+43))/(5*52+9)
        fore <- predict(mf, pred, level=1)
        PE <- pred$outcome - fore
      }
      if(model=="lme2")
      {
        # model 9 with T=52
        mf <- lme(outcome~t+S11+C11, data=dpif, random=pdDiag(~S11+C11), 
                  correlation=corAR1(form=~week), method="REML")
        pred <- dataprev %>% filter(yearepi==year[i]&week>9)
        pred$t <- c(270:(269+43))/(5*52+9)
        fore <- predict(mf, pred, level=1)
        PE <- pred$outcome - fore
      }
    }

    tot <- sum(pred$outcome)
    e2_t <- sqrt(sum(PE^2))/tot  # verbeeck
    res[i,] <- c(ME=mean(PE),
                 MAE=mean(abs(PE)),
                 RMSE=sqrt(mean(PE^2)),
                 RMSE2=100*e2_t, 
                 MAPE=100*mean(abs(PE)/pred$outcome),
                 MPE=100*mean(PE/pred$outcome))
    
  }
  return(res)
}

################################################################################
# 
# Predictions, standard errors, excess deaths, prediction intervals
# 2020 baseline forecasts
utilityF <- function(dataf, omega, bestfit)
{
  preddata <- dataf %>% filter((yearepi==2020)|(yearepi==2021)) %>% 
  mutate(S1=sin(omega*week), C1=cos(omega*week), S2=sin(2*omega*week), C2=cos(2*omega*week),
         S3=sin(3*omega*week), C3=cos(3*omega*week), P1=(week-26.5)/25.5, 
         P2=((week-26.5)/25.5)^2, P3=((week-26.5)/25.5)^3,
         S11=sin(2*pi*week/52), C11=cos(2*pi*week/52),)
  # LMM
  pred20 <- predict(bestfit, preddata[1:53,], level=1)
  pred21 <- predict(bestfit, preddata[-c(1:53),], level=0)
  pred1 <- c(pred20, pred21)
 
# Variance for the Fixed part LMM 202
  X <- model.matrix(bestfit, data=preddata[1:53,])
  VCondF <- X %*% vcov(bestfit) %*% t(X)

# Variance for 2020 predictions 
  R <- getVarCov(bestfit, type="conditional", individuals = 5)$`2019`
  R53r <-  c(0,R[52,-52])
  R53c <- c(R53r,R[52,52])
  R53 <- rbind(R,R53r)
  R53 <- cbind(R53,R53c)
  VPred2020 <- VCondF + R53

 # Before week 10 only the fixed part counts
  SE <- sqrt(diag(VCondF))
  SEP2020 <- sqrt(diag(VPred2020))
  SEP2020[1:9] <- SE[1:9]

# Variance for 2021
  Z <- model.matrix(formula(bestfit$modelStruct$reStr)[[1]],data=bestfit$data)[1:52,]
  D <- getVarCov(bestfit,type="random.effects")
  VRandom <- Z%*%D%*%t(Z)

# 2021
  VPred2021 <- VCondF[1:52,1:52] + R53[1:52,1:52] + VRandom
  SEP2021 <- sqrt(diag(VPred2021))
  # SE's for all
  SEF <- c(SEP2020,SEP2021)
  
  dgraph <- dataf %>% filter((yearepi>=2015&yearepi<=2021))
  preddata <- preddata %>% dplyr::select(outcome, yearepi, week, covid) 
  preddata$predM <- pred1[1:nrow(preddata)]
  preddata$se <- SEF[1:nrow(preddata)]
  preddata <- preddata %>% 
                 mutate(deathP=predM+covid, excessD=outcome-predM)
  
# Totals 
#  
  x <- matrix(1,ncol=44,nr=1)
  SESum20 <- sqrt(x%*%VPred2020[-c(1:9),-c(1:9)]%*%t(x))
  x <- matrix(1,ncol=52,nr=1)
  SESum21 <- sqrt(x%*%VPred2021%*%t(x))
  SEsum <- c(SESum20,SESum21)
  totals <- preddata[-c(1:9),] %>% group_by(yearepi) %>% 
              summarise(out= sum(outcome), covid = sum(covid), 
                        predM = sum(predM), predP = sum(deathP), 
                        excessD = sum(excessD))
  totals <- totals %>% mutate(PScore=100*excessD/predM, 
                                ratioEC=100*excessD/covid, SEsum=SEsum)
  totals <- totals %>% mutate(IPa=excessD-1.96*SEsum, IPb=excessD+1.96*SEsum, 
                                category=preddata$category[1:2])
  list(pred1=preddata, d=dgraph, totals=totals)
}




