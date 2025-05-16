
#Author: Ana Payo Payo
#Date: September 2024

# GLMM
# Zone
# E - Entrance
# C - Chambers

# Breeding success
# Dates:2014- 2022 
#
###House keeping 
rm(list=ls())
### load libraries 
library(glmmTMB) #for glmm
library(ggeffects) #for effects
#set working directory
#load data
BS<-readRDS(file = "3.1.BS_experience_2015_2022_.Rds")

  #Run glmm models 
  m0 <- glmmTMB(BS ~ 1                                    + (1 | BirdID)+(1 | Year), family = binomial, data = BS)
  m1 <- glmmTMB(BS ~ 1                  +experience_good  + (1 | BirdID)+(1 | Year), family = binomial, data = BS)
  m2 <- glmmTMB(BS ~ zone                                 + (1 | BirdID)+(1 | Year), family = binomial, data = BS)
  m3 <- glmmTMB(BS ~ zone               +experience_good  + (1 | BirdID)+(1 | Year), family = binomial, data = BS)

  anova(m0,m1, m2,m3,test="chisQ")
  
  #get beta values for zone 
  m3$sdr 
  #get ci values for zone
  -1.046  -1.96*0.1460010 #low
  -1.046  +1.96*0.1460010 #high
  0.4585662  -1.96*0.1321451
  0.4585662  +1.96*0.1321451
  # Estimate predicted values 
  t<-ggpredict(m3, c("zone","experience_good"))
  t
  #save predicted values in object to plot figure. 
  Zone<-c("Entrance","Entrance","Inner chambers","Inner chambers")
  BS_pred<-as.data.frame(cbind(Zone,t$predicted,t$conf.low,t$conf.high))
  names(BS_pred)<-c("Zone","Mean","Low","High")
  BS_pred$Zone<-as.factor(BS_pred$Zone)
  BS_pred$Mean<-as.numeric(BS_pred$Mean)
  BS_pred$Low<-as.numeric(BS_pred$Low)
  BS_pred$High<-as.numeric(BS_pred$High)
  BS_pred$Experience<-c("FTB","EB","FTB","EB")
  #Store BS_by_Zone
  saveRDS(BS_pred, file = "6.3.BS_by_zone_pred.rds")
  
  