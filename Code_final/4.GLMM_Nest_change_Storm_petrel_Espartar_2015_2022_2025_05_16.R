####Storm Petrels  ####
#Author: Ana Payo Payo
#Date: SEPT_2024

# GLM
# Zone
# E-Entrance
# C- Chambers
###House keeping 
rm(list=ls())
### load libraries 
library(glmmTMB) #for glmm


#Load data 
  
  data<-readRDS(file = "4.Nest_change_storm_petrel_Espartar_2014_2022_.rds")
  

  #Run glmm models 
  m0 <- glmmTMB(nest_fidelity ~ 1    + (1 | BirdID), family = binomial, data = data)
  m1 <- glmmTMB(nest_fidelity ~ zone + (1 | BirdID), family = binomial, data = data)
  
  anova(m0,m1,test="chisQ")
  
  #get beta values for zone 
  m1$sdr 
  #get ci values for zone
  0.8852893    -1.96*0.2989012 #low
  0.8852893    +1.96*0.2989012 #high
  
  
  # Estimate predicted values 
  t<-ggpredict(m1, c("zone"))
  t
  #save predicted values in object to plot figure. 
  Zone<-c("Inner chambers","Entrance")
  Nest_fid_pred<-as.data.frame(cbind(Zone,t$predicted,t$conf.low,t$conf.high))
  names(Nest_fid_pred)<-c("Zone","Mean","Low","High")
  Nest_fid_pred$Zone<-as.factor(Nest_fid_pred$Zone)
  Nest_fid_pred$Mean<-as.numeric(Nest_fid_pred$Mean)
  Nest_fid_pred$Low<-as.numeric(Nest_fid_pred$Low)
  Nest_fid_pred$High<-as.numeric(Nest_fid_pred$High)
  
  #Store BS_by_Zone
  saveRDS(Nest_fid_pred, file = "6.4.Nest_fid_by_zone_pred.rds")
  
  
