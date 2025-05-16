###Author : Ana Payo Payo
#Date: 04/09/2024
###Audited by : Andreu Rotger 
### Date audit: 

# Evaluate the differences in the number of ticks between 
# the Entrance and the inner cave. 

###House keeping 
      rm(list=ls())
### load libraries 
      library(glmmTMB) #for glmm
      library(ggeffects) #for effects
## set working directory 

### Read dataset ####
      Nticks<- readRDS(file = "1.Chicks_ticks_Espartar_2018_2020_.rds")
#Run glmm models 
      m0 <- glmmTMB(N_ticks ~1           +(1|Year)+(1|log_Age)+(1|BirdID), family = poisson, data = Nticks)
      m1 <- glmmTMB(N_ticks ~Zone        +(1|Year)+(1|log_Age)+(1|BirdID), family = poisson, data = Nticks)
#get beta values for zone 
      m1$sdr # is beta2 
#get ci values 
      -1.1436308-1.96*0.09357464 #low
      -1.1436308+1.96*0.09357464 #high
      #Compare models
      anova(m0,m1,test="Chisq")
# Estimate predicted values 
      t<-ggpredict(m1, c("Zone"))
#save predicted values in object to plot figure. 
      Zone<-c("Inner chambers","Entrance")
      Nticks_pred<-as.data.frame(cbind(Zone,t$predicted,t$conf.low,t$conf.high))
      names(Nticks_pred)<-c("Zone","Mean","Low","High")
      Nticks_pred$Zone<-as.factor(Nticks_pred$Zone)
      Nticks_pred$Mean<-as.numeric(Nticks_pred$Mean)
      Nticks_pred$Low<-as.numeric(Nticks_pred$Low)
      Nticks_pred$High<-as.numeric(Nticks_pred$High)

#Store Ntick_by_Zone
      saveRDS(Nticks_pred, file = "6.1.Nticks_by_zone_pred.rds")


