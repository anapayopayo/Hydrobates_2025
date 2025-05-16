###Author
# Ana Payo Payo
#Date: 04/09/2024
###Audited by : 
### Date audit: 
#### Aim 
# Evaluate the differences chick fate by the maximun number of ticks by zone.
###House keeping 
    rm(list=ls())
### load libraries 
    library(glmmTMB) #for glmm
    library(ggeffects) #for effects
#Load data 
    Chick_fate<-readRDS(file = "2.Chick_fate_by_MaxNtick_Zone_.rds")
    
    #Chick fate 
    #Dead - 0 
    #Alive - 1

#### Model Ticks by zone ####
    m0 <- glmmTMB(Fate_10 ~ 1                       +(1 | Year), family = binomial(link="logit"), data = Chick_fate)
    m1 <- glmmTMB(Fate_10 ~           Zone          +(1 | Year), family = binomial(link="logit"), data = Chick_fate)
    m2 <- glmmTMB(Fate_10 ~ Max_tick                +(1 | Year), family = binomial(link="logit"), data = Chick_fate)
    m3 <- glmmTMB(Fate_10 ~ Max_tick +Zone          +(1 | Year), family = binomial(link="logit"), data = Chick_fate)


anova(m0,m1,m2,m3,test="chisQ")

#get beta values for zone 
m3$sdr 
#get ci values for zone
-1.74454354-1.96*0.8017758 #low
-1.74454354+1.96*0.8017758 #high
#get ci values for NMax tick
-0.07055943-1.96*0.0246017
-0.07055943+1.96*0.0246017
# Estimate predicted values 

fit = glmmTMB(Fate_10 ~ Max_tick +Zone,
              family = binomial(link="logit"),
              data = Chick_fate)


new.data<- data.frame(Zone = sample(c("Inner chambers","Entrance"),275,replace=TRUE),
                      Max_tick= runif(n=275, min=0, max=66))
new.data<- data.frame(Zone = Chick_fate$Zone,
                      Max_tick= Chick_fate$Max_tick)
head(new.data)
preds <- predict(fit, new.data, se=T, allow.new.levels = T, type='response')
new.data$pred=preds$fit
new.data$se = preds$se.fit

new.data$ulimit = new.data$pred+qnorm(0.975)*(new.data$se)
new.data$llimit = new.data$pred-qnorm(0.975)*(new.data$se)
new.data[which(new.data$ulimit>1), 5]<-1
new.data[which(new.data$llimit<0), 6]<-0
new.data$Fate_10<-Chick_fate$Fate_10
#Store Chick fate by Ntick_by_Zone
saveRDS(new.data, file = "6.2.Chick_fate_by_zone_pred.rds")




