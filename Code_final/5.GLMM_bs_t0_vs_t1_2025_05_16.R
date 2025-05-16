####Storm Petrels  ####
#Author: Ana Payo Payo
#Date: SEPT_2024
###House keeping 
rm(list=ls())
### load libraries 
library(glmmTMB) #for glmm
library(ggeffects) #for effects
library(readxl)

#set working directory
#load data
BS_INDt<- readRDS(file = "5.Causes_nest_change.rds")
getwd()
#Run glmm models 
m0 <- glmmTMB( bs~ zone*change +(1|ID)+(1|year)+(1|event), family = binomial, data = BS_INDt)
m1 <- glmmTMB( bs~ zone+change +(1|ID)+(1|year)+(1|event), family = binomial, data = BS_INDt)
m2 <- glmmTMB( bs~      change +(1|ID)+(1|year)+(1|event), family = binomial, data = BS_INDt)
m3 <- glmmTMB( bs~ zone        +(1|ID)+(1|year)+(1|event), family = binomial, data = BS_INDt)
m4 <- glmmTMB( bs~            1+(1|ID)+(1|year)+(1|event), family = binomial, data = BS_INDt)

BS_model<-anova(m0,m1,m2,m3,m4,test="Chisq")
BS_model
summary(m1)
t<-ggpredict(m1, c("zone","change"))
t
-0.6293+0.2886  *1.96
-0.6293-0.2886  *1.96

#fracaso induce al cambio pero una vez que cambian no mejoran. 
t<-ggpredict(m1, c("change","zone"))
t


BS_INDt1<-BS_IND[which(BS_IND$t=="t+1"),]

#Run glmm models 
m0 <- glmmTMB( bs~ zone*change +(1|ID)+(1|year)+(1|event), family = binomial, data = BS_INDt1)
m1 <- glmmTMB( bs~ zone+change +(1|ID)+(1|year)+(1|event), family = binomial, data = BS_INDt1)
m2 <- glmmTMB( bs~      change +(1|ID)+(1|year)+(1|event), family = binomial, data = BS_INDt1)
m3 <- glmmTMB( bs~ zone        +(1|ID)+(1|year)+(1|event), family = binomial, data = BS_INDt1)
m4 <- glmmTMB( bs~            1+(1|ID)+(1|year)+(1|event), family = binomial, data = BS_INDt1)

BS_model<-anova(m0,m1,m2,m3,m4,test="Chisq")
BS_model
summary(m2)
t<-ggpredict(m2, c("zone"))
t
#fracaso induce al cambio pero una vez que cambian no mejoran. 
t<-ggpredict(m1, c("change","zone"))
t

ab<-as.data.frame(cbind(t$predicted,
                        t$conf.low,
                        t$conf.high,
                        t$x,
                        t$group,
                        t$facet
))
names(ab)<-c("mean","low","upp","Zone","Change","time")
ab$Zone<-c("Entrance","Entrance","Entrance","Entrance","Inner chambers" ,"Inner chambers","Inner chambers" ,"Inner chambers")
ab$Change<-c("Nest fidelity","Nest fidelity","Nest change","Nest change","Nest fidelity","Nest fidelity","Nest change","Nest change")
ab$time<-c("t" ,"t+1", "t", "t+1", "t" ,"t+1" ,"t" ,"t+1")
############################################################################
#############################################################################
#############################################################################


