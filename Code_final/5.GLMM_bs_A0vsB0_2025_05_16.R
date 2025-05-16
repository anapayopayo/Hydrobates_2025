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


BS_A0_B0_FINAL<- readRDS(file = "5.Information_nest_change.rds")

m0 <- glmmTMB( bs~ zone*what       +(1|ID)+(1|Year)+(1|event), family = binomial, data = BS_A0_B0_FINAL)
m1 <- glmmTMB( bs~ zone+what       +(1|ID)+(1|Year)+(1|event), family = binomial, data = BS_A0_B0_FINAL)
m2 <- glmmTMB( bs~      what       +(1|ID)+(1|Year)+(1|event), family = binomial, data = BS_A0_B0_FINAL)
m3 <- glmmTMB( bs~ what            +(1|ID)+(1|Year)+(1|event), family = binomial, data = BS_A0_B0_FINAL)
m4 <- glmmTMB( bs~         1 +(1|ID)+(1|Year)+(1|event), family = binomial, data = BS_A0_B0_FINAL)

BS_model<-anova(m0,m1,m2,m3,m4,test="Chisq")
BS_model
summary(m3)
summary(m1)
mod <- glmmTMB( bs~ zone*what +(1|ID)+(1|Year)+(1|event), family = binomial, data = BS_A0_B0_FINAL, na.action='na.fail')

# Model-averaging
maveg.mod <- MuMIn::dredge(mod, rank = "AICc")
#> Fixed terms are "cond((Int))" and "disp((Int))"
maveg.mod2<-subset(maveg.mod, delta <= 2)
maveg.mod.est<- MuMIn::model.avg(maveg.mod2, revised.var = TRUE, fit = TRUE)
confset.95p <- MuMIn::get.models(maveg.mod2, subset = cumsum(weight) <= 1)

#Fixed terms are "cond((Int))" and "disp((Int))"
avgm <- MuMIn::model.avg(confset.95p)
Coef <- coef(avgm, full=TRUE)
CI <- confint(avgm, full=TRUE)

test <- data.frame(avgm$x, Species=NA)[,-1]
p <- predict(avgm, test, type='link', se.fit=TRUE, re.form=~0, backtransform=FALSE)

#generate raster data
r <- terra::rast(nrow=10, ncol=15, nlyr=8, names=names(test), vals=test)
pr <- terra::predict(r, avgm, type='link', se.fit=TRUE, re.form=~0, backtransform=FALSE)
all(do.call(cbind, p) == values(pr))
# TRUE