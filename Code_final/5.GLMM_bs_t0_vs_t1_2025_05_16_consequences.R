####Storm Petrels  ####
#Author: Ana Payo Payo
#Date: SEPT_2024
###House keeping 
rm(list=ls())
### load libraries 
library(glmmTMB) #for glmm

BS_IND<- readRDS(file = "7.Consequences_nest_change.rds")

#Run glmm models 
m0 <- glmmTMB( bs~ zone*change*t +(1|ID), family = binomial, data = BS_IND)
m1 <- glmmTMB( bs~      change*t +(1|ID), family = binomial, data = BS_IND)
m2 <- glmmTMB( bs~ zone       *t +(1|ID), family = binomial, data = BS_IND)
m3 <- glmmTMB( bs~             t +(1|ID), family = binomial, data = BS_IND)

BS_model<-anova(m0,m1,m2,m3,test="Chisq")
BS_model
summary(m2)
t<-ggpredict(m2, c("zone","t"))
t
ab<-as.data.frame(cbind(t$predicted,
                        t$conf.low,
                        t$conf.high,
                        t$x,
                        t$group))
names(ab)<-c("mean","low","upp","Zone","time")
ab$Zone<-c("Entrance","Entrance","Inner chambers" ,"Inner chambers")
ab$time<-c("t" ,"t+1", "t", "t+1" )

BS_ind<-ab
library(ggplot2)
pd0<-position_dodge(width = 0.3)

ggplot(BS_ind,aes(x=time,y=mean), position=pd0 )+
  geom_point(aes(),size=3, position=pd0 )+ 
  ylim(0,1)+
  xlab("Time")+
  ylab(bquote(BS["Bird"]))+
  geom_errorbar(aes(ymin=low,ymax=upp,width=0), position=pd0 )+
  facet_wrap(~Zone)+
  facet_grid(cols = vars(Zone), switch = "both")+
  theme_bw() +
  theme(strip.placement = "outside",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15),
        strip.text = element_text(color = "black",  size = 10),
        strip.background = element_rect(fill = "white", color="white", linetype = "solid"),
        axis.title.y = element_text(angle = 90,hjust=0.5, vjust=0.5,size=15),
        legend.text=element_text(size=12),
        legend.title=element_text(size=8),
        legend.position=c(0.25, 0.15),
        plot.margin = margin(1,1,1.5,1.2, "cm"))


#Causa 
# para ver si los que fracasan cambian más. 
#Si el fracaso proboca un cambio de nido 
#Despues las consecuencias del cambio de nido. 

BS_INDt<-BS_IND[which(BS_IND$t=="t"),]

table(BS_IND$t      )

t<-ggpredict(m0, c("zone","change","t"))
t
dim(BS_INDt)


saveRDS(BS_INDt, file = "5.Causes_nest_change.rds")


BS_INDt<- readRDS(file = "5.Causes_nest_change.rds")

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


