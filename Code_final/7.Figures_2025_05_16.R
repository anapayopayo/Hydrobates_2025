####Load libraries #### surely we dont need all these 
library(ggplot2)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(imager)
library(ggtext)
library(DAAG)
library(extrafont)
library(cowplot)
library(showtext)
library(gridBase)
library(gridExtra)
library(patchwork)

####Load data ###
    #### Observed number of ticks ####
        N_ticks       <-readRDS(file = "6.1.Nticks_by_zone_pred.rds")
    #### Observed fate by zone ####
        Fate          <-readRDS(file = "6.2.Chick_fate_by_zone_pred.rds")
     #### Observed BS by zone ####
        BS            <-readRDS(file = "6.3.BS_by_zone_pred.rds")
     #### Observed Nest fidelity by zone ####
        Nest_fidelity <-readRDS(file = "6.4.Nest_fid_by_zone_pred.rds")
     ####ME estimates ####    
        vital_rates     <-readRDS(file = "6.5.Fig_vital_rates.rds")
        fidelity<-vital_rates[14:15,]
        survival<-vital_rates[12:13,]
        survival$Zone<-c("Entrance","Inner chambers")
        survival$Zone<-as.factor(survival$Zone)
        #### Population growth rate 
        lambda     <-readRDS(file =  "6.6.Fig_Observed_lambda.Rds")    

#### Observed number of ticks ####
pd<-pd0 <- position_dodge(0.3) # move them .05 to the left and right
library(ggplot2)
A<-ggplot(N_ticks, aes(x=Zone, y=Mean)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=Low , ymax=High), width=0, position=pd)  +
  scale_x_discrete(labels= c("Entrance"," Inner \n chambers"))+ 
  ylab(expression(N["ticks"])) +
  ylim(c(0,1.5))+
  xlab("Zone")+
  ggtitle("") +
  # geom_richtext(aes(x = 2.5,
  #                   y = 9, 
  #                   label = "**D**"),fill=NA,
  #               text.colour = "black",
  #               label.size = NA)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20),
        axis.title.y = element_text(angle = 90,hjust=0.5, vjust=0.5,size=20),
        plot.margin = margin(1,1,1,1, "cm"))
A
#### FATE ####
B<-ggplot(data=Fate, aes(x=Max_tick, y = pred,color=factor(Zone)))+
  geom_line(aes(color = factor(Zone)), size=1)+
  geom_ribbon(aes(ymin=llimit, ymax=ulimit, fill=factor(Zone)), alpha = 0.25,show.legend = FALSE)+
  theme(legend.position = "right")  +
  scale_colour_manual(name="",values = c("Entrance"="black", "Inner chambers"="grey")) +
  scale_fill_manual(name="",values = c("Entrance"="black", "Inner chambers"="grey")) +
  ylab(expression(Survival ["Nestling"])) +
  ylim(0,1)+
  xlab(expression(N[Nticks]))+
  ggtitle("") +
  annotate("text", x=59, y=1, label= "Alive") + 
  annotate("text", x=59, y=0, label= "Dead") + 
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20),
        axis.title.y = element_text(angle = 90,hjust=0.5, vjust=0.5,size=20),
        plot.margin = margin(1,1,1,1, "cm"),
legend.text=element_text(size=10),
        legend.position = "top")
B

###BS ####

#### plot means and se of predicted values. 
C<-ggplot(BS, aes(x=Zone, y=Mean, colour=Experience)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=Low , ymax=High), width=0, position=pd)  +
  scale_colour_manual(name="",values = c("EB"="black", "FTB"="grey")) +
  scale_fill_manual(name="",values = c("EB"="black", "FTB"="grey")) +  ylab("BS") +
  scale_x_discrete(labels= c("Entrance"," Inner \n chambers"))+ 
  
  ylim(0,1)+
  xlab("Zone")+
  ggtitle("") +
  # geom_richtext(aes(x = 2.5,
  #                   y = 7.5, 
  #                   label = "**D**"),fill=NA,
  #               text.colour = "black",
  #               label.size = NA)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20),
        legend.text=element_text(size=10),
        legend.position = "top",
        axis.title.y = element_text(angle = 90,hjust=0.5, vjust=0.5,size=20),
        plot.margin = margin(1,1,1,1, "cm"))
        C

###nest fidelity #####
D<-ggplot(Nest_fidelity, aes(x=Zone, y=Mean)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=Low , ymax=High), width=0, position=pd)  +
  scale_x_discrete(labels= c("Entrance"," Inner \n chambers"))+ 
    ylab(expression(Nest ["fidelity"])) +
  ylim(0.7,1)+
  xlab("Zone")+
  ggtitle("") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20),
        axis.title.y = element_text(angle = 90,hjust=0.5, vjust=0.5,size=20),
        plot.margin = margin(1,1,1,1, "cm"))
        D

####BS ind ####

####  Vital rates ####
#####Natal_f Fidelity ####

E<-ggplot(fidelity, aes(x=rownames(fidelity),  y=Mean)) + #shape=sex,
  geom_errorbar(aes(ymin=lowCI , ymax=upCI), width=0, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_discrete(labels= c("Entrance"," Inner \n chambers"))+ 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01,
                                   decimal.mark = '.'))+
  xlab("Zone") +
  ylab(expression("Natal zone" ["fidelity"])) +
  ylim(0,1)+
  ggtitle("") +
  annotate("text", x = 1.35, y = 0.62,color="black"   , label = expression(paste(N==10)),size=3)+ 
  annotate("text", x = 2.35, y = 1, color="black", label = expression(paste(N== 21)),size=3)+ 
  theme_bw() +
  theme(strip.placement = "outside",
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20),
        strip.text = element_text(color = "black",  size = 10),
        strip.background = element_rect(fill = "white", color="white", linetype = "solid"),
        axis.title.y = element_text(angle = 90,hjust=0.5, vjust=0.5,size=15),
        legend.text=element_text(size=12),
        legend.title=element_text(size=8),
        legend.position=c(0.25, 0.15),
        plot.margin = margin(1,1,1,1, "cm"))
        
E
##### Survival ####
F1<-ggplot(survival, aes(x=Zone,  y=Mean)) + #shape=sex,
  geom_errorbar(aes(ymin=lowCI , ymax=upCI), width=0, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_discrete(labels= c("Entrance","Inner \n chambers"))+
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01,
                                   decimal.mark = '.'))+
  ylim(0.7,0.95)+xlab("Zone") +
  ylab("Adult survival ") +
  ggtitle("") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20),
        axis.title.y = element_text(angle = 90,hjust=0.5, vjust=0.5,size=15),
        plot.margin = margin(1,1,1,1, "cm"),
        legend.position = "none")
F1


library(ggplot2)
library(patchwork)




# List the subplots
plot_list <- list(A,B,C,D,E,F1)

# Define a function to add a tag to a plot
add_tag <- function(label, x = 1, y = 1, padding.x = unit(5, "pt"), padding.y = padding.x, hjust = 1, vjust = 1, size = 13) {
  annotation_custom(
    grid::textGrob(
      x = unit(x, "npc") - padding.x,
      y = unit(y, "npc") - padding.y,
      hjust = hjust, vjust = vjust,
      label = label, gp = grid::gpar(fontsize = size)
    )
  )
}

# Add a tag to each plot
plot_list <- lapply(
  seq_along(plot_list), 
  \(x) plot_list[[x]] + add_tag(LETTERS[x], x = 1, y = .95)
)

# Combine the plots into a grid
wrap_plots(plot_list, ncol = 3)




  dev.off()

png("Fig1.png", width=25, height=22, units="cm", res=600)
wrap_plots(plot_list, ncol = 3)
dev.off() #only 129kb in size


#population growth rate #### 
G<-ggplot(lambda, aes(x=year, y=mean_N  )) +
  #geom_line(colour = "#00AFBB")+
  geom_line( aes(y=lambdaT,        color="#00AFBB",lwd=2))+
  geom_line( aes(y=lambda_Cueva,   color="black"  ,lwd=2))+
  geom_line( aes(y=lambda_Entrada, color="#E7B800",lwd=2))+
  geom_point(aes(y=lambdaT,        color="#00AFBB"))+      # Colony lambda
  geom_point(aes(y=lambda_Cueva,   color="black"  ))+      # Cave lambda
  geom_point(aes(y=lambda_Entrada, color="#E7B800"))+      # Entrance lambda
  labs(x = "Year")  + 
  labs(y = expression(lambda))  +
  ylim(0.6,1.3)+
  scale_x_continuous(breaks=c(2015:2021)) +
  scale_color_manual(labels=c('Colony',  "Entrance",  'Inner Chambers', ""),
                     values=c("#00AFBB",  "#E7B800", "black","grey"))+
  labs(color=NULL) +
  annotate("text", x = 2020.9, y = 1.02,color="grey"   , label = expression(paste(lambda==1)),size=5)+ 
  annotate("text", x = 2020.9, y = 0.91, color="#00AFBB", label = expression(paste(lambda== 0.91)),size=5)+ 
  annotate("text", x = 2015.5, y = 0.69, color="#00AFBB", label = expression(paste(lambda)["Colony"]),size = 10)+ 
  annotate("text", x = 2015.6, y = 0.65, color="#E7B800", label = expression(paste(lambda)["Entrance"]),size = 10)+ 
  annotate("text", x = 2016  , y = 0.61, color="black"  , label = expression(paste(lambda)["Inner chambers"]),size = 10)+ 
  
  theme(axis.text = element_text(size = 15),
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.key = element_rect(colour = NA, fill = NA),
        text = element_text(size = 20),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"), 
        axis.title.y = element_text(angle=0, vjust=0.5,size=15)) +
  
  theme(legend.position = "none") +
  geom_hline(aes(yintercept=0.89, 
                 color = "#00AFBB"),
             linetype="dashed")+
  geom_hline(aes(yintercept=0.85, 
                 color = "#00AFBB",alpha = 0.4,),
             linetype="dashed")+
  geom_hline(aes(yintercept=0.93, 
                color = "#00AFBB"),
                linetype="dashed",alpha = 0.4,)+
  geom_hline(aes(yintercept=1, 
                 color = "grey"),
             linetype="dashed")+  guides(lwd = FALSE)  
G

 