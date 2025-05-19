## ECOSPHERE ##
#1.Load the packages.  ----
library("readxl")
library("RMark")
#2. Function to get the data of first capture. ----
get.first <- function(x) min(which(x != 0))
# first <- apply(my_data, 1, get.first)

#3.Simulate CMRH. ----
## States 

##-Define states and events ##---
# 7 States (z):
# 1 Chick - Non Breeder Entrance
# 2 Chick - Non Breeder Camera
# 3 First time breeder Entrance 
# 4 First time breeder Camera
# 5 Experienced Entrance
# 6 Experienced Camera
# 7 Dead

# # 9 States (z):

# 1 Chick - Non Breeder Entrance
# 2 Chick - Non Breeder Camera
# 3 rCe - recruited from entrance
# 4 rCc- recruited from camera
# 5 First time breeder Entrance
# 6 First time breeder Camera
# 7 Experienced Entrance
# 8 Experienced Camera
# 9 Dead

#Events (O)

#0 Not seen.
#1 Captured as nestling in the entrance.*
#2 Captured as nestling in the inner chambers.*
#3 Captured as breeder in the entrance.
#4 Captured as breeder in the inner chambers.


# We simulate CMR data by releasing 
# Number of chicks 50 E, 50 IC
# Number of FTB 15 E, 50 IC 
# Number of EB 85 E, 150 IC

###Released individuals ----
n_occasions<-9
#Newind stores the releases by state in each time step.
newind<-matrix(0, ncol = n_occasions, nrow = 6)
newind[1,1:9]<-N_c_e   <-50 #1
newind[2,1:9]<-N_c_ic  <-50 #2
newind[3,1]<-N_ftb_e <-15 #3
newind[4,1]<-N_ftb_ic<-50#4
newind[5,1]<-N_eb_e  <-85 #5
newind[6,1]<-N_eb_ic <-150 #6
newind[3,2:9]<-10
newind[4,2:9]<-20
  
#We fill the CMR history. 
#First ocassion and state. 
#Empty CH
CH<-rep(0,9)
tack0<-CH
    i<-1
    j<-1
    N<-newind[i,j]
    CH2<-CH
    CH2[j]<-i
    tack<-CH2
    for(r in 1:(N)){
      tack0<-rbind(tack0,CH2)
    }
    #Remove first line. 
    tack0<-tack0[-1,]
    #Rename rows to check it is working
    rownames(tack0)<-paste0("ind",1:N)
    #Clean workspace
    rm(list = c('tack','N',"i","j","CH2","r"))
    #Iterate rest of years and states
for(i in 2:dim(newind)[1]){#estado
  for (j in 1){#año
    N<-newind[i,j]
    if(N!=0){ 
      CH2<-CH
      CH2[j]<-i
      tack<-rbind(CH2,CH2)
      for(r in 1:(N-1)){
        tack<-rbind(tack,CH2)
      }
      tack<-tack[-1,]
      rownames(tack)<-paste0("ind",1:N)
      dim(tack)
      tack0<-rbind(tack0,tack)
      dim(tack0)
      rm(tack)
    }else{
      #Just to check it is working activate
      #print("no")
    }}}
#     rm(list = c('N',"i","j","CH2","r"))
#     View(tack0)
# for(i in 1:dim(newind)[1]){#estado
#     for (j in 2:dim(newind)[2]){#año
#       N<-newind[i,j]
#       if(N!=0){ 
#       CH2<-CH
#       CH2[j]<-i
#       tack<-rbind(CH2,CH2)
#       for(r in 1:(N-1)){
#         tack<-rbind(tack,CH2)
#       }
#       tack<-tack[-1,]
#       rownames(tack)<-paste0("ind",1:N)
#       dim(tack)
#       tack0<-rbind(tack0,tack)
#       dim(tack0)
#       rm(tack)
#       }else{
#         #Just to check it is working activate
#         #print("no")
#         }
#   }
# }
  View(tack0)
  table(tack0)
  CH<-tack0
  rm(tack0)
# Total number of individuals released
  totrel<-dim(CH)[1]
#4.Load parameters --------------------
##Fidelity----
  f1<-0.62
  f2<-1
  f3<-0.99
  f4<-1
##Recruitment----
  r1<-0.01
  r2<-0.02
  r3<-0.03
  r4<-0.24
  r5<-0.3
  r6<-0.8
  r7<-1
##Survival----
  phiC<-0.75
  phiE<-0.75
  phiFTBe<-0.5
  phiFTBc<-0.5
  phiEBe<-0.86
  phiEBc<-0.79
##Recapture----
  p<-0.47
#5.Estimate age of individuals at each time step. ----
age<-CH
#Get first ocasion
first <- apply(age, 1, get.first)
get.age<-function(CH, first){for (i in 1:dim(CH)[1]){
  position<- first[i]
  state<-age[i,position]
  if(state%in%c(1,2)){
    age[i,position]<-1
    tack<-position+1
    if(tack<(n_occasions+1)){
      for(t in tack:dim(CH)[2]){
        prev<-t-1
        age[i,t]<-age[i,prev]+1
      }  }
    
  }else{
    age[i,position]<-7
    tack<-position+1
    if(tack<(n_occasions+1)){
      for(t in tack:dim(CH)[2]){
        prev<-t-1
        age[i,t]<-age[i,prev]+1
      } }
  }
}
  return(age)
  }

age<-get.age(CH, first)

#6.Create recruitment matrix based on calculated age.----
# First time breeders are given age 7. This does not affect survival . 
r<-age
#create an object that stores the values of recruitment by age
r[which(r==1) ]<- r1
r[which(r==2) ]<- r2
r[which(r==3) ]<- r3
r[which(r==4) ]<- r4
r[which(r==5) ]<- r5
r[which(r==6) ]<- r6
r[which(r==7) ]<- r7
r[which(r>7) ]<- r7

#7. Define State and Event transition probability matrices----

##Define dimensions----
n.states<-7
n.states2<-9
n.obs<-5
n.occasions<-9
totrel<-dim(CH)[1]

#7.1. State process matrix PSI.STATE----
#This matrix is the product of 3 matrices psi1 (survival),psi2 (recruitment),psi3(fidelity)
PSI.STATE <- array(NA, dim = c(n.states, n.states, totrel, n.occasions - 1))
psi1<- array(NA,dim=c(n.states,n.states))
psi2<- array(NA,dim=c(n.states,n.states2,totrel,n.occasions-1))
psi3<- array(NA,dim=c(n.states2,n.states))

##Define survival #----

    # Survival matrix. Probabilities of state z(t+1) given z(t)
        psi1[1,1] <- phiE  #phiCe survival probability state Chick entrance
        psi1[1,2] <- 0
        psi1[1,3] <- 0
        psi1[1,4] <- 0
        psi1[1,5] <- 0
        psi1[1,6] <- 0
        psi1[1,7] <- 1 - phiE
            
        psi1[2,1] <- 0
        psi1[2,2] <- phiC  #phiCe survival probability state Chick camera
        psi1[2,3] <- 0
        psi1[2,4] <- 0
        psi1[2,5] <- 0
        psi1[2,6] <- 0
        psi1[2,7] <- 1 - phiC
            
        psi1[3,1] <- 0
        psi1[3,2] <- 0
        psi1[3,3] <- phiFTBe 
        psi1[3,4] <- 0
        psi1[3,5] <- 0
        psi1[3,6] <- 0
        psi1[3,7] <- 1 - phiFTBe
            
        psi1[4,1] <- 0
        psi1[4,2] <- 0
        psi1[4,3] <- 0
        psi1[4,4] <- phiFTBc 
        psi1[4,5] <- 0
        psi1[4,6] <- 0
        psi1[4,7] <- 1 - phiFTBc
            
        psi1[5,1] <- 0
        psi1[5,2] <- 0
        psi1[5,3] <- 0
        psi1[5,4] <- 0 
        psi1[5,5] <- phiEBe   
        psi1[5,6] <- 0
        psi1[5,7] <- 1 - phiEBe
            
        psi1[6,1] <- 0
        psi1[6,2] <- 0
        psi1[6,3] <- 0
        psi1[6,4] <- 0 
        psi1[6,5] <- 0   
        psi1[6,6] <- phiEBc 
        psi1[6,7] <- 1 - phiEBc 
            
        psi1[7,1] <- 0
        psi1[7,2] <- 0
        psi1[7,3] <- 0
        psi1[7,4] <- 0 
        psi1[7,5] <- 0   
        psi1[7,6] <- 0
        psi1[7,7] <- 1  
##Define recruitment ----
    for(i in 1:totrel){
      for(t in 1:(n.occasions-1)){
        #Recruitment 
        psi2[1,1,i,t] <- 1 - r[i,t]
        psi2[1,2,i,t] <- 0
        psi2[1,3,i,t] <- r[i,t]
        psi2[1,4,i,t] <- 0
        psi2[1,5,i,t] <- 0
        psi2[1,6,i,t] <- 0
        psi2[1,7,i,t] <- 0
        psi2[1,8,i,t] <- 0
        psi2[1,9,i,t] <- 0
        
        psi2[2,1,i,t] <- 0
        psi2[2,2,i,t] <- 1 - r[i,t]
        psi2[2,3,i,t] <- 0
        psi2[2,4,i,t] <- r[i,t]
        psi2[2,5,i,t] <- 0
        psi2[2,6,i,t] <- 0
        psi2[2,7,i,t] <- 0
        psi2[2,8,i,t] <- 0
        psi2[2,9,i,t] <- 0
        
        psi2[3,1,i,t] <- 0
        psi2[3,2,i,t] <- 0
        psi2[3,3,i,t] <- 0
        psi2[3,4,i,t] <- 0
        psi2[3,5,i,t] <- 1
        psi2[3,6,i,t] <- 0
        psi2[3,7,i,t] <- 0
        psi2[3,8,i,t] <- 0
        psi2[3,9,i,t] <- 0
        
        psi2[4,1,i,t] <- 0
        psi2[4,2,i,t] <- 0
        psi2[4,3,i,t] <- 0
        psi2[4,4,i,t] <- 0
        psi2[4,5,i,t] <- 0
        psi2[4,6,i,t] <- 1
        psi2[4,7,i,t] <- 0
        psi2[4,8,i,t] <- 0
        psi2[4,9,i,t] <- 0
        
        psi2[5,1,i,t] <- 0
        psi2[5,2,i,t] <- 0
        psi2[5,3,i,t] <- 0
        psi2[5,4,i,t] <- 0 
        psi2[5,5,i,t] <- 0
        psi2[5,6,i,t] <- 0
        psi2[5,7,i,t] <- 1
        psi2[5,8,i,t] <- 0
        psi2[5,9,i,t] <- 0
        
        psi2[6,1,i,t] <- 0
        psi2[6,2,i,t] <- 0
        psi2[6,3,i,t] <- 0
        psi2[6,4,i,t] <- 0 
        psi2[6,5,i,t] <- 0  
        psi2[6,6,i,t] <- 0 
        psi2[6,7,i,t] <- 0 
        psi2[6,8,i,t] <- 1 
        psi2[6,9,i,t] <- 0 
        
        psi2[7,1,i,t] <- 0
        psi2[7,2,i,t] <- 0
        psi2[7,3,i,t] <- 0
        psi2[7,4,i,t] <- 0 
        psi2[7,5,i,t] <- 0   
        psi2[7,6,i,t] <- 0
        psi2[7,7,i,t] <- 0  
        psi2[7,8,i,t] <- 0   
        psi2[7,9,i,t] <- 1
      }}
##Define fidelity ----
        psi3[1,1] <- 1 
        psi3[1,2] <- 0
        psi3[1,3] <- 0 
        psi3[1,4] <- 0
        psi3[1,5] <- 0
        psi3[1,6] <- 0
        psi3[1,7] <- 0
        
        psi3[2,1] <- 0
        psi3[2,2] <- 1
        psi3[2,3] <- 0
        psi3[2,4] <- 0
        psi3[2,5] <- 0
        psi3[2,6] <- 0
        psi3[2,7] <- 0
        
        psi3[3,1] <- 0
        psi3[3,2] <- 0
        psi3[3,3] <- f1
        psi3[3,4] <- 1-f1
        psi3[3,5] <- 0
        psi3[3,6] <- 0
        psi3[3,7] <- 0
        
        psi3[4,1] <- 0
        psi3[4,2] <- 0
        psi3[4,3] <- 1-f2
        psi3[4,4] <- f2
        psi3[4,5] <- 0
        psi3[4,6] <- 0
        psi3[4,7] <- 0
        
        psi3[5,1] <- 0
        psi3[5,2] <- 0
        psi3[5,3] <- 0
        psi3[5,4] <- 0 
        psi3[5,5] <- f3
        psi3[5,6] <- 1 - f3
        psi3[5,7] <- 0
        
        psi3[6,1] <- 0
        psi3[6,2] <- 0
        psi3[6,3] <- 0
        psi3[6,4] <- 0 
        psi3[6,5] <- 1 - f3   
        psi3[6,6] <- f3 
        psi3[6,7] <- 0 
        
        psi3[7,1] <- 0
        psi3[7,2] <- 0
        psi3[7,3] <- 0
        psi3[7,4] <- 0 
        psi3[7,5] <- f4
        psi3[7,6] <- 1 - f4
        psi3[7,7] <- 0
        
        psi3[8,1] <- 0
        psi3[8,2] <- 0
        psi3[8,3] <- 0
        psi3[8,4] <- 0 
        psi3[8,5] <- 1 - f4   
        psi3[8,6] <- f4 
        psi3[8,7] <- 0 
        
        psi3[9,1] <- 0
        psi3[9,2] <- 0
        psi3[9,3] <- 0
        psi3[9,4] <- 0 
        psi3[9,5] <- 0   
        psi3[9,6] <- 0
        psi3[9,7] <- 1  
        
    
##Calculate PSI.STATE for each individual and each time step #----
for(i in 1:totrel){
  for(t in 1:8){
    PSI.STATE[1:n.states,1:n.states,i,t]<-psi1[1:n.states,1:n.states]%*%psi2[1:n.states,1:n.states2,i,t]%*%psi3[1:n.states2,1:n.states]
  }}
 
#7.2. Observation process matrix ----
B <- array(NA, dim = c(n.states, n.obs, totrel, n.occasions))
for (i in 1:totrel) {
  for (t in 1:(n.occasions )) {
    B[1,1,i,t] <- 1 -p
    B[1,2,i,t] <- p
    B[1,3,i,t] <- 0 
    B[1,4,i,t] <- 0
    B[1,5,i,t] <- 0
    
    B[2,1,i,t] <- 1 -p
    B[2,2,i,t] <- 0
    B[2,3,i,t] <- p
    B[2,4,i,t] <- 0
    B[2,5,i,t] <- 0
    
    B[3,1,i,t] <- 1 - p
    B[3,2,i,t] <- 0
    B[3,3,i,t] <- 0
    B[3,4,i,t] <-  p
    B[3,5,i,t] <- 0
    
    B[4,1,i,t] <- 1 - p
    B[4,2,i,t] <- 0
    B[4,3,i,t] <- 0
    B[4,4,i,t] <- 0
    B[4,5,i,t] <- p
    
    B[5,1,i,t] <- 1 - p
    B[5,2,i,t] <- 0
    B[5,3,i,t] <- 0
    B[5,4,i,t] <- p
    B[5,5,i,t] <- 0
    
    B[6,1,i,t] <- 1 - p
    B[6,2,i,t] <- 0
    B[6,3,i,t] <- 0
    B[6,4,i,t] <- 0 
    B[6,5,i,t] <- p
    
    B[7,1,i,t] <- 1
    B[7,2,i,t] <- 0
    B[7,3,i,t] <- 0
    B[7,4,i,t] <- 0 
    B[7,5,i,t] <- 0 
    
  }
}
#Check it is working 
first2<-first
first2[which(first2==9)]<-NA
##Update observation matrix on first capture----
for( i in 1:dim(B)[3]){
     t<-first[i]
#First time you see a bird 
     B[1,1,i,t]<-0
     B[1,2,i,t]<-1
     
     B[2,1,i,t]<-0
     B[2,3,i,t]<-1
     
     B[3,1,i,t]<-0
     B[3,4,i,t]<-1
     
     B[4,1,i,t]<-0
     B[4,5,i,t]<-1
     
     B[5,1,i,t]<-0
     B[5,4,i,t]<-1
     
     B[6,1,i,t]<-0
     B[6,5,i,t]<-1
}

PSI.OBS<-B

#To check dimensions work
 #PSI.STATE[,,1,1]%*%PSI.OBS[,,1,1]
 rm(CH2)

 #8. Simulate latent states ----
 ##Generate individual initial states----
 CH.init<-CH
 ## Define function to simulate latent states ----
simul.states <- function(PSI.STATE,CH.init,first,unobservable=NA) {
  # Unobservable: number of state that is unobservable
  n.occasions <- dim(PSI.STATE)[4] + 1
  CH.latent <- CH.init
for (i in 1:dim(CH.latent)[1]){
  if (first[i] == n.occasions) next
  for (t in (first[i] + 1):n.occasions) {
    PSI.STATE[CH.latent[i, t - 1], , i, t - 1]
    # Multinomial trials for state transitions
    state <- which(rmultinom(1, 1, PSI.STATE[CH.latent[i, t - 1], , i, t - 1]) == 1)
    CH.latent[i, t] <- state
  }
}
   # Replace NA and highest state number (dead) in CH with 0
  return(CH.latent = CH.latent)
}
##Generate latent state CRH----
CH.latent<-simul.states(PSI.STATE,CH.init,first,unobservable=NA)
#9. Simulate latent states ----
#Define function to simulate observed events ----
 simul.events <- function(PSI.OBS, CH.latent,first) {
  n.occasions <- dim(PSI.OBS)[4] 
  CH.event <- CH.latent
  for (i in 1:dim(CH)[1]){
   for (t in first[i] :n.occasions) {
         # Multinomial trials for observation process
      event <- which(rmultinom(1, 1, PSI.OBS[CH.event[i, t], , i, t]) == 1)
      #The events go from 1 to 5 but they correspond with 0 to 4 in the observation matrix so we substract 1 from them. 
      CH.event[i, t] <- event-1
    }
  }
  return(CH.event = CH.event)
}
##Generate observed event CRH----
 CH.event<-simul.events(PSI.OBS, CH.latent,first)
#10.Export data as inp. 
CH<-as.data.frame(collapseCH(CH.event))
CH$group<-"1;"
names(CH)<-c("CH","group")
write.table(CH, "Simulated_7states_5events_Hp.inp", append = FALSE, sep = " ", 
            row.names = FALSE, col.names = FALSE,quote=FALSE)
#Tadah!