##read the libraries that we will need

rm(list=ls())
library(readr)
library(tidyverse)
library(Epi)
library(splines)

##read the data already cleaned up for analysis (essentially we had cases by age and country merged in each wave, and we used to get the exposure that way, and used the average to
# obtain the mid point of waves that were not even (say, the average of 2011 and 2013 waves, which were 4 and 5, should be 2012) 
# route is the path that you will use: i.e. c/users/username/program_files, etc
SHARE <- read_table2("C:route/shareexposure.xls")
str(SHARE)
SHARE$Prevalence <- SHARE$Depressed/(SHARE$Depressed+SHARE$Non_Depressed)
SHARE$Exposure <- (SHARE$Depressed+SHARE$Non_Depressed)


##round to 0 and set as integer. 
SHARE$Depressed<-round(SHARE$Depressed,digits=0)
SHARE$Depressed<-as.integer(SHARE$Depressed)
SHARE$Exposure<-round(SHARE$Exposure,digits=0)
SHARE$Exposure<-as.integer(SHARE$Exposure)

# checking for missing data (APC analysis does not work if we have a value equal to 0)  
which(is.na(SHARE$Prevalence))
which(is.na(SHARE$Depressed))
which(is.na(SHARE$Exposure))

## Obtain proportion males/females
Maleexposure<- SHARE  %>%
  filter(Sex=="Male")   %>%
  group_by(Country,Period)  %>%
  summarize(Sumexposure=sum(Exposure))

print(Maleexposure)

Femaleexposure<- SHARE  %>%
  filter(Sex=="Female")   %>%
  group_by(Country,Period)  %>%
  summarize(Sumexposure=sum(Exposure))

print(Femaleexposure)

Exposures <- rbind(Maleexposure,Femaleexposure)
#write.table(Exposures,"shareexposure.xls",sep="/t")

Malepercentage<-Maleexposure$Sumexposure/(Femaleexposure$Sumexposure+Maleexposure$Sumexposure)
mean(Malepercentage)

#means 
mean(Malepercentage)
sd(Malepercentage)
mean(SHARE$Prevalence)
sd(SHARE$Prevalence)
sum(SHARE$Exposure)


###Group into Macro Regions For figure 1 

SHARE$Region [SHARE$Country=="Denmark" | SHARE$Country=="Sweden"| SHARE$Country=="Germany"] <- "North"
SHARE$Region [SHARE$Country=="Italy" | SHARE$Country=="Spain"| SHARE$Country=="France"] <- "South"


## Figure 1 : inspecting the prevalence trends at the beginning by region 
### 
Age2004 <- SHARE %>% filter(Period=="2004")


ggplot(Age2004, aes(x=Age,y=Depressed/Exposure*100, color=Region))+
  geom_point()+
  geom_smooth()+
  geom_text(aes(label=Country), size=3,vjust=1)+
  labs(x="Age",
       y="Prevalence (by 100)")+
  theme_bw()+
  scale_linetype_discrete("Region") +
  scale_shape_discrete("Region") +
  scale_colour_discrete("Region")+
  facet_wrap(~Sex)

Standard: Structure of Overall Population of 6 countries in 2004 (which was the reference used for standardization)
##Obtain Standardized Rates

StructureVector<- SHARE %>% filter(Period=="2004") %>%
  group_by(Age)%>%
  summarize(Stand=sum(Exposure)) 


overall<-sum(StructureVector$Stand)

StructureVector$Standard <- StructureVector$Stand/overall
sum(StructureVector$Standard)

SRATEALL <- SHARE  %>%
  group_by(Country,Age,Sex,Period)  %>%
  summarize(Depressed=sum(Depressed),
            Exposure=sum(Exposure),
            Prevalence=Depressed/Exposure)


SRATEALL$PropRef <- rep((StructureVector$Standard),length.out=432)
SRATEALL$StandprevALL<-SRATEALL$Prevalence*SRATEALL$PropRef

SRATEALL <- SRATEALL  %>%
  group_by(Country,Sex,Period)  %>%
  summarize(Cruderate=sum(Prevalence)*10,
            Standardizedrate=sum(StandprevALL)*100)



##plot age standardized rates for population >50

### 
#title="Figure 2: Age-Standardized Depressed Incidence Rates in 2004-2017"


ggplot(transform(SRATEALL, Country=factor(Country,
                                          levels=c("Denmark","Sweden","Germany",
                                                   "Italy","Spain","France"))), aes(x=Period,y=Standardizedrate, color=Sex))+
  geom_line(lty=2)+
  geom_point()+
  labs(x="Year",
       y="Standardized Prevalence (by 100)")+
  theme_bw()+
  scale_linetype_discrete("Sex") +
  scale_shape_discrete("Sex") +
  scale_colour_discrete("Sex")+
  facet_wrap(~Country)

#


#Create Cohort for 2d plots, equaling 2013 and 2015 to ...

Age_Groups<-c("50-53","54-57","58-61","62-65","66-69","70-73","74-77",
              "78-81","82 and +")

##reconvert as factors for better plotting 
SHARE$Year <- as.factor(SHARE$Period)
SHARE$Birth_Cohort <- as.factor(SHARE$Cohort)


##Filter out by sex separately for visualizing

#Age-Period Plots: Period by Age


SHARE2DPLOTS <- SHARE  %>%
  group_by(Country,Sex,Age,Period,Birth_Cohort,Cohort)  %>%
  summarize(Exposure=sum(Exposure),
            Depressed=sum(Depressed))

sum(SHARE2DPLOTS$Exposure)

SHARE2DPLOTS$Prevalence<-SHARE2DPLOTS$Depressed/SHARE2DPLOTS$Exposure
SHARE2DPLOTS$Year <- as.factor(SHARE2DPLOTS$Period)


SHARE2DPLOTS$Age_Groups <- as.factor(SHARE2DPLOTS$Age)


levels(SHARE2DPLOTS$Age_Groups)[1]<-"50-53"
levels(SHARE2DPLOTS$Age_Groups)[2]<-"54-57"
levels(SHARE2DPLOTS$Age_Groups)[3]<-"58-61"
levels(SHARE2DPLOTS$Age_Groups)[4]<-"62-65"
levels(SHARE2DPLOTS$Age_Groups)[5]<-"66-69"
levels(SHARE2DPLOTS$Age_Groups)[6]<-"70-73"
levels(SHARE2DPLOTS$Age_Groups)[7]<-"74-77"
levels(SHARE2DPLOTS$Age_Groups)[8]<-"78-81"
levels(SHARE2DPLOTS$Age_Groups)[9]<-"82+"

 #   this is to get the table with the exposures and rates in the appendix                                             
##write.table(SHARE2DPLOTS,"DepressionMACROExposuresbyperiod.xls",sep=" ")

###Period BY age plot: Figure 1 of appendix


ggplot(transform(SHARE2DPLOTS, Country=factor(Country,
                                          levels=c("Denmark","Sweden","Germany",
                                                   "Italy","Spain","France"))), aes(x=Age,y=Prevalence*100, color=Year))+

  geom_line()+
  labs(x="Age"
       ,y="Prevalence (by 100)")+
  theme_bw()+
  facet_wrap(~Country+Sex)


##AGE by PERIOD PLOT: Figure 2 of appendix



ggplot(transform(SHARE2DPLOTS, Country=factor(Country,
                                              levels=c("Denmark","Sweden","Germany",
                                                       "Italy","Spain","France"))), aes(x=Period,y=Prevalence*100, color=Age_Groups))+
  geom_line()+
  #scale_y_log10()+
  #scale_x_log10()+
  labs(x="Age"
       ,y="Prevalence (by 100)")+
  theme_bw()+
  facet_wrap(~Country+Sex)


##Age Cohort / Cohort Age (Figures 3 and 4 appendix)

#Figure 3 Appendix: Cohort by Age



ggplot(transform(SHARE2DPLOTS, Country=factor(Country,
                                              levels=c("Denmark","Sweden","Germany",
                                                       "Italy","Spain","France"))), aes(x=Age,y=Prevalence*100, color=Birth_Cohort))+
  geom_line()+
  #scale_y_log10()+
  #scale_x_log10()+
  labs(x="Age",
       y="Prevalence (by 100)")+
  theme_bw()+
  facet_wrap (~Country+Sex)

#Figure 4 Appendix: Age by Cohort


ggplot(transform(SHARE2DPLOTS, Country=factor(Country,
                                              levels=c("Denmark","Sweden","Germany",
                                                       "Italy","Spain","France"))), aes(x=Cohort,y=Prevalence*100, color=Age_Groups))+
  geom_line()+
  #scale_y_log10()+
  #scale_x_log10()+
  labs(x="Cohort",
       y="Prevalence (by 100)")+
  theme_bw()+
  facet_wrap (~Country+Sex)

# Set APC analysis suitable for Epi Package (in an  APDY structure)

SHARE$Exposure<-round(SHARE$Exposure,digits=0)
SHARE$Depressed<-round(SHARE$Depressed,digits=0)

SHARE$Exposure<-as.integer(SHARE$Exposure)
SHARE$Depressed<-as.integer(SHARE$Depressed)

APCSHARE <- SHARE %>% 
  select(Age,Period,Depressed,Exposure,Country,Sex) %>%
  rename(A=Age,P=Period,D=Depressed,Y=Exposure)


SHAREALL <- SHARE  %>%
  group_by(Country,Age,Period,Cohort)  %>%
  summarize(Exposure=mean(Exposure),
            Depressed=mean(Depressed))

ntotal <- SHAREALL %>% group_by(Country,Period) %>% 
  summarize(n=sum(Exposure))


##Period and Cohort Major Parametrization


Denmark <- APCSHARE %>% 
  filter(Country=="Denmark") 
Sweden <- APCSHARE %>% 
  filter(Country=="Sweden")
Germany <- APCSHARE %>% 
  filter(Country=="Germany")
Italy <- APCSHARE %>% 
  filter(Country=="Italy")
Spain <- APCSHARE %>% 
  filter(Country=="Spain")
France <- APCSHARE %>% 
  filter(Country=="France")

##MALES AND FEMALES

# Period Major parameterization (APC, non linear cohort effects, with 2004 as the reference period)

apc.Denmale <- apc.fit( subset( Denmark, Sex== "Male" ),
                          parm = "APC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


apc.Swemale <- apc.fit( subset( Sweden, Sex== "Male" ),
                          parm = "APC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


apc.Germale <- apc.fit( subset( Germany, Sex== "Male" ),
                          parm = "APC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))

apc.Itamale <- apc.fit( subset( Italy, Sex== "Male" ),
                          parm = "APC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


apc.Spamale <- apc.fit( subset( Spain, Sex== "Male" ),
                          parm = "APC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))

apc.Framale <- apc.fit( subset( France, Sex== "Male" ),
                          parm = "APC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


apc.Denfemale <- apc.fit( subset( Denmark, Sex== "Female" ),
                            parm = "APC",
                            ref.p = 2004,
                            scale=100,
                            npar=c(A=5, P=3, C=5))


apc.Swefemale <- apc.fit( subset( Sweden, Sex== "Female" ),
                            parm = "APC",
                            ref.p = 2004,
                            scale=100,
                            npar=c(A=5, P=3, C=5))



apc.Gerfemale <- apc.fit( subset( Germany, Sex== "Female" ),
                            parm = "APC",
                            ref.p = 2004,
                            scale=100,
                            npar=c(A=5, P=3, C=5))

apc.Itafemale <- apc.fit( subset( Italy, Sex== "Female" ),
                            parm = "APC",
                            ref.p = 2004,
                            scale=100,
                            npar=c(A=5, P=3, C=5))

apc.Spafemale <- apc.fit( subset( Spain, Sex== "Female" ),
                            parm = "APC",
                            ref.p = 2004,
                            scale=100,
                            npar=c(A=5, P=3, C=5))

apc.Frafemale <- apc.fit( subset( France, Sex== "Female" ),
                            parm = "APC",
                            ref.p = 2004,
                            scale=100,
                            npar=c(A=5, P=3, C=5))

                                                                  
# Obtain the anovas to calculate the analysis of deviance (to get figure 3, that's why I
# wrote the table (the allanovas.xls or "all analysis of variance file" to get the raw output) and did that part in excel. 
#  This in R requires probably a loop or a function that I could not get quite right, but basically
#  But it is explained in the "anovas procedure for figure 3.xls" and shown clearly                                                                
                                                                  
                                                                  
anova1 <- apc.Denmale[["Anova"]]
anova2 <- apc.Swemale[["Anova"]]
anova3 <- apc.Germale[["Anova"]]
anova4 <- apc.Itamale[["Anova"]]
anova5 <- apc.Spamale[["Anova"]]
anova6 <- apc.Framale[["Anova"]]

anova7 <- apc.Denfemale[["Anova"]]
anova8 <- apc.Swefemale[["Anova"]]
anova9 <- apc.Gerfemale[["Anova"]]
anova10 <- apc.Itafemale[["Anova"]]
anova11 <- apc.Spafemale[["Anova"]]
anova12 <- apc.Frafemale[["Anova"]]

allanovas <- rbind(anova1,anova2,anova3,anova4,anova5,anova6,
                   anova7,anova8,anova9,anova10,anova11,anova12)
allanovas$`Mod. dev.` <- round(allanovas$`Mod. dev.`,digits=3)

# this is to get the raw table, which later was cleaned up in the "anovas procedure for figure 3.xls" file and then in the anovasbysex to read it in .csv file
#write.table(allanovas,"all analyses of variance.xls",sep=" ")

library(readr)
#anovasbysex <- read_table2("C:/route/anovasbysex.txt")

# we filter age because we want to plot the reduction of the other three components: Age Drift, Age Period and Age Period Cohort
anovasbysex2 <- anovasbysex %>% 
  filter(Component!="Age")

ggplot(transform(anovasbysex2, Component=factor(Component,
                                              levels=c("APC","AP","AD"))), aes(x = Sex,
                                                                                         y = Contribution,
                                                                                         fill= Component))+
  geom_bar(stat= "identity", position="stack") +
  theme_bw()+
  geom_hline(yintercept = 0) +
  labs(x="Sex",
       y="Contribution") +
  facet_wrap(~ Country)


###estimating the drift (Table 1)

apc.Denmale[["Drift"]]
apc.Denfemale[["Drift"]]

apc.Swemale[["Drift"]]
apc.Swefemale[["Drift"]]

apc.Germale[["Drift"]]
apc.Gerfemale[["Drift"]]

apc.Itamale[["Drift"]]
apc.Itafemale[["Drift"]]

apc.Spamale[["Drift"]]
apc.Spafemale[["Drift"]]

apc.Framale[["Drift"]]
apc.Frafemale[["Drift"]]


###Cohort Major Parameterization or ACP (Alternative, with 1944 Cohort as reference), for Males and Females


acp.Denmale <- apc.fit( subset( Denmark, Sex== "Male" ),
                          parm = "ACP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))



acp.Swemale <- apc.fit( subset( Sweden, Sex== "Male" ),
                          parm = "ACP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


acp.Germale <- apc.fit( subset( Germany, Sex== "Male" ),
                          parm = "ACP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


acp.Itamale <- apc.fit( subset( Italy, Sex== "Male" ),
                          parm = "ACP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))



acp.Spamale <- apc.fit( subset( Spain, Sex== "Male" ),
                          parm = "ACP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


acp.Framale <- apc.fit( subset( France, Sex== "Male" ),
                          parm = "ACP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


acp.Denfemale <- apc.fit( subset( Denmark, Sex== "Female" ),
                            parm = "ACP",
                            ref.c = 1944,
                            scale=100,
                            npar=c(A=5, P=3, C=5))


acp.Swefemale <- apc.fit( subset( Sweden, Sex== "Female" ),
                            parm = "ACP",
                            ref.c = 1944,
                            scale=100,
                            npar=c(A=5, P=3, C=5))



acp.Gerfemale <- apc.fit( subset( Germany, Sex== "Female" ),
                            parm = "ACP",
                            ref.c = 1944,
                            scale=100,
                            npar=c(A=5, P=3, C=5))


acp.Itafemale <- apc.fit( subset( Italy, Sex== "Female" ),
                           parm = "ACP",
                            ref.c = 1944,
                            scale=100,
                            npar=c(A=5, P=3, C=5))

acp.Spafemale <- apc.fit( subset( Spain, Sex== "Female" ),
                            parm = "ACP",
                            ref.c = 1944,
                            scale=100,
                            npar=c(A=5, P=3, C=5))


acp.Frafemale <- apc.fit( subset( France, Sex== "Female" ),
                            parm = "ACP",
                            ref.c = 1944,
                            scale=100,
                            npar=c(A=5, P=3, C=5))




### Plot of the APC Models
# Males 

par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")
title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( apc.Denmale$Age[,1], apc.Denmale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( apc.Denmale$Per[,1], apc.Denmale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( apc.Denmale$Coh[,1], apc.Denmale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 2004, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")
title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( apc.Swemale$Age[,1], apc.Swemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( apc.Swemale$Per[,1], apc.Swemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( apc.Swemale$Coh[,1], apc.Swemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )

matshade( apc.Germale$Age[,1], apc.Germale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( apc.Germale$Per[,1], apc.Germale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( apc.Germale$Coh[,1], apc.Germale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( apc.Itamale$Age[,1], apc.Itamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( apc.Itamale$Per[,1], apc.Itamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( apc.Itamale$Coh[,1], apc.Itamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( apc.Spamale$Age[,1], apc.Spamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( apc.Spamale$Per[,1], apc.Spamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( apc.Spamale$Coh[,1], apc.Spamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( apc.Framale$Age[,1], apc.Framale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( apc.Framale$Per[,1], apc.Framale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( apc.Framale$Coh[,1], apc.Framale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")
pc.points( 2004, 1, pch=16 )


###Females


par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( apc.Denfemale$Age[,1], apc.Denfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( apc.Denfemale$Per[,1], apc.Denfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( apc.Denfemale$Coh[,1], apc.Denfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 2004, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)

pc.points( 2004, 1, pch=16 )
matshade( apc.Swefemale$Age[,1], apc.Swefemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( apc.Swefemale$Per[,1], apc.Swefemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( apc.Swefemale$Coh[,1], apc.Swefemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )

matshade( apc.Gerfemale$Age[,1], apc.Gerfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( apc.Gerfemale$Per[,1], apc.Gerfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( apc.Gerfemale$Coh[,1], apc.Gerfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( apc.Itafemale$Age[,1], apc.Itafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( apc.Itafemale$Per[,1], apc.Itafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( apc.Itafemale$Coh[,1], apc.Itafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( apc.Spafemale$Age[,1], apc.Spafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( apc.Spafemale$Per[,1], apc.Spafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( apc.Spafemale$Coh[,1], apc.Spafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( apc.Frafemale$Age[,1], apc.Frafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( apc.Frafemale$Per[,1], apc.Frafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( apc.Frafemale$Coh[,1], apc.Frafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")
pc.points( 2004, 1, pch=16 )

###Plot of the ACP models  Males and Females
#Males



par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( acp.Denmale$Age[,1], acp.Denmale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( acp.Denmale$Per[,1], acp.Denmale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( acp.Denmale$Coh[,1], acp.Denmale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 1944, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( acp.Swemale$Age[,1], acp.Swemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( acp.Swemale$Per[,1], acp.Swemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( acp.Swemale$Coh[,1], acp.Swemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )

matshade( acp.Germale$Age[,1], acp.Germale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( acp.Germale$Per[,1], acp.Germale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( acp.Germale$Coh[,1], acp.Germale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 1944, 1, pch=16 )
matshade( acp.Itamale$Age[,1], acp.Itamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( acp.Itamale$Per[,1], acp.Itamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( acp.Itamale$Coh[,1], acp.Itamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( acp.Spamale$Age[,1], acp.Spamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( acp.Spamale$Per[,1], acp.Spamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( acp.Spamale$Coh[,1], acp.Spamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( acp.Framale$Age[,1], acp.Framale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( acp.Framale$Per[,1], acp.Framale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( acp.Framale$Coh[,1], acp.Framale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")

pc.points( 1944, 1, pch=16 )


###Females


par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( acp.Denfemale$Age[,1], acp.Denfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( acp.Denfemale$Per[,1], acp.Denfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( acp.Denfemale$Coh[,1], acp.Denfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 1944, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( acp.Swefemale$Age[,1], acp.Swefemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( acp.Swefemale$Per[,1], acp.Swefemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( acp.Swefemale$Coh[,1], acp.Swefemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )

matshade( acp.Gerfemale$Age[,1], acp.Gerfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( acp.Gerfemale$Per[,1], acp.Gerfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( acp.Gerfemale$Coh[,1], acp.Gerfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( acp.Itafemale$Age[,1], acp.Itafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( acp.Itafemale$Per[,1], acp.Itafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( acp.Itafemale$Coh[,1], acp.Itafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( acp.Spafemale$Age[,1], acp.Spafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( acp.Spafemale$Per[,1], acp.Spafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( acp.Spafemale$Coh[,1], acp.Spafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( acp.Frafemale$Age[,1], acp.Frafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( acp.Frafemale$Per[,1], acp.Frafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( acp.Frafemale$Coh[,1], acp.Frafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")

pc.points( 1944, 1, pch=16 )


# Other alternative parameterizations (AdPC and AdCP, without any linear drift, so only looking for non-linear effects. This should be similar to the Median Polish procedure)

##MALES AND FEMALES  Ad PC and Ad CP



adpc.Denmale <- apc.fit( subset( Denmark, Sex== "Male" ),
                        parm = "AdPC",
                        ref.p = 2004,
                        scale=100,
                        npar=c(A=5, P=3, C=5))


adpc.Swemale <- apc.fit( subset( Sweden, Sex== "Male" ),
                        parm = "AdPC",
                        ref.p = 2004,
                        scale=100,
                        npar=c(A=5, P=3, C=5))


adpc.Germale <- apc.fit( subset( Germany, Sex== "Male" ),
                        parm = "AdPC",
                        ref.p = 2004,
                        scale=100,
                        npar=c(A=5, P=3, C=5))

adpc.Itamale <- apc.fit( subset( Italy, Sex== "Male" ),
                        parm = "AdPC",
                        ref.p = 2004,
                        scale=100,
                        npar=c(A=5, P=3, C=5))


adpc.Spamale <- apc.fit( subset( Spain, Sex== "Male" ),
                        parm = "AdPC",
                        ref.p = 2004,
                        scale=100,
                        npar=c(A=5, P=3, C=5))

adpc.Framale <- apc.fit( subset( France, Sex== "Male" ),
                        parm = "AdPC",
                        ref.p = 2004,
                        scale=100,
                        npar=c(A=5, P=3, C=5))


adpc.Denfemale <- apc.fit( subset( Denmark, Sex== "Female" ),
                          parm = "AdPC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


adpc.Swefemale <- apc.fit( subset( Sweden, Sex== "Female" ),
                          parm = "AdPC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))



adpc.Gerfemale <- apc.fit( subset( Germany, Sex== "Female" ),
                          parm = "AdPC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))

adpc.Itafemale <- apc.fit( subset( Italy, Sex== "Female" ),
                          parm = "AdPC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))

adpc.Spafemale <- apc.fit( subset( Spain, Sex== "Female" ),
                          parm = "AdPC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))

adpc.Frafemale <- apc.fit( subset( France, Sex== "Female" ),
                          parm = "AdPC",
                          ref.p = 2004,
                          scale=100,
                          npar=c(A=5, P=3, C=5))



adcp.Denmale <- apc.fit( subset( Denmark, Sex== "Male" ),
                        parm = "AdCP",
                        ref.c = 1944,
                        scale=100,
                        npar=c(A=5, P=3, C=5))



adcp.Swemale <- apc.fit( subset( Sweden, Sex== "Male" ),
                        parm = "AdCP",
                        ref.c = 1944,
                        scale=100,
                        npar=c(A=5, P=3, C=5))


adcp.Germale <- apc.fit( subset( Germany, Sex== "Male" ),
                        parm = "AdCP",
                        ref.c = 1944,
                        scale=100,
                        npar=c(A=5, P=3, C=5))


adcp.Itamale <- apc.fit( subset( Italy, Sex== "Male" ),
                        parm = "AdCP",
                        ref.c = 1944,
                        scale=100,
                        npar=c(A=5, P=3, C=5))



adcp.Spamale <- apc.fit( subset( Spain, Sex== "Male" ),
                        parm = "AdCP",
                        ref.c = 1944,
                        scale=100,
                        npar=c(A=5, P=3, C=5))


adcp.Framale <- apc.fit( subset( France, Sex== "Male" ),
                        parm = "AdCP",
                        ref.c = 1944,
                        scale=100,
                        npar=c(A=5, P=3, C=5))


adcp.Denfemale <- apc.fit( subset( Denmark, Sex== "Female" ),
                          parm = "AdCP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


adcp.Swefemale <- apc.fit( subset( Sweden, Sex== "Female" ),
                          parm = "AdCP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))



adcp.Gerfemale <- apc.fit( subset( Germany, Sex== "Female" ),
                          parm = "AdCP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


adcp.Itafemale <- apc.fit( subset( Italy, Sex== "Female" ),
                          parm = "AdCP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))

adcp.Spafemale <- apc.fit( subset( Spain, Sex== "Female" ),
                          parm = "AdCP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


adcp.Frafemale <- apc.fit( subset( France, Sex== "Female" ),
                          parm = "AdCP",
                          ref.c = 1944,
                          scale=100,
                          npar=c(A=5, P=3, C=5))


### plot 2nd order (non linear only) effects



### AdPC males  reference 2004


par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")
title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( adpc.Denmale$Age[,1], adpc.Denmale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( adpc.Denmale$Per[,1], adpc.Denmale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( adpc.Denmale$Coh[,1], adpc.Denmale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 2004, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")
title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( adpc.Swemale$Age[,1], adpc.Swemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( adpc.Swemale$Per[,1], adpc.Swemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( adpc.Swemale$Coh[,1], adpc.Swemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )

matshade( adpc.Germale$Age[,1], adpc.Germale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( adpc.Germale$Per[,1], adpc.Germale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( adpc.Germale$Coh[,1], adpc.Germale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( adpc.Itamale$Age[,1], adpc.Itamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( adpc.Itamale$Per[,1], adpc.Itamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( adpc.Itamale$Coh[,1], adpc.Itamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( adpc.Spamale$Age[,1], adpc.Spamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( adpc.Spamale$Per[,1], adpc.Spamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( adpc.Spamale$Coh[,1], adpc.Spamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( adpc.Framale$Age[,1], adpc.Framale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( adpc.Framale$Per[,1], adpc.Framale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( adpc.Framale$Coh[,1], adpc.Framale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")
pc.points( 2004, 1, pch=16 )


###Females


par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( adpc.Denfemale$Age[,1], adpc.Denfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( adpc.Denfemale$Per[,1], adpc.Denfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( adpc.Denfemale$Coh[,1], adpc.Denfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 2004, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)

pc.points( 2004, 1, pch=16 )
matshade( adpc.Swefemale$Age[,1], adpc.Swefemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( adpc.Swefemale$Per[,1], adpc.Swefemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( adpc.Swefemale$Coh[,1], adpc.Swefemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )

matshade( adpc.Gerfemale$Age[,1], adpc.Gerfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( adpc.Gerfemale$Per[,1], adpc.Gerfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( adpc.Gerfemale$Coh[,1], adpc.Gerfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( adpc.Itafemale$Age[,1], adpc.Itafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( adpc.Itafemale$Per[,1], adpc.Itafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( adpc.Itafemale$Coh[,1], adpc.Itafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( adpc.Spafemale$Age[,1], adpc.Spafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( adpc.Spafemale$Per[,1], adpc.Spafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( adpc.Spafemale$Coh[,1], adpc.Spafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( adpc.Frafemale$Age[,1], adpc.Frafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( adpc.Frafemale$Per[,1], adpc.Frafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( adpc.Frafemale$Coh[,1], adpc.Frafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")
pc.points( 2004, 1, pch=16 )

###AdCP  Males and Females
#Males



par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( adcp.Denmale$Age[,1], adcp.Denmale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( adcp.Denmale$Per[,1], adcp.Denmale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( adcp.Denmale$Coh[,1], adcp.Denmale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 1944, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( adcp.Swemale$Age[,1], adcp.Swemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( adcp.Swemale$Per[,1], adcp.Swemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( adcp.Swemale$Coh[,1], adcp.Swemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )

matshade( adcp.Germale$Age[,1], adcp.Germale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( adcp.Germale$Per[,1], adcp.Germale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( adcp.Germale$Coh[,1], adcp.Germale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 1944, 1, pch=16 )
matshade( adcp.Itamale$Age[,1], adcp.Itamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( adcp.Itamale$Per[,1], adcp.Itamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( adcp.Itamale$Coh[,1], adcp.Itamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( adcp.Spamale$Age[,1], adcp.Spamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( adcp.Spamale$Per[,1], adcp.Spamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( adcp.Spamale$Coh[,1], adcp.Spamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( adcp.Framale$Age[,1], adcp.Framale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( adcp.Framale$Per[,1], adcp.Framale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( adcp.Framale$Coh[,1], adcp.Framale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")

pc.points( 1944, 1, pch=16 )


###Females


par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( adcp.Denfemale$Age[,1], adcp.Denfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( adcp.Denfemale$Per[,1], adcp.Denfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( adcp.Denfemale$Coh[,1], adcp.Denfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 1944, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( adcp.Swefemale$Age[,1], adcp.Swefemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( adcp.Swefemale$Per[,1], adcp.Swefemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( adcp.Swefemale$Coh[,1], adcp.Swefemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )

matshade( adcp.Gerfemale$Age[,1], adcp.Gerfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( adcp.Gerfemale$Per[,1], adcp.Gerfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( adcp.Gerfemale$Coh[,1], adcp.Gerfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( adcp.Itafemale$Age[,1], adcp.Itafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( adcp.Itafemale$Per[,1], adcp.Itafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( adcp.Itafemale$Coh[,1], adcp.Itafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( adcp.Spafemale$Age[,1], adcp.Spafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( adcp.Spafemale$Per[,1], adcp.Spafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( adcp.Spafemale$Coh[,1], adcp.Spafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( adcp.Frafemale$Age[,1], adcp.Frafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( adcp.Frafemale$Per[,1], adcp.Frafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( adcp.Frafemale$Coh[,1], adcp.Frafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")

pc.points( 1944, 1, pch=16 )




##Additional parameterizations: MALES AND FEMALES  Ad-P-C and Ad-C-P (only linear)



AdminuspcDenmale <- apc.fit( subset( Denmark, Sex== "Male" ),
                             parm = "Ad-P-C",
                             ref.p = 2004,
                             scale=100,
                             npar=c(A=5, P=3, C=5))


AdminuspcSwemale <- apc.fit( subset( Sweden, Sex== "Male" ),
                             parm = "Ad-P-C",
                             ref.p = 2004,
                             scale=100,
                             npar=c(A=5, P=3, C=5))


AdminuspcGermale <- apc.fit( subset( Germany, Sex== "Male" ),
                             parm = "Ad-P-C",
                             ref.p = 2004,
                             scale=100,
                             npar=c(A=5, P=3, C=5))

AdminuspcItamale <- apc.fit( subset( Italy, Sex== "Male" ),
                             parm = "Ad-P-C",
                             ref.p = 2004,
                             scale=100,
                             npar=c(A=5, P=3, C=5))


AdminuspcSpamale <- apc.fit( subset( Spain, Sex== "Male" ),
                             parm = "Ad-P-C",
                             ref.p = 2004,
                             scale=100,
                             npar=c(A=5, P=3, C=5))

AdminuspcFramale <- apc.fit( subset( France, Sex== "Male" ),
                             parm = "Ad-P-C",
                             ref.p = 2004,
                             scale=100,
                             npar=c(A=5, P=3, C=5))


AdminuspcDenfemale <- apc.fit( subset( Denmark, Sex== "Female" ),
                               parm = "Ad-P-C",
                               ref.p = 2004,
                               scale=100,
                               npar=c(A=5, P=3, C=5))


AdminuspcSwefemale <- apc.fit( subset( Sweden, Sex== "Female" ),
                               parm = "Ad-P-C",
                               ref.p = 2004,
                               scale=100,
                               npar=c(A=5, P=3, C=5))



AdminuspcGerfemale <- apc.fit( subset( Germany, Sex== "Female" ),
                               parm = "Ad-P-C",
                               ref.p = 2004,
                               scale=100,
                               npar=c(A=5, P=3, C=5))

AdminuspcItafemale <- apc.fit( subset( Italy, Sex== "Female" ),
                               parm = "Ad-P-C",
                               ref.p = 2004,
                               scale=100,
                               npar=c(A=5, P=3, C=5))

AdminuspcSpafemale <- apc.fit( subset( Spain, Sex== "Female" ),
                               parm = "Ad-P-C",
                               ref.p = 2004,
                               scale=100,
                               npar=c(A=5, P=3, C=5))

AdminuspcFrafemale <- apc.fit( subset( France, Sex== "Female" ),
                               parm = "Ad-P-C",
                               ref.p = 2004,
                               scale=100,
                               npar=c(A=5, P=3, C=5))



Adminuscp.Denmale <- apc.fit( subset( Denmark, Sex== "Male" ),
                              parm = "Ad-C-P",
                              ref.c = 1944,
                              scale=100,
                              npar=c(A=5, P=3, C=5))



Adminuscp.Swemale <- apc.fit( subset( Sweden, Sex== "Male" ),
                              parm = "Ad-C-P",
                              ref.c = 1944,
                              scale=100,
                              npar=c(A=5, P=3, C=5))


Adminuscp.Germale <- apc.fit( subset( Germany, Sex== "Male" ),
                              parm = "Ad-C-P",
                              ref.c = 1944,
                              scale=100,
                              npar=c(A=5, P=3, C=5))


Adminuscp.Itamale <- apc.fit( subset( Italy, Sex== "Male" ),
                              parm = "Ad-C-P",
                              ref.c = 1944,
                              scale=100,
                              npar=c(A=5, P=3, C=5))



Adminuscp.Spamale <- apc.fit( subset( Spain, Sex== "Male" ),
                              parm = "Ad-C-P",
                              ref.c = 1944,
                              scale=100,
                              npar=c(A=5, P=3, C=5))


Adminuscp.Framale <- apc.fit( subset( France, Sex== "Male" ),
                              parm = "Ad-C-P",
                              ref.c = 1944,
                              scale=100,
                              npar=c(A=5, P=3, C=5))


Adminuscp.Denfemale <- apc.fit( subset( Denmark, Sex== "Female" ),
                                parm = "Ad-C-P",
                                ref.c = 1944,
                                scale=100,
                                npar=c(A=5, P=3, C=5))


Adminuscp.Swefemale <- apc.fit( subset( Sweden, Sex== "Female" ),
                                parm = "Ad-C-P",
                                ref.c = 1944,
                                scale=100,
                                npar=c(A=5, P=3, C=5))



Adminuscp.Gerfemale <- apc.fit( subset( Germany, Sex== "Female" ),
                                parm = "Ad-C-P",
                                ref.c = 1944,
                                scale=100,
                                npar=c(A=5, P=3, C=5))


Adminuscp.Itafemale <- apc.fit( subset( Italy, Sex== "Female" ),
                                parm = "Ad-C-P",
                                ref.c = 1944,
                                scale=100,
                                npar=c(A=5, P=3, C=5))

Adminuscp.Spafemale <- apc.fit( subset( Spain, Sex== "Female" ),
                                parm = "Ad-C-P",
                                ref.c = 1944,
                                scale=100,
                                npar=c(A=5, P=3, C=5))


Adminuscp.Frafemale <- apc.fit( subset( France, Sex== "Female" ),
                                parm = "Ad-C-P",
                                ref.c = 1944,
                                scale=100,
                                npar=c(A=5, P=3, C=5))


### plot only linear effects



### Ad-P-C males  reference 2004


par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")
title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( AdminuspcDenmale$Age[,1], AdminuspcDenmale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( AdminuspcDenmale$Per[,1], AdminuspcDenmale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( AdminuspcDenmale$Coh[,1], AdminuspcDenmale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 2004, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")
title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( AdminuspcSwemale$Age[,1], AdminuspcSwemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( AdminuspcSwemale$Per[,1], AdminuspcSwemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( AdminuspcSwemale$Coh[,1], AdminuspcSwemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )

matshade( AdminuspcGermale$Age[,1], AdminuspcGermale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( AdminuspcGermale$Per[,1], AdminuspcGermale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( AdminuspcGermale$Coh[,1], AdminuspcGermale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( AdminuspcItamale$Age[,1], AdminuspcItamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( AdminuspcItamale$Per[,1], AdminuspcItamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( AdminuspcItamale$Coh[,1], AdminuspcItamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( AdminuspcSpamale$Age[,1], AdminuspcSpamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( AdminuspcSpamale$Per[,1], AdminuspcSpamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( AdminuspcSpamale$Coh[,1], AdminuspcSpamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( AdminuspcFramale$Age[,1], AdminuspcFramale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( AdminuspcFramale$Per[,1], AdminuspcFramale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( AdminuspcFramale$Coh[,1], AdminuspcFramale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")
pc.points( 2004, 1, pch=16 )


###Females


par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( AdminuspcDenfemale$Age[,1], AdminuspcDenfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( AdminuspcDenfemale$Per[,1], AdminuspcDenfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( AdminuspcDenfemale$Coh[,1], AdminuspcDenfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 2004, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)

pc.points( 2004, 1, pch=16 )
matshade( AdminuspcSwefemale$Age[,1], AdminuspcSwefemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( AdminuspcSwefemale$Per[,1], AdminuspcSwefemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( AdminuspcSwefemale$Coh[,1], AdminuspcSwefemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )

matshade( AdminuspcGerfemale$Age[,1], AdminuspcGerfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( AdminuspcGerfemale$Per[,1], AdminuspcGerfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( AdminuspcGerfemale$Coh[,1], AdminuspcGerfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( AdminuspcItafemale$Age[,1], AdminuspcItafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( AdminuspcItafemale$Per[,1], AdminuspcItafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( AdminuspcItafemale$Coh[,1], AdminuspcItafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 2004, 1, pch=16 )
matshade( AdminuspcSpafemale$Age[,1], AdminuspcSpafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( AdminuspcSpafemale$Per[,1], AdminuspcSpafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( AdminuspcSpafemale$Coh[,1], AdminuspcSpafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( AdminuspcFrafemale$Age[,1], AdminuspcFrafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( AdminuspcFrafemale$Per[,1], AdminuspcFrafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( AdminuspcFrafemale$Coh[,1], AdminuspcFrafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")
pc.points( 2004, 1, pch=16 )

###Adminuscp  Males and Females
#Males



par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( Adminuscp.Denmale$Age[,1], Adminuscp.Denmale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( Adminuscp.Denmale$Per[,1], Adminuscp.Denmale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( Adminuscp.Denmale$Coh[,1], Adminuscp.Denmale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 1944, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( Adminuscp.Swemale$Age[,1], Adminuscp.Swemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( Adminuscp.Swemale$Per[,1], Adminuscp.Swemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( Adminuscp.Swemale$Coh[,1], Adminuscp.Swemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )

matshade( Adminuscp.Germale$Age[,1], Adminuscp.Germale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( Adminuscp.Germale$Per[,1], Adminuscp.Germale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( Adminuscp.Germale$Coh[,1], Adminuscp.Germale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)
pc.points( 1944, 1, pch=16 )
matshade( Adminuscp.Itamale$Age[,1], Adminuscp.Itamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( Adminuscp.Itamale$Per[,1], Adminuscp.Itamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( Adminuscp.Itamale$Coh[,1], Adminuscp.Itamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( Adminuscp.Spamale$Age[,1], Adminuscp.Spamale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( Adminuscp.Spamale$Per[,1], Adminuscp.Spamale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( Adminuscp.Spamale$Coh[,1], Adminuscp.Spamale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( Adminuscp.Framale$Age[,1], Adminuscp.Framale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( Adminuscp.Framale$Per[,1], Adminuscp.Framale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( Adminuscp.Framale$Coh[,1], Adminuscp.Framale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")

pc.points( 1944, 1, pch=16 )


###Females


par(mfrow=c(2,3), mar=c(3,4,1,4), mgp=c(3,1,0)/1.6, las=1 )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Denmark", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("DEN"),cex=0.9, bty = "n",  horiz=F)
matshade( Adminuscp.Denfemale$Age[,1], Adminuscp.Denfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( Adminuscp.Denfemale$Per[,1], Adminuscp.Denfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="red" )
pc.matshade( Adminuscp.Denfemale$Coh[,1], Adminuscp.Denfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="red" )

pc.points( 1944, 1, pch=16 )

apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Sweden", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SWE"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( Adminuscp.Swefemale$Age[,1], Adminuscp.Swefemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( Adminuscp.Swefemale$Per[,1], Adminuscp.Swefemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
pc.matshade( Adminuscp.Swefemale$Coh[,1], Adminuscp.Swefemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="blue" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Germany", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("GER"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )

matshade( Adminuscp.Gerfemale$Age[,1], Adminuscp.Gerfemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( Adminuscp.Gerfemale$Per[,1], Adminuscp.Gerfemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
pc.matshade( Adminuscp.Gerfemale$Coh[,1], Adminuscp.Gerfemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="black" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Italy", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("ITA"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( Adminuscp.Itafemale$Age[,1], Adminuscp.Itafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( Adminuscp.Itafemale$Per[,1], Adminuscp.Itafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
pc.matshade( Adminuscp.Itafemale$Coh[,1], Adminuscp.Itafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="forestgreen" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "Spain", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("SPA"),cex=0.9, bty = "n",  horiz=F)

pc.points( 1944, 1, pch=16 )
matshade( Adminuscp.Spafemale$Age[,1], Adminuscp.Spafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( Adminuscp.Spafemale$Per[,1], Adminuscp.Spafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
pc.matshade( Adminuscp.Spafemale$Coh[,1], Adminuscp.Spafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="darkorange" )
apc.frame( a.lab=c(50,60,70,80,85),
           a.tic=c(50,60,70,80,85),
           cp.lab=c(1920,1940,1960,2000,2020),
           cp.tic=c(1920,1940,1960,2000,2020),
           r.lab=c(c(7.5,15,30,45,60,75),c(7.5,15,30,45,60,75)),
           r.tic=c(7.5,15,30,45,60,75),
           rr.ref=30,
           r.txt="Prevalence per 100",
           gap = 20,
           cp.txt="Calendar Year")

title(main = "France", 
      cex.main = 1.2,   font.main= 1, col.main= "black")
#legend("bottomright", legend=c("FRA"),cex=0.9, bty = "n",  horiz=F)
matshade( Adminuscp.Frafemale$Age[,1], Adminuscp.Frafemale$Age[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( Adminuscp.Frafemale$Per[,1], Adminuscp.Frafemale$Per[,-1],lwd=2,alpha=0.2,lty="22", col="purple" )
pc.matshade( Adminuscp.Frafemale$Coh[,1], Adminuscp.Frafemale$Coh[,-1],lwd=2,alpha=0.2,lty="22", col="purple")

pc.points( 1944, 1, pch=16 )


