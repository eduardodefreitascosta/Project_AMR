library(ggplot2)
library(readxl)
library(dplyr)
library(multcompView)
library(lsmeans)
library(tidyr)
library(here)
library(lme4)
library(logistf)

#########
# E coli#
#########


coli <- read_excel(here("Data","coli.xlsx"), sheet = "coli")
animal <- read_excel(here("Data","enumeration.xlsx"), sheet = "animal")

coli$Animal<-as.character(coli$Animal)
animal$Animal<-as.character(animal$Animal)

coli<-left_join(coli,animal,by="Animal")


coli2<-coli%>%
  filter( zootecnic == "nursery" |zootecnic == "grower/finisher" )

coli_wide <- spread(coli2, Antibiotic, Result)

vetor<-list()

for (i in 9:19){
  vetor[[i]]<-coli_wide[,i]
}


mod<-list()
mod1<-list()
mod2<-list()
margin1<-list()
margin2<-list()

for (i in 9:19){
  
  mod[[i]]<-glmer(unlist(vetor[[i]])~Group*zootecnic+(1|Sow/Animal),data=coli_wide,family = binomial)
  mod1[[i]]<-summary(mod[[i]])
  margin1[[i]]<-emmeans(mod[[i]], pairwise~Group|zootecnic,adjust="tukey")
  margin2[[i]]<-emmeans(mod[[i]], pairwise~zootecnic|Group,adjust="tukey")
  
  #names(mod[[i]])<-names(coli_wide)[i]
 # names(mod1[[i]])<-names(coli_wide)[i]
  names(margin1[[i]])<-names(coli_wide)[i]
  names(margin2[[i]])<-names(coli_wide)[i]
}


capture.output(margin1, file =here("Output", "output1_coli.csv"))
capture.output(margin2, file =here("Output", "output2_coli.csv"))

###############
# Enterococcus#
###############

enterococcus <- read_excel(here("Data","enterococcus.xlsx"), sheet = "enterococcus")

enterococcus$Animal<-as.character(enterococcus$Animal)

enterococcus<-left_join(enterococcus,animal,by="Animal")


enterococcus2<-subset(enterococcus, zootecnic %in%c( "nursery" , "finisher") & Antibiotic %in% c("Chloramphenicol","Ciprofloxacin","Erytromicin", "Gentamicin(120)" ,"Tetracycline" ))

enterococcus_wide <- spread(enterococcus2, Antibiotic, Result)

vetor<-list()

for (i in 7:11){
  vetor[[i]]<-enterococcus_wide[,i]
}


mod<-list()
mod1<-list()
mod2<-list()
margin1<-list()
margin2<-list()
for (i in 7:11){
  
  mod[[i]]<-glmer(unlist(vetor[[i]])~Group*zootecnic+(1|Sow/Animal),data=enterococcus_wide,family = binomial)
  mod1[[i]]<-summary(mod[[i]])
  margin1[[i]]<-emmeans(mod[[i]], pairwise~Group|zootecnic,adjust="tukey")
  margin2[[i]]<-emmeans(mod[[i]], pairwise~zootecnic|Group,adjust="tukey")
  
  names(margin1[[i]])<-names(enterococcus_wide)[i]
  names(margin2[[i]])<-names(enterococcus_wide)[i]
  
}


f_log1<-logistf(Erytromicin~Group*zootecnic,data=enterococcus_wide)
summary(f_log1)

logistftest(f_log1, test = ~ zootecnic)



f_log2<-logistf(Chloramphenicol~Group*zootecnic,data=enterococcus_wide)
summary(f_log2)
emmeans(f_log2, pairwise~zootecnic|Group,adjust="tukey")

capture.output(margin, file = "output.csv")
