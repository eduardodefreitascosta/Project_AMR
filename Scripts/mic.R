
library(readxl)
library(here)
library(lme4)
library(tidyverse)

mic <- read_excel(here("Data","mic.xlsx"))
mic$MIC<-as.numeric(mic$MIC)
mic$Animal<-as.factor(mic$Animal)

animal <- read_excel(here("Data","enumeration.xlsx"),sheet = "animal")
animal$Animal<-as.factor(animal$Animal)


mic<-left_join(mic,animal,by=c("Animal"))



dose4prob2 = function(b0,b1,b2,b3,prob){
  d = (log(prob/(1-prob))-(b0+b2+b3))/(b1)
  return(d)
}



G<-c("G1","G2","G3","G4")
Z<-c("nursery","grower/finisher")


summary(glmer(Result~MIC+Group+Zootecnic+(1|Sow/Animal),family=binomial,data=mic)->exp.glm)

exp.glm<-summary(exp.glm)

#MIC50%

dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0,coef(exp.glm)[[6]],0.5) #Nursery, G1
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[3]],coef(exp.glm)[[6]],0.5) #Nursery, G2
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[4]],coef(exp.glm)[[6]],0.5) #Nursery, G3
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[5]],coef(exp.glm)[[6]],0.5) #Nursery, G4

dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0,0,0.5) #G/F, G1
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[3]],0,0.5) #G/F, G2
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[4]],0,0.5) #G/F, G3
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[5]],0,0.5) #G/F, G4


#MIC90%

dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0,coef(exp.glm)[[6]],0.9) #Nursery, G1
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[3]],coef(exp.glm)[[6]],0.9) #Nursery, G2
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[4]],coef(exp.glm)[[6]],0.9) #Nursery, G3
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[5]],coef(exp.glm)[[6]],0.9) #Nursery, G4

dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0,0,0.9) #G/F, G1
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[3]],0,0.9) #G/F, G2
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[4]],0,0.9) #G/F, G3
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[5]],0,0.9) #G/F, G4


###################
#Linear regression#
###################

#MIC90%

mics<-c(
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0,coef(exp.glm)[[6]],0.9), #Nursery, G1
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[3]],coef(exp.glm)[[6]],0.9), #Nursery, G2
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[4]],coef(exp.glm)[[6]],0.9), #Nursery, G3
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[5]],coef(exp.glm)[[6]],0.9), #Nursery, G4

dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0,0,0.9), #G/F, G1
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[3]],0,0.9), #G/F, G2
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[4]],0,0.9), #G/F, G3
dose4prob2(coef(exp.glm)[[1]],coef(exp.glm)[[2]],coef(exp.glm)[[5]],0,0.9) #G/F, G4
)


dias<-c(0,10,24,10,0,0,0,0)

plot(mics,dias)

summary(lm(mics~dias)->ln1)

plot(ln1)

plot(dias,mics)

lines(predict(ln1),dias)

plot(mics,predict(ln1))

