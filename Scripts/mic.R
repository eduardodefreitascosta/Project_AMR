# Read data
mic <- read_excel(here("Data","mic.xlsx"))
mic$MIC<-as.numeric(mic$MIC)
mic$Animal<-as.factor(mic$Animal)

animal <- read_excel(here("Data","enumeration.xlsx"),sheet = "animal")

#Tidy data
animal$Animal<-as.factor(animal$Animal)


mic<-left_join(mic,animal,by=c("Animal"))


#MIC function
dose4prob2 = function(b0,b1,b2,b3,prob){
  d = (log(prob/(1-prob))-(b0+b2+b3))/(b1)
  return(d)
}


#Labels
G<-c("G1","G2","G3","G4")
Z<-c("nursery","grower/finisher")

#Logistic model
summary(glmer(Result~MIC+Group+Zootecnic+(1|Sow/Animal),family=binomial,data=mic)->exp.glm)

#Summary of logistic model
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

#Days vector
dias<-c(0,10,24,10,0,0,0,0)

#Linear model
summary(lm(mics~dias)->ln1)



################
# Gamma model  #
################

mic <- read_excel(here("Data","mic_reg.xlsx"))


reg_mic <- flexsurvreg(Surv(min_coli, max_coli, type="interval2") ~ sorovar , dist="gamma", data = mic)

reg_mic

ggsurvplot(reg_mic, conf.int = TRUE,fun="survival",
           ggtheme = theme_minimal(),data = mic,ylab="Survival probability",xlab="Dose")


summary(reg_mic)


mg1<-function(x){
  rate.1<-( ((reg_mic$res[2]))*exp(reg_mic$res[3]*x))
  mg1<-reg_mic$res[1]*(1/rate.1)
  mg1
}
#Derby
mg1(0)
#Typhimurium
mg1(1)


##MIC (%)
#Derby
qgamma(c(0.5,0.9),shape=reg_mic$res[1],scale=1/(reg_mic$res[2]),lower.tail=T)



#Typhimurium
qgamma(c(0.5,0.9),shape=reg_mic$res[1],scale=1/(reg_mic$res[2]*exp(reg_mic$res[3])),lower.tail=T)


#Quantis

#Derby
pgamma(c(0.5,1,2,4,8,16),shape=reg_mic$res[1],scale=1/(reg_mic$res[2]),lower.tail=T)

#Typhimurium
pgamma(c(0.5,1,2,4,8,16),shape=reg_mic$res[1],scale=1/(reg_mic$res[2]*exp(reg_mic$res[3])),lower.tail=T)
