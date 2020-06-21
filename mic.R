
library(readxl)
library(here)

mic <- read_excel(here("Data","mic.xlsx"))

mic$MIC<-as.numeric(mic$MIC)


dose4prob = function(b0,b1,prob){
  d = (-b0+log(-prob/(prob-1)))/b1
  return(d)
}
G<-c("G1","G2","G3","G4")
Z<-c("nursery","grower/finisher")

par(mfrow=c(2,2))

summary(glm(mic$Result[mic$Group=="G1" & mic$Zootecnic=="nursery"] ~ mic$MIC[mic$Group=="G1"& mic$Zootecnic=="nursery"],
            family=binomial,
            data=mic)->exp.glm)
dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.5)
dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.9)

####
plot(seq(0,15,0.1), 1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="n",ylab="Percentage of inibition",xlab="Antimicrobial concentration (mg/mL)")

rect(2,par("usr")[3],4,par("usr")[4],col="gray")
rect(2,par("usr")[3],4,par("usr")[4],col="gray")

lines(seq(0,15,0.1),1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="l")

text(3,0.1, "MIC50")

text(3,0.15, "MIC90")
text(0,1.1,"A", xpd=NA)
##################

summary(glm(mic$Result[mic$Group=="G2"& mic$Zootecnic=="nursery"] ~ mic$MIC[mic$Group=="G2"& mic$Zootecnic=="nursery"],
            family=binomial,
            data=mic)->exp.glm)


####
plot(seq(0,15,0.1), 1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="n",ylab="Percentage of inibition",xlab="Antimicrobial concentration (mg/mL)")

dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.5)

dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.9)

rect(2,par("usr")[3],4,par("usr")[4],col="grey")
rect(2,par("usr")[3],4,par("usr")[4],col="grey")

lines(seq(0,15,0.1),1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="l")

text(2.5,0.1, "MIC50")

text(2.5,0.15, "MIC90")
text(0,1.1,"B", xpd=NA)
##################

summary(glm(mic$Result[mic$Group=="G3"& mic$Zootecnic=="nursery"] ~ mic$MIC[mic$Group=="G3"& mic$Zootecnic=="nursery"],
            family=binomial,
            data=mic)->exp.glm)

####
plot(seq(0,15,0.1), 1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="n",ylab="Percentage of inibition",xlab="Antimicrobial concentration (mg/mL)")

dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.5)

dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.9)

rect(4,par("usr")[3],8,par("usr")[4],col="gray")
rect(4,par("usr")[3],8,par("usr")[4],col="gray")

lines(seq(0,15,0.1),1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="l")

text(5.5,0.1, "MIC50")

text(5.5,0.15, "MIC90")
text(0,1.1,"C", xpd=NA)
##################

summary(glm(mic$Result[mic$Group=="G4"& mic$Zootecnic=="nursery"] ~ mic$MIC[mic$Group=="G4"& mic$Zootecnic=="nursery"],
            family=binomial,
            data=mic)->exp.glm)

####
plot(seq(0,15,0.1), 1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="n",ylab="Percentage of inibition",xlab="Antimicrobial concentration (mg/mL)")

dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.5)
dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.9)

rect(2,par("usr")[3],4,par("usr")[4],col="gray")
rect(4,par("usr")[3],8,par("usr")[4],col="gray")

lines(seq(0,15,0.1),1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="l")

text(2.5,0.1, "MIC50")

text(4.5,0.1, "MIC90")
text(0,1.1,"D", xpd=NA)



mtext("MIC 50 and MIC 90 for Nursery", side = 3, line = -2, outer = TRUE)
##########################################################################################################################################



summary(glm(mic$Result[mic$Group=="G1" & mic$Zootecnic=="grower/finisher"] ~ mic$MIC[mic$Group=="G1"& mic$Zootecnic=="grower/finisher"],
            family=binomial,
            data=mic)->exp.glm)

####
plot(seq(0,15,0.1), 1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="n",ylab="Percentage of inibition",xlab="Antimicrobial concentration (mg/mL)")

dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.5)
dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.9)

rect(2,par("usr")[3],4,par("usr")[4],col="gray")
rect(2,par("usr")[3],4,par("usr")[4],col="gray")

lines(seq(0,15,0.1),1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="l")

text(2.5,0.1, "MIC50")

text(2.5,0.15, "MIC90")
text(0,1.1,"A", xpd=NA)
##################

summary(glm(mic$Result[mic$Group=="G2"& mic$Zootecnic=="grower/finisher"] ~ mic$MIC[mic$Group=="G2"& mic$Zootecnic=="grower/finisher"],
            family=binomial,
            data=mic)->exp.glm)


####
plot(seq(0,15,0.1), 1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="n",ylab="Percentage of inibition",xlab="Antimicrobial concentration (mg/mL)")

dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.5)
dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.9)

rect(1,par("usr")[3],2,par("usr")[4],col="gray")
rect(2,par("usr")[3],4,par("usr")[4],col="gray")

lines(seq(0,15,0.1),1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="l")

text(1.5,0.1, "MIC50")

text(2.5,0.1, "MIC90")
text(0,1.1,"B", xpd=NA)
##################

summary(glm(mic$Result[mic$Group=="G3"& mic$Zootecnic=="grower/finisher"] ~ mic$MIC[mic$Group=="G3"& mic$Zootecnic=="grower/finisher"],
            family=binomial,
            data=mic)->exp.glm)


####
plot(seq(0,15,0.1), 1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="n",ylab="Percentage of inibition",xlab="Antimicrobial concentration (mg/mL)")

dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.5)
dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.9)

rect(1,par("usr")[3],2,par("usr")[4],col="gray")
rect(2,par("usr")[3],4,par("usr")[4],col="gray")

lines(seq(0,15,0.1),1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="l")

text(1.5,0.1, "MIC50")

text(2.5,0.1, "MIC90")
text(0,1.1,"C", xpd=NA)
##################

summary(glm(mic$Result[mic$Group=="G4"& mic$Zootecnic=="grower/finisher"] ~ mic$MIC[mic$Group=="G4"& mic$Zootecnic=="grower/finisher"],
            family=binomial,
            data=mic)->exp.glm)

####
plot(seq(0,15,0.1), 1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="n",ylab="Percentage of inibition",xlab="Antimicrobial concentration (mg/mL)")

dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.5)
dose4prob(coef(exp.glm)[[1]],coef(exp.glm)[[2]],0.9)

rect(2,par("usr")[3],4,par("usr")[4],col="gray")
rect(4,par("usr")[3],8,par("usr")[4],col="gray")

lines(seq(0,15,0.1),1/(1+exp(-(exp.glm$coefficients[1]+exp.glm$coefficients[2]*seq(0,15,0.1)))),type="l")

text(2.5,0.1, "MIC50")

text(4.5,0.1, "MIC90")
text(0,1.1,"D", xpd=NA)
mtext("MIC 50 and MIC 90 for Growing/finishing", side = 3, line = -2, outer = TRUE)
##########################################################################################################################################



