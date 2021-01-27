library(ggplot2)
library(car)
library(readxl)
library(here)
library(knitr)
##############
#E. coli     #
##############


mdr_coli <- read_excel(here("Data","mdr_coli.xlsx"))

mdr_coli$Animal<-as.factor(mdr_coli$Animal)

animal <- read_excel(here("Data","enumeration.xlsx"),sheet = "animal")
animal$Animal<-as.factor(animal$Animal)

mdr_coli<-left_join(mdr_coli,animal,by=c("Animal"))


head(mdr_coli)
mdr_coli2<-subset(mdr_coli,Antibiotic=="MDR")
mdr_coli3<-subset(mdr_coli,Antibiotic=="full suscetible")



ggplot(mdr_coli2,aes(x=factor(dose_zoo),fill=factor(1-Result)))+
  geom_bar(position="fill",na.rm = TRUE)+
  labs(fill = "MDR")+
  xlab("Dose (mg)") + 
  ylab("Freqeuncy")+
  scale_color_manual(name="MDR", labels = c("Positive", "Negative","NA"))+
  facet_wrap(~zootecnic)

ggplot(mdr_coli3,aes(x=factor(dose_zoo),fill=factor(1-Result)))+
  geom_bar(position="fill",na.rm = TRUE)+
  labs(fill = "Full Susceptible")+
  xlab("Dose (mg)") + 
  ylab("Freqeuncy")+
  scale_color_manual(name="MDR", labels = c("Positive", "Negative","NA"))+
  facet_wrap(~zootecnic)


mdr_coli2$Animal<-as.factor(mdr_coli2$Animal)
mdr_coli2$Cycle<-as.factor(mdr_coli2$Cycle)
str(mdr_coli2)

summary(glmer(Result~as.numeric(dose_kg)+zootecnic+(1|Sow/Animal),data=mdr_coli2,family = binomial(link="logit"))->mod)

vif(mod)




mdr_coli3$Animal<-as.factor(mdr_coli3$Animal)
mdr_coli3$Cycle<-as.factor(mdr_coli3$Cycle)
str(mdr_coli3)


summary(glmer(Result~as.numeric(dose_kg)+zootecnic+(1|Sow/Animal),data=mdr_coli3,family = binomial(link="logit"))->mod1)

vif(mod1)


##############
#Enterococcus#
##############
mdr_ente <-read_excel(here("Data","mdr_ente.xlsx"))

mdr_ente$Animal<-as.factor(mdr_ente$Animal)

mdr_ente<-left_join(mdr_ente,animal,by=c("Animal"))


head(mdr_ente)
mdr_ente2<-subset(mdr_ente,Antibiotic=="MDR")
mdr_ente3<-subset(mdr_ente,Antibiotic=="full suscetible")


ggplot(mdr_ente2,aes(x=factor(dose_zoo),fill=factor(1-Result)))+
  geom_bar(position="fill",na.rm = TRUE)+
  labs(fill = "MDR")+
  xlab("Dose (mg)") + 
  ylab("Freqeuncy")+
  scale_color_manual(name="MDR", labels = c("Positive", "Negative","NA"))+
  facet_wrap(~zootecnic)

ggplot(mdr_ente3,aes(x=factor(dose_zoo),fill=factor(1-Result)))+
  geom_bar(position="fill",na.rm = TRUE)+
  labs(fill = "Full Susceptible")+
  xlab("Dose (mg)") + 
  ylab("Freqeuncy")+
  scale_color_manual(name="MDR", labels = c("Positive", "Negative","NA"))+
  facet_wrap(~zootecnic)


mdr_ente2$Animal<-as.factor(mdr_ente2$Animal)
mdr_ente2$Cycle<-as.factor(mdr_ente2$Cycle)
str(mdr_ente2)

summary(glmer(Result~as.numeric(dose_kg)+zootecnic+(1|Sow/Animal),data=mdr_ente2,family = binomial(link="logit"))->mod2)

vif(mod2)



summary(glmer(Result~as.numeric(dose_kg)+zootecnic+(1|Sow/Animal),data=mdr_ente3,family = binomial(link="logit"))->mod3)

vif(mod3)


###########
#Graficos#
###########


##Summary
mod<-summary(mod)
mod1<-summary(mod1)
mod2<-summary(mod2)
mod3<-summary(mod3)


eixo<-seq(1,8,0.1)

layout.matrix <- matrix(c(1, 2, 3, 4,5,5), byrow=T,nrow = 3, ncol = 2)

layout(mat = layout.matrix,
       heights = c(2, 2,0.7), # Heights of the 3 rows
       widths = c(2, 2)) # Widths of the two columns

par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod$coefficients[1] +mod$coefficients[2]*eixo+mod$coefficients[3]*1)))),ylim = c(0,0.07),
     ylab="Probability of MDR",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lty=1,lwd=3,type="l")
lines(eixo,(1/(1+exp(- (mod$coefficients[1] +mod$coefficients[2]*eixo+mod$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), 0.077, paste('A'), xpd=NA,cex=2)

par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod1$coefficients[1] +mod1$coefficients[2]*eixo+mod1$coefficients[3]*1)))),ylim = c(0,0.04),
     ylab="Probability of full susceptibility",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (mod1$coefficients[1] +mod1$coefficients[2]*eixo+mod1$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), .0445, paste('B'), xpd=NA,cex=2)


par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod2$coefficients[1] +mod2$coefficients[2]*eixo+mod2$coefficients[3]*1)))),ylim = c(0.2,0.7),
     ylab="Probability of MDR",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (mod2$coefficients[1] +mod2$coefficients[2]*eixo+mod2$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), 0.765, paste('C'), xpd=NA,cex=2)


par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod3$coefficients[1] +mod3$coefficients[2]*eixo+mod3$coefficients[3]*1)))),ylim = c(0,0.07),
     ylab="Probability of full susceptibility",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (mod3$coefficients[1] +mod3$coefficients[2]*eixo+mod3$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), .079, paste('D'), xpd=NA,cex=2)


par(xpd=NA)
par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=c("Nursery", "Growing/finishing"), lty=c(1,5), lwd=2,cex=1.5,
       title="Phase", text.font=4,horiz = T)
