library(ggplot2)
library(car)
library(readxl)
library(here)
library(knitr)
##############
#E. coli     #
##############


mdr_coli <- read_excel(here("Data","mdr_coli.xlsx"))

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

summary(glm(Result~as.numeric(dose_kg)+zootecnic,data=mdr_coli2,family = binomial(link="logit"))->mod)

vif(mod)

summary(glm(Result~as.numeric(dose_kg)+zootecnic,data=mdr_coli3,family = binomial(link="logit"))->mod1)

vif(mod1)


##############
#Enterococcus#
##############
mdr_ente <-read_excel(here("Data","mdr_ente.xlsx"))

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

summary(glm(Result~as.numeric(dose_kg)+zootecnic,data=mdr_ente2,family = binomial(link="logit"))->mod2)

vif(mod2)

summary(glm(Result~as.numeric(dose_kg)+zootecnic,data=mdr_ente3,family = binomial(link="logit"))->mod3)

vif(mod3)


###########
#Graficos#
###########


eixo<-seq(1,8,0.1)

layout.matrix <- matrix(c(1, 2, 3, 4,5,5), byrow=T,nrow = 3, ncol = 2)

layout(mat = layout.matrix,
       heights = c(2, 2,0.7), # Heights of the 3 rows
       widths = c(2, 2)) # Widths of the two columns

par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod$coefficients[1] +mod$coefficients[2]*eixo+mod$coefficients[3]*1)))),ylim = c(0.4,1),
     ylab="Probability of MDR",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lty=1,lwd=3,type="l")
lines(eixo,(1/(1+exp(- (mod$coefficients[1] +mod$coefficients[2]*eixo+mod$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), 1.08, paste('A'), xpd=NA,cex=2)

par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod1$coefficients[1] +mod1$coefficients[2]*eixo+mod1$coefficients[3]*1)))),ylim = c(0,0.09),
     ylab="Probability of full susceptible",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (mod1$coefficients[1] +mod1$coefficients[2]*eixo+mod1$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), .101, paste('B'), xpd=NA,cex=2)


par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod2$coefficients[1] +mod2$coefficients[2]*eixo+mod2$coefficients[3]*1)))),ylim = c(0.2,0.7),
     ylab="Probability of MDR",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (mod2$coefficients[1] +mod2$coefficients[2]*eixo+mod2$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), 0.755, paste('C'), xpd=NA,cex=2)


par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod3$coefficients[1] +mod3$coefficients[2]*eixo+mod3$coefficients[3]*1)))),ylim = c(0,0.1),
     ylab="Probability of full susceptible",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (mod3$coefficients[1] +mod3$coefficients[2]*eixo+mod3$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), .112, paste('D'), xpd=NA,cex=2)


par(xpd=NA)
par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=c("Nursery", "Growing/finishing"), lty=c(1,5), lwd=2,cex=1.5,
       title="Phase", text.font=4,horiz = T)
