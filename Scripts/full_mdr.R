
##############
#E. coli     #
##############


mdr_coli <- read_excel(here("Data","mdr_coli_old.xlsx"))


head(mdr_coli)
mdr_coli2<-subset(mdr_coli,Antibiotic=="MDR")
mdr_coli3<-subset(mdr_coli,Antibiotic=="full suscetible")


mdr_coli2$Animal<-as.factor(mdr_coli2$Animal)
mdr_coli2$Cycle<-as.factor(mdr_coli2$Cycle)
str(mdr_coli2)

summary(glmer(Result~as.numeric(dose_kg)+zootecnic+(1|sow/Animal),data=mdr_coli2,family = binomial(link="logit"))->mod)

vif(mod)




mdr_coli3$Animal<-as.factor(mdr_coli3$Animal)
mdr_coli3$Cycle<-as.factor(mdr_coli3$Cycle)
str(mdr_coli3)


summary(glmer(Result~as.numeric(dose_kg)+zootecnic+(1|sow/Animal),data=mdr_coli3,family = binomial(link="logit"))->mod1)

vif(mod1)


##############
#Enterococcus#
##############
mdr_ente <-read_excel(here("Data","mdr_entero_old.xlsx"))


head(mdr_ente)
mdr_ente2<-subset(mdr_ente,Antibiotic=="MDR")
mdr_ente3<-subset(mdr_ente,Antibiotic=="full suscetible")


mdr_ente2$Animal<-as.factor(mdr_ente2$Animal)
mdr_ente2$Cycle<-as.factor(mdr_ente2$Cycle)
str(mdr_ente2)

summary(glmer(Result~as.numeric(dose_kg)+zootecnic+(1|sow/Animal),data=mdr_ente2,family = binomial(link="logit"))->mod2)

vif(mod2)



summary(glmer(Result~as.numeric(dose_kg)+zootecnic+(1|sow/Animal),data=mdr_ente3,family = binomial(link="logit"))->mod3)

vif(mod3)


###########
#Graphics#
###########


##Summary
mod<-summary(mod)
mod1<-summary(mod1)
mod2<-summary(mod2)
mod3<-summary(mod3)


## Confidence intervals

exp(mod$coefficients[,1]+1.96*mod$coefficients[,2]);exp(mod$coefficients[,1]-1.96*mod$coefficients[,2])

exp(mod1$coefficients[,1]+1.96*mod1$coefficients[,2]);exp(mod1$coefficients[,1]-1.96*mod1$coefficients[,2])

exp(mod2$coefficients[,1]+1.96*mod2$coefficients[,2]);exp(mod2$coefficients[,1]-1.96*mod2$coefficients[,2])

exp(mod3$coefficients[,1]+1.96*mod3$coefficients[,2]);exp(mod3$coefficients[,1]-1.96*mod3$coefficients[,2])



# Grafico

eixo<-seq(1,8,0.1)

layout.matrix <- matrix(c(1, 2, 3, 4,5,5), byrow=T,nrow = 3, ncol = 2)

layout(mat = layout.matrix,
       heights = c(2, 2,0.7), # Heights of the 3 rows
       widths = c(2, 2)) # Widths of the two columns

par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod$coefficients[1] +mod$coefficients[2]*eixo+mod$coefficients[3]*1)))),ylim = c(0.5,1),
     ylab="Probability of MDR",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lty=1,lwd=3,type="l")
lines(eixo,(1/(1+exp(- (mod$coefficients[1] +mod$coefficients[2]*eixo+mod$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), 1.055, paste('A'), xpd=NA,cex=2)

par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod1$coefficients[1] +mod1$coefficients[2]*eixo+mod1$coefficients[3]*1)))),
     ylab="Probability of full susceptibility",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (mod1$coefficients[1] +mod1$coefficients[2]*eixo+mod1$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), 1.000003, paste('B'), xpd=NA,cex=2)


par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod2$coefficients[1] +mod2$coefficients[2]*eixo+mod2$coefficients[3]*1)))),ylim = c(0.1,0.8),
     ylab="Probability of MDR",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (mod2$coefficients[1] +mod2$coefficients[2]*eixo+mod2$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), 0.9, paste('C'), xpd=NA,cex=2)


par(mar=c(5,6,2,1))
plot(eixo,(1/(1+exp(- (mod3$coefficients[1] +mod3$coefficients[2]*eixo+mod3$coefficients[3]*1)))),ylim = c(0,0.07),
     ylab="Probability of full susceptibility",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (mod3$coefficients[1] +mod3$coefficients[2]*eixo+mod3$coefficients[3]*0)))),lty=5,lwd=3)
text(min(eixo), .0045, paste('D'), xpd=NA,cex=2)


par(xpd=NA)
par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=c("Nursery", "Growing/finishing"), lty=c(1,5), lwd=2,cex=1.5,
       title="Phase", text.font=4,horiz = T)





# Plots according to the Table


eixo<-seq(1,8,0.1)

layout.matrix <- matrix(c(1, 2, 3, 4,5,5), byrow=T,nrow = 3, ncol = 2)

layout(mat = layout.matrix,
       heights = c(2, 2,0.7), # Heights of the 3 rows
       widths = c(2, 2)) # Widths of the two columns

par(mar=c(5,6,3,1))
plot(eixo,(1/(1+exp(- (-0.387 +0.26*eixo+1.46*1)))),ylim = c(0.4,1),
     ylab="Probability of MDR",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lty=1,lwd=3,type="l")
lines(eixo,(1/(1+exp(- (-0.387 +0.26*eixo+1.46*0)))),lty=5,lwd=3)
text(min(eixo), 1.09, paste('A'), xpd=NA,cex=2)

par(mar=c(5,6,3,1))
plot(eixo,(1/(1+exp(- (-3.4 +-0.3*eixo+0.33*1)))),ylim=c(0,0.04),
     ylab="Probability of full susceptibility",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (-3.4 +-0.3*eixo+0.33*0)))),lty=5,lwd=3)
text(min(eixo), 0.045, paste('B'), xpd=NA,cex=2)


par(mar=c(5,6,3,1))
plot(eixo,(1/(1+exp(- (1.3 +0.152*eixo+0.76*1)))),ylim=c(0.8,1),
     ylab="Probability of MDR",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (1.3 +0.152*eixo+0.76*0)))),lty=5,lwd=3)
text(min(eixo), 1.028, paste('C'), xpd=NA,cex=2)


par(mar=c(5,6,3,1))
plot(eixo,(1/(1+exp(- (-1.64 +-0.98*eixo+-0.11*1)))),ylim=c(0,0.07),
     ylab="Probability of full susceptibility",xlab="Expected total dose/animal (g)",cex.axis=1.5, cex.lab=1.5,lwd=3,lty=1,type="l")
lines(eixo,(1/(1+exp(- (-1.64 +-0.98*eixo+-0.11*0)))),lty=5,lwd=3)
text(min(eixo), 0.08, paste('D'), xpd=NA,cex=2)


par(xpd=NA)
par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=c("Nursery", "Growing/finishing"), lty=c(1,5), lwd=2,cex=1.5,
       title="Phase", text.font=4,horiz = T)
