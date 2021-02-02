
#####################
#Analisys Log10cfu/g#
#####################

#Import data
carol <- read_excel(here("Data","enumeration.xlsx"), 
                           col_types = c("text", "text", "numeric", 
                                                   "text", "text", "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric"))

#Compare means per group

summary(aov(logcolisow~group, data=carol))


summary(aov(logenterosow~group, data=carol))



#Histograms for E. coli
par(mfrow=c(2,2))
for (i in 1:4){
hist(carol$logcoli[carol$group==paste("G",i,sep='')],
xlab=expression(paste("log(cfu/g) ", italic("E. coli"), " in faeces",sep=" ")), 
main=paste("G",i,sep='')
     
     )
}


#Histograms for Enterococcus
par(mfrow=c(2,2))
for (i in 1:4){
  hist(carol$logentero[carol$group==paste("G",i,sep='')],
       xlab=expression(paste("log(cfu/g) enterobacteria in faeces",sep=" ")), 
       main=paste("G",i,sep='')
       
  )
}


#Summary table
data.resume <- carol %>% 
  group_by(group, time) %>% 
  summarise(logcoli.m = mean(logcoli,na.rm=T), 
            logentero.m=mean(logentero,na.rm=T),
            dp1=sd(logcoli,na.rm=T),n1 = n(),
            dp2=sd(logentero,na.rm=T),n2 = n() ) %>%
  mutate(ep1 = dp1/sqrt(n1),ep2 = dp2/sqrt(n2) )
  


#Summary data
kable(data.resume)
kable(matrix(unlist((data.resume[,3])),nrow=4,ncol=4,byrow=T))
kable(matrix(unlist(data.resume[,4]),nrow=4,ncol=4,byrow=T))



# Time point E. coli
p <- ggplot(data = data.resume,
            mapping = aes(x = as.numeric(time),
                          y = logcoli.m,
                          colour = group)) +
  geom_errorbar(aes(ymin = logcoli.m - ep1,
                    ymax = logcoli.m + ep1),
                width = .1,
                position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  geom_line(position = position_dodge(0.1)) +
  labs(x = "Sample",
       y = expression(paste("Mean log(cfu/g) ", italic("E. coli"), " in faeces")),
       colour = "Group")
p + theme_bw() + theme(legend.position = "bottom")


#Time point Enterococcus
p1 <- ggplot(data = data.resume,
            mapping = aes(x = as.numeric(time),
                          y = logentero.m,
                          colour = group)) +
  geom_errorbar(aes(ymin = logentero.m - ep2,
                    ymax = logentero.m + ep2),
                width = .1,
                position = position_dodge(0.1)) +
  geom_point(position = position_dodge(0.1)) +
  geom_line(position = position_dodge(0.1)) +
  labs(x = "Sample",
       y = "Mean log(cfu/g) enterobacteria in faeces",
       colour = "Group")
p1 + theme_bw() + theme(legend.position = "bottom")



#MIxed model for E. coli
mod<- lmer(logcoli~time*group+logcolisow+(1|sow/Animal),REML=TRUE,data=carol,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
           optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),na.action = na.omit )


summary(mod)
Anova(mod)
lsmeans(mod, pairwise ~ time|group,adjust="tukey")
lsmeans(mod, pairwise ~ group|time,adjust="tukey")


plot(mod)
qqnorm(residuals(mod))
hist(residuals(mod))
acf(residuals(mod))
vif(mod)

#Mixed model for Enterococcus
mod1<- lmer(logentero~time*group+logenterosow+(1|sow/Animal),data=carol)
summary(mod1)
Anova(mod1)
lsmeans(mod1, pairwise ~ time|group,adjust="tukey")
lsmeans(mod1, pairwise ~ group|time,adjust="tukey")

plot(mod1)
qqnorm(residuals(mod1))
hist(residuals(mod1))
acf(residuals(mod1))
vif(mod1)