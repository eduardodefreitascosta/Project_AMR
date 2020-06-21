

if(!require(knitr)){
  install.packages("knitr")
}
library(knitr)

if(!require(kableExtra)){
  install.packages("kableExtra")
}
library(kableExtra)



if(!require(ggplot2)){
  install.packages("ggplot2")
}
library(ggplot2)


if(!require(MASS)){
  install.packages("MASS")
}
library(MASS)

if(!require(car)){
  install.packages("car")
}
library(car)

if(!require(lmm)){
  install.packages("lmm")
}
library(lmm)


if(!require(lme4)){
  install.packages("lme4")
}
library(lme4)


if(!require(tidyr)){
  install.packages("tidyr")
}
library(tidyr)

if(!require(dplyr)){
  install.packages("dplyr")
}
library(dplyr)


if(!require(ordinal)){
  install.packages("ordinal")
}
library(ordinal)


if(!require(readxl)){
  install.packages("readxl")
}
library(readxl)



if(!require(multcompView)){
  install.packages("multcompView")
}
library(multcompView)


if(!require(lsmeans)){
  install.packages("lsmeans")
}
library(lsmeans)



ordinal$Id<-as.factor(ordinal$Id)
ordinal$outcome<-as.factor(ordinal$outcome)
colnames(ordinal)[7]<-"binario"


summary(glmer(cbind(ordinal$binario,10)~Phase*Group+(1|Id),family = binomial(link="logit"),data=ordinal)->a)


marginal_log = emmeans(a,
                   pairwise ~ Phase*Group,
                   adjust="tukey") 

marg_log<-summary(marginal_log)






fm1 <- clmm(outcome ~  Phase/Group +(1|Id), data=ordinal)

summa1<-summary(fm1)
summa1

BIC(fm1)


marginal = emmeans(fm1,
                   pairwise ~ Phase/Group,
                   adjust="tukey") 

marg1<-summary(marginal)

tab4<-kable (marg1$contrast,digits = 3, caption=" ")

