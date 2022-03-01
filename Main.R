#################################
#Install and load packages      #
#################################

#Packages to be used
packages<-c("readxl","here","tidyverse","ggplot2","fmsb","knitr","multcompView",
            "logistf","MASS", "car", "lmm","lme4","here", "haven","tidyr","dplyr",
            "lsmeans", "mgcv","optimx","flexsurv","survminer","lmerTest","splines", 
            "coxme", "gridExtra","BRugs","coda","rjags","rgl","survival","reghelper")




# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


#MDR or full susceptible
source(here("Scripts","full_mdr.R"))

#ATM resistance
source(here("Scripts","resistance_atm.R"))

#Log mean comparisson
source(here("Scripts","medias.R"))


#MIC
source(here("Scripts","mic.R"))
