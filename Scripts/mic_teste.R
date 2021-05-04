
#Packages to be used
packages<-c("readxl","here","tidyverse","ggplot2","gridExtra","knitr","BRugs","coda","rjags","rgl","flexsurv")


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# Creating the dataset

labe<-c(rep(1,200),rep(0,200)) #1=comme; 0=no_atb
mic<-NULL

for(i in 1:400){
  mic[i]<-rweibull(1,shape=0.5,scale=2+(1.5*labe[i]))
}

data<-cbind.data.frame(mic=mic,farm=labe)

#c(0.125,	0.25,	0.5,	1,	2,	4,	8,	16)

data$lim_sup<-ifelse(data$mic<0.125,0.125,ifelse(data$mic<0.25,0.25,ifelse(data$mic<0.5,0.5,ifelse(data$mic<1,1,ifelse(data$mic<2,2,ifelse(data$mic<4,4,ifelse(data$mic<8,8,
                                                                              ifelse(data$mic<16,16,16))))))))

data$lim_inf<-ifelse(data$mic<0.125,0,ifelse(data$mic<0.25,0.125,ifelse(data$mic<0.5,0.25,ifelse(data$mic<1,0.5,ifelse(data$mic<2,1,ifelse(data$mic<4,2,ifelse(data$mic<8,4,
                                                                              ifelse(data$mic<16,8,15.9))))))))
data$y<-ifelse(data$mic>16,2,1)

write.csv(data,here("Projeto_Carol","Output","MIC_teste.csv"))

#Bayesian model

cat(
  'model{

    for (i in 1:400){
      y[i] ~ dinterval(x[i], lim[i,])
      x[i] ~ dweib(r, mu[i])     
      log(mu[i])<-alpha1+(beta*farm[i])    
}

  log(r)<-alpha2
  beta ~ dnorm(0.0, 0.0001)  
  
  alpha1 ~ dnorm(0.0, 0.0001) #log_scale
  alpha2 ~ dnorm(0.0, 0.0001) #log_shape
  
  shape<-r
  scale1<-exp(-(alpha1+beta)/shape)
  scale2<-exp(-(alpha1)/shape)

##Outputs
MIC50_comm<-scale1*(-log(1-0.5))^(1/r)
MIC50_no<-scale2*(-log(1-0.5))^(1/r)

MIC90_comm<-scale1*(-log(1-0.9))^(1/r)
MIC90_no<-scale2*(-log(1-0.9))^(1/r)


  }' , file={f1<-tempfile()} )



# Defining burn-in, number of simulations, thin and parameters to be shown
burn = 100;nsim = 30000;nthin = 10

# Defining the data as a list



dadosjags<- list(y=data$y,x=rep(NA,400),lim=cbind(data$lim_inf,data$lim_sup),farm=data$farm)

parms = c("alpha1","beta","r","scale1","scale2","MIC50_comm","MIC50_no","MIC90_comm","MIC90_no")

# initializing for adoptation
m1 <- jags.model(f1, dadosjags,  n.chains=3,n.adapt=1000)


# updating burn-in
update(m1,burn)

# Final sampling
mcmc1 <- coda.samples(m1, parms, n.iter=nsim,thin=nthin)

plot(mcmc1)
effectiveSize(mcmc1)
autocorr.diag(mcmc1)
summary(mcmc1)
gelman.plot(mcmc1)


################
# Comparissons #
################

col_name<-c("Mean_MIC_comm","Mean_MIC_no", "MIC50_comm","MIC90_comm","MIC50_no","MIC90_no")
row_name<-c("True","Bayesian","Crude")

#True values
table<-rbind.data.frame(
c(
mean(data$mic[data$farm==1]),
mean(data$mic[data$farm==0]),

quantile(data$mic[data$farm==1],c(0.5,0.9)),
quantile(data$mic[data$farm==0],c(0.5,0.9))
),

#Bayesian model

c(
resul$statistics[8]*gamma(1+1/resul$statistics[7]),
resul$statistics[9]*gamma(1+1/resul$statistics[7]),
resul$statistics[1],
resul$statistics[3],
resul$statistics[2],
resul$statistics[4]
),

#Crude
c(
mean(data$lim_sup[data$farm==1 & data$y==1]),
mean(data$lim_sup[data$farm==0 & data$y==1]),
2,
16,
1,
8
)
)

transform(table(data$lim_sup[data$farm==1 & data$y==1]), cumFreq = cumsum(Freq), relative = prop.table(Freq))
transform(table(data$lim_sup[data$farm==0 & data$y==1]), cumFreq = cumsum(Freq), relative = prop.table(Freq))

rownames(table)<-row_name
colnames(table)<-col_name
