################
#Bayesian model#
################
#cat(
'model{

  
  
    #Likelihood#
    for (i in 1:nobs){
      y[i] ~ dinterval(x[i], lim[i,])
      x[i] ~ dweib(shape, mu[i]) 
      
      log(mu[i])<-b_0+b_s*serovar[i]+b_m*material[i]+b_a*ano[i]+(b1*sero_mat1[i]+b2*sero_mat2[i]+b3*sero_mat3[i])*ano[i]
    }


  #Priors for fixed effects#
  
  
  shape ~ dgamma(1, 0.0001) #Shape parameter
  
  b_0 ~ dnorm(0.0, 0.0001) #intercept for Thyphimu+carcass
  b_s ~ dnorm(0.0, 0.0001) #intercept for Derby+carcass
  b_m ~ dnorm(0.0, 0.0001) #intercept for Thyphimu+food
  
  b_a ~ dnorm(0.0, 0.0001) #slope for Thyphimu+carcass
  b1 ~ dnorm(0.0, 0.0001)  #slope for Thyphimu+food
  b2 ~ dnorm(0.0, 0.0001)  #slope for Derby+carcass
  b3 ~ dnorm(0.0, 0.0001)  #slope for Derby+food
  
 
   #Outcomes#
   
   ## Typh+carc
   # Scale
   for(i in 1:16){
   scale1[i]<-exp(b_0+b_a*year[i])
   #scale1.1[i]<-pow(scale1[i],-1/shape)
   }
   
   
   ## Typh+food
   # scale
   for(i in 1:16){
   scale2[i]<-exp(b_0+b_m+(b_a+b1)*year[i])
   #scale2.1[i]<-pow(scale2[i],-1/shape)
   }
   
   ## Derby+carc
   # Scale
   for(i in 1:16){
   scale3[i]<-exp(b_0+b_s+(b_a+b2)*year[i])
  # scale3.1[i]<-pow(scale3[i],-1/shape)
   }
   
   ## Derby+food
    # Scale
    for(i in 1:16){
   scale4[i]<-exp(b_0+b_m+b_s+(b_a+b3)*year[i])
 # scale4.1[i]<-pow(scale4[i],-1/shape)
   }

   
  

#   }' , #file={f1<-tempfile()} )


# Defining the data as a list

#dadosjags<- list(nobs=dim(carol)[1],ngroups=12,projeto=carol$projeto,ano=carol$ano,serovar=carol$sorovar,material=carol$material,
#                 sero_mat1=carol$sero_mat1,sero_mat2=carol$sero_mat2,sero_mat3=carol$sero_mat3,
#                 y=rep(1,dim(carol)[1]),x=rep(NA,dim(carol)[1]),lim=cbind(carol$min_coli,carol$max_coli),
#                 year=seq(0,15,1))


#Output parameters
#parms = c("shape","scale1","scale2","scale3","scale4","b_0","b_s","b_m","b_a","b1","b2","b3")

# Defining burn-in, number of simulations, thin and parameters to be shown
#burn = 100;nsim = 100000;nthin = 10


# Inits function
#inits <- function() {
#  list(alpha=rnorm(12, 0, 2), shape=rnorm(1, 1, 1), mu.int=rnorm(1, 0, 1), tau.int=rlnorm(1),b_s=rnorm(1, 0, 1),
#       b_m=rnorm(1, 0, 1),b_a=rnorm(1, 0, 1),b1=rnorm(1, 0, 1),b2=rnorm(1, 0, 1),b3=rnorm(1, 0, 1))
#}



# initializing for adaptation
#m1 <- jags.model(f1, dadosjags, inits = inits, n.chains=2,n.adapt=1000)


# updating burn-in
#update(m1,burn)

# Final sampling
#mcmc1 <- coda.samples(m1, parms, n.iter=nsim,thin=nthin)


#plot(mcmc1)
#effectiveSize(mcmc1)
#autocorr.diag(mcmc1)
#summary(mcmc1)
#gelman.plot(mcmc1)


################
# Comparissons #
################
