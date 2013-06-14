library(R2jags)
load.module("glm") #improves performance

inputdata <- read.table("darin_hkeasy.txt", header=T)
#inputdata <- read.table("darin_exopops.txt", header=T)
#inputdata <- read.table("tom_hk.txt", header=T)
#inputdata
Ndata <- length(inputdata$H_OBS)

ecc_disc_rayleigh <- function() {
sigmae ~ dunif(0.,1.)
for (i in 1:Ndata) { 
 h[i] ~ dnorm(0.,1.0/(sigmae*sigmae)) %_% T(-1,1) # *lambda[i]
 k[i] ~ dnorm(0.,1.0/(sigmae*sigmae)) %_% T(-1,1)
 hhat[i] ~ dnorm(h[i],1.0/(sigmahobs[i]*sigmahobs[i])) %_% T(-1,1)
 khat[i] ~ dnorm(k[i],1.0/(sigmakobs[i]*sigmakobs[i])) %_% T(-1,1)
 }
}
model.file = "ecc_heir_rayleigh.txt"
write.model(ecc_disc_rayleigh, model.file)
#file.show(model.file)

Ncomp <- 3
# doesn't work yet
ecc_mixture_disc_three <- function() {
for (cc in 1:Ncomp) { 
 sigmae[cc] ~ dunif(0.,1.)
}
f[1:3] ~ ddirch(3) #don't work yet
for (i in 1:Ndata) {  
 c[i] ~ dcat(f) #don't work yet
 h[i] ~ dnorm(0.,1.0/(sigmae[c]*sigmae[c])) %_% T(-1,1) 
 k[i] ~ dnorm(0.,1.0/(sigmae[c]*sigmae[c])) %_% T(-1,1)
 hhat[i] ~ dnorm(h[i],1.0/(sigmahobs[i]*sigmahobs[i])) %_% T(-1,1)
 khat[i] ~ dnorm(k[i],1.0/(sigmakobs[i]*sigmakobs[i])) %_% T(-1,1)
 }
}
model.file = "ecc_heir_disc.txt"
write.model(ecc_mixture_disc_three, model.file)
#file.show(model.file)

inits = NULL  # initial values

data = list(hhat=inputdata$H_OBS,khat=inputdata$K_OBS,sigmahobs=inputdata$H_SIGMA,sigmakobs=inputdata$K_SIGMA,Ndata=Ndata)
#data = list(hhat=inputdata$H_OBS,khat=inputdata$K_OBS,sigmahobs=inputdata$H_SIGMA,sigmakobs=inputdata$K_SIGMA,Ndata=Ndata,Ncomp=Ncomp)
parameters.to.save = c("h", "k", "sigmae")

sim = jags(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=100)

#print(sim)
#plot(sim)

colnames(sim$BUGSoutput$sims.matrix)
hist(sim$BUGSoutput$sims.matrix[,"h[1]"], prob=T, breaks=20)
hfirstcol = 2
hlastcol = hfirstcol + length(inputdata$H_OBS)-1
hpred <- sim$BUGSoutput$sims.matrix[,hfirstcol:hlastcol]
hist(hpred, prob=T, breaks=20)
hist(inputdata$H_TRUE, breaks=20)
kfirstcol = hlastcol + 1
klastcol = kfirstcol + length(inputdata$K_OBS)-1
kpred <- sim$BUGSoutput$sims.matrix[,kfirstcol:klastcol]
hist(kpred, prob=T, breaks=20)
hist(inputdata$K_TRUE, breaks=20)

epred <- sqrt(hpred^2+kpred^2)
hist(epred, prob=T, breaks=20)
hist(sqrt(inputdata$H_TRUE^2+inputdata$K_TRUE^2), breaks=20)

sigmae_post <- sim$BUGSoutput$sims.matrix[,"sigmae"]
hist(sigmae_post, prob=T, breaks=20)

