library(R2jags)
load.module("glm") #improves performance

inputdata <- read.table("../datasets/darin_hkeasy.txt", header=T)
#inputdata <- read.table("../datasets/darin_exopops.txt", header=T)
#inputdata <- read.table("../datasets/tom_hk.txt", header=T)
inputdata = subset(inputdata, inputdata$H_OBS^2+inputdata$K_OBS^2<1)
#num.data.to.keep = 500
#data.idx.min = 100
#data.idx.max = data.idx.min + num.data.to.keep
#inputdata = inputdata[data.idx.min:data.idx.max,]
#inputdata
Ndata <- length(inputdata$H_OBS)

	
ecc.disc.rayleigh <- function() {
sigmae ~ dunif(0.,1.)
for (i in 1:Ndata) { 
 h[i] ~ dnorm(0.,1.0/(sigmae*sigmae)) %_% T(-1,1) # *lambda[i]
 k[i] ~ dnorm(0.,1.0/(sigmae*sigmae)) %_% T(-sqrt(1.0-h[i]^2),sqrt(1.0-h[i]^2))
 hhat[i] ~ dnorm(h[i],1.0/(sigmahobs[i]*sigmahobs[i])) %_% T(-1,1)
 khat[i] ~ dnorm(k[i],1.0/(sigmakobs[i]*sigmakobs[i])) %_% T(-sqrt(1.0-hhat[i]^2),sqrt(1.0-hhat[i]^2))
 }
}
model.file = "ecc_heir_rayleigh.txt"
parameters.to.save = c("h", "k", "sigmae")
write.model(ecc.disc.rayleigh, model.file)

ecc.disc.rayleigh.untruncated <- function() {
sigmae ~ dunif(0.,1.)
zero[1] <- 0.
zero[2] <- 0.
prec.theta[1, 1] <- 1/(sigmae * sigmae)
prec.theta[2, 2] <- 1/(sigmae * sigmae)
prec.theta[1, 2] <- 0.0
prec.theta[2, 1] <- 0.0
for (i in 1:Ndata) { 
 theta[i, 1:2] ~ dmnorm(zero[], prec.theta[,])
 hhat[i] ~ dnorm(h[i], 1/sigmahobs[i]) %_% T(-1,1)
 khat[i] ~ dnorm(k[i], 1/sigmakobs[i]) %_% T(-sqrt(1.0-hhat[i]^2),sqrt(1.0-hhat[i]^2))
 }
}
#model.file = "ecc_heir_rayleigh_untruncated.txt"
#parameters.to.save = c("theta", "sigmae")
#write.model(ecc.disc.rayleigh.untruncated, model.file)
#file.show(model.file)


inits = NULL  # initial values
data = list(hhat=inputdata$H_OBS,khat=inputdata$K_OBS,sigmahobs=inputdata$H_SIGMA,sigmakobs=inputdata$K_SIGMA,Ndata=Ndata)
sim = jags(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=400)

print(sim)
#plot(sim)


colnames(sim$BUGSoutput$sims.matrix)
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

sigmae.post <- sim$BUGSoutput$sims.matrix[,"sigmae"]
hist(sigmae.post, prob=T, breaks=20)

calc.ecc.metric.1(inputdata$H_TRUE,inputdata$K_TRUE,hpred,kpred)
