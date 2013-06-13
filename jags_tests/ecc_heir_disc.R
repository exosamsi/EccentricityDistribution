#dcat
#dmmult
#ddirc
library(R2jags)
load.module("glm") #improves performance

#inputdata <- read.table("ecc_data.txt", header=T)
#inputdata <- read.table("https://raw.github.com/exosamsi/EccentricityDistribution/master/datasets/darin_hkeasy.txt", header=T)
inputdata <- read.table("darin_hkeasy.txt", header=T)
#inputdata
Ndata <- length(inputdata$HHAT)

ecc_mixture_disc <- function() {
#sigmahobs <- 0.01
#sigmakobs <- 0.01
#alpha <- 2.0
#f ~ ddirchilet(3)
sigmae ~ dunif(0.,1.)
#Ndata <- 905
#for (i in 1:5) { 
for (i in 1:Ndata) { 
 #lambda[i] ~ dgamma(alpha/2,alpha/2)
 h[i] ~ dnorm(0.,1.0/(sigmae*sigmae)) %_% T(-1,1) # *lambda[i]
 k[i] ~ dnorm(0.,1.0/(sigmae*sigmae)) %_% T(-1,1)
 hhat[i] ~ dnorm(h[i],1.0/(sigmahobs[i]*sigmahobs[i])) %_% T(-1,1)
 khat[i] ~ dnorm(k[i],1.0/(sigmakobs[i]*sigmakobs[i])) %_% T(-1,1)
 }
}
inits = NULL  # initial values

model.file = "ecc_heir_disc.txt"
write.model(ecc_mixture_disc, model.file)
file.show(model.file)

data = list(hhat=inputdata$HHAT,khat=inputdata$KHAT,sigmahobs=inputdata$SIGMAH,sigmakobs=inputdata$SIGMAK,Ndata=Ndata)
parameters.to.save = c("h", "k", "sigmae")

sim = jags(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=5000)

#print(sim)
#plot(sim)

hist(inputdata$HTRUE)
colnames(sims$BUGSoutput$sims.matrix)
hist(sim$BUGSoutput$sims.matrix[,"h[1]"], prob=T, breaks=20)
hfirstcol = 2
hlastcol = hfirstcol + length(inputdata$HHAT)-1
hist(sim$BUGSoutput$sims.matrix[,hfirstcol:hlastcol], prob=T, breaks=20)
