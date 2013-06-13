#dcat
#dmmult
#ddirc
library(R2jags)
load.module("glm") #improves performance

inputdata <- read.table("darin_hkeasy.txt", header=T)
#inputdata
Ndata <- length(inputdata$HHAT)

ecc_mixture_cont <- function() {
alpha <- 2.0
sigmae ~ dunif(0.,1.)
for (i in 1:Ndata) { 
 lambda[i] ~ dgamma(alpha/2,alpha/2)
 h[i] ~ dnorm(0.,lambda[i]/(sigmae*sigmae)) %_% T(-1,1) 
 k[i] ~ dnorm(0.,lambda[i]/(sigmae*sigmae)) %_% T(-1,1)
 hhat[i] ~ dnorm(h[i],1.0/(sigmahobs[i]*sigmahobs[i])) %_% T(-1,1)
 khat[i] ~ dnorm(k[i],1.0/(sigmakobs[i]*sigmakobs[i])) %_% T(-1,1)
 }
}
inits = NULL  # initial values

model.file = "ecc_heir_cont.txt"
write.model(ecc_mixture_cont, model.file)
file.show(model.file)

data = list(hhat=inputdata$HHAT,khat=inputdata$KHAT,sigmahobs=inputdata$SIGMAH,sigmakobs=inputdata$SIGMAK,Ndata=Ndata)
parameters.to.save = c("h", "k", "sigmae","lambda")

sim = jags(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=500)

#print(sim)
#plot(sim)

hist(inputdata$HTRUE)
colnames(sims$BUGSoutput$sims.matrix)
hist(sim$BUGSoutput$sims.matrix[,"h[1]"], prob=T, breaks=20)
hfirstcol = 2
hlastcol = hfirstcol + length(inputdata$HHAT)-1
hist(sim$BUGSoutput$sims.matrix[,hfirstcol:hlastcol], prob=T, breaks=20)
