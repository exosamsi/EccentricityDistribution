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

ecc.mixture.disc <- function() {
for (cc in 1:Ncomp) { 
 sigmae[cc] ~ dunif(0.,1.)
 alpha[cc] <- 1.0
 }
f[1:Ncomp] ~ ddirch(alpha) 
for (i in 1:Ndata) {  
 c[i] ~ dcat(f) 
 h[i] ~ dnorm(0.,1.0/(sigmae[c[i]]*sigmae[c[i]])) %_% T(-1,1) 
 k[i] ~ dnorm(0.,1.0/(sigmae[c[i]]*sigmae[c[i]])) %_% T(-sqrt(1.0-h[i]^2),sqrt(1.0-h[i]^2))
 hhat[i] ~ dnorm(h[i],1.0/(sigmahobs[i]*sigmahobs[i])) %_% T(-1,1)
 khat[i] ~ dnorm(k[i],1.0/(sigmakobs[i]*sigmakobs[i])) %_% T(-sqrt(1.0-hhat[i]^2),sqrt(1.0-hhat[i]^2))
 }
}
model.file = "ecc_heir_disc.txt"
write.model(ecc.mixture.disc, model.file)
#file.show(model.file)

inits = NULL  # initial values
parameters.to.save = c("h", "k", "sigmae", "f")

Ncomp <- 2
data = list(hhat=inputdata$H_OBS,khat=inputdata$K_OBS,sigmahobs=inputdata$H_SIGMA,sigmakobs=inputdata$K_SIGMA,Ndata=Ndata,Ncomp=Ncomp)
sim = jags(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=1000)
calc.metric.1(inputdata$H_TRUE,inputdata$K_TRUE,hpred,kpred)

Ncomp <- 2
hktest <- generate.ecc.mixture.disc(Ndata,c(0.3,0.7),c(0.3,0.05),0.05,0.2)
htest <- hktest[1:Ndata]
ktest <- hktest[(Ndata+1):(2*Ndata)]
data = list(hhat=htest,khat=ktest,sigmahobs=rep(0.05,Ndata),sigmakobs=rep(0.2,Ndata),Ndata=Ndata,Ncomp=Ncomp)
sim = jags(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=1000)
calc.metric.1(htest,ktest,hpred,kpred)

print(sim)
#plot(sim)


colnames(sim$BUGSoutput$sims.matrix)
ffirstcol = 2
flastcol = 2 + Ncomp -1
fpred <- sim$BUGSoutput$sims.matrix[,ffirstcol:flastcol]
hfirstcol = 2 + Ncomp
hlastcol = hfirstcol + length(inputdata$H_OBS)-1
hpred <- sim$BUGSoutput$sims.matrix[,hfirstcol:hlastcol]
hist(hpred, prob=T, breaks=20)
hist(inputdata$H_TRUE, breaks=20)
kfirstcol = hlastcol + 1
klastcol = kfirstcol + length(inputdata$K_OBS)-1
kpred <- sim$BUGSoutput$sims.matrix[,kfirstcol:klastcol]
hist(kpred, prob=T, breaks=20)
hist(inputdata$K_TRUE, breaks=20)
sigmaefirstcol = klastcol + 1
sigmaelastcol = klastcol + Ncomp -1
sigmae.post <- sim$BUGSoutput$sims.matrix[,sigmaefirstcol:sigmaelastcol]
hist(sigmae.post, prob=T, breaks=20)

epred <- sqrt(hpred^2+kpred^2)
hist(epred, prob=T, breaks=40)
hist(sqrt(inputdata$H_TRUE^2+inputdata$K_TRUE^2), breaks=20)


plot(sim$BUGSoutput$sims.matrix[,"sigmae[1]"],sim$BUGSoutput$sims.matrix[,"sigmae[2]"])
sigmaelo <- sim$BUGSoutput$sims.matrix[,"sigmae[1]"]
sigmaehi <- sim$BUGSoutput$sims.matrix[,"sigmae[2]"]
for(i in 1:length(sigmaelo)) {
 if(sigmaelo[i]>sigmaehi[i]) {
  tmp = sigmaelo[i]
  sigmaelo[i] = sigmaehi[i]
  sigmaehi[i] = tmp
  }
 }
plot(sigmaelo,sigmaehi)
#hist2d(sigmaelo, sigmaehi, same.scale=TRUE, nbins=200)

calc.ecc.metric.1(inputdata$H_TRUE,inputdata$K_TRUE,hpred,kpred)
