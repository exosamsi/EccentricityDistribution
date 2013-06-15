#Code to fit discrete mixture model to eccentricity distribution
#Leslie A. Rogers, June 13 2013, SAMSI
library(R2jags)
library(gplots)
load.module("glm")  # use this to improve the glm estimation of SE's from output see post http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/e19faa17


#inputdata <- read.table("ecc_data.txt", header=T)
#inputdata <- read.table("https://raw.github.com/exosamsi/EccentricityDistribution/master/datasets/darin_hkeasy.txt", header=T)
inputdata0 <- read.table("~/Documents/Exoplanets/SAMSI/darin_hkeasy.txt", header=T)
#Remove data with hhat^2+khat^2>1
inputdata <- subset(inputdata0, inputdata0$HHAT*inputdata0$HHAT+inputdata0$KHAT*inputdata0$KHAT<=1)


Ndata <- length(inputdata$HHAT)
Nm <- 2 #Number of mixtures

eccmodel <- function() {
    
    #Population parameters
    for (j in 1:Nm) {
        e.sigma[j] ~ dunif(0, 1)
        e.phi[j] <- 1/(e.sigma[j]*e.sigma[j])
        #f[j] <- 1.0/Nm #Eventually update here
        a[j] <- 1;
    }
    #a <- rep(1,Nm)
    #a <- matrix(1,Nm,1)
    f ~ ddirch(a[])
    
    for (n in 1:Ndata){
        #True planet properties
        c[n] ~ dcat(f[]) #Need to check
        h[n] ~ dnorm(0, e.phi[c[n]]) %_% T(-1,1) #Eventually do multivariate truncated normal
        k[n] ~ dnorm(0, e.phi[c[n]]) %_% T(-sqrt(1-h[n]*h[n]),sqrt(1-h[n]*h[n]))
        
        #Observed planet properties
        hhat[n] ~ dnorm(h[n], 1.0/(hhat.sigma[n]*hhat.sigma[n])) %_% T(-1,1)
        khat[n] ~ dnorm(k[n], 1.0/(khat.sigma[n]*khat.sigma[n])) %_% T(-sqrt(1-hhat[n]*hhat[n]),sqrt(1-hhat[n]*hhat[n]))
        #Note: some of input e-distribution has hhat^2 + khat^2 >1, JAGS complains about initialization when I try to constrain khat^2+hhat^2<1
    }
    
}

# write the model code out to a file
model.file = "eccmodel.txt"
write.model(eccmodel, model.file)
#file.show(model.file)

data = list(Nm=Nm, Ndata=Ndata, hhat=inputdata$HHAT, khat=inputdata$KHAT, hhat.sigma=inputdata$SIGMAH, khat.sigma=inputdata$SIGMAK)  # include only variables in teh model

#inits = function() {
#    # based on ols
#    list(e.sigma=rnorm(Nm,0, .0000341),
#    h=runif(Ndata,0, 1), #eventually need to truncate
#    k=runif(Ndata,0, 1),
#    c= )
#}

inits = NULL

parameters.to.save = c("e.sigma", "h", "k", "c", "f") # parameters to return from the MCMC

sim = jags(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=10000)

#print(sim)
plot(sim)


#Make some plots to explore/diagnose the results
hist(inputdata$HTRUE)
colnames(sim$BUGSoutput$sims.matrix)
hist(sim$BUGSoutput$sims.matrix[,"h[1]"], prob=T, breaks=20) #Histogram of h for particular planet
hist(sim$BUGSoutput$sims.matrix[,"e.sigma[1]"], prob=T, breaks=20)
hist(sim$BUGSoutput$sims.matrix[,"e.sigma[2]"], prob=T, breaks=20)
#hist(sim$BUGSoutput$sims.matrix[,"c[2]"], prob=T, breaks=20)

hist(sim$BUGSoutput$sims.matrix[,"h[1]"]*sim$BUGSoutput$sims.matrix[,"h[1]"]+sim$BUGSoutput$sims.matrix[,"k[1]"]*sim$BUGSoutput$sims.matrix[,"k[1]"], prob=T, breaks=20) #Histogram of h for particular planet

hist(inputdata$HTRUE*inputdata$HTRUE+inputdata$KTRUE*inputdata$KTRUE)

#Look at eccentricity distribution and sort different components (would need to be generalized for Nm>2)
e.sigma.low <- pmin(sim$BUGSoutput$sims.matrix[,"e.sigma[1]"], sim$BUGSoutput$sims.matrix[,"e.sigma[2]"])
e.sigma.hi <- pmax(sim$BUGSoutput$sims.matrix[,"e.sigma[1]"], sim$BUGSoutput$sims.matrix[,"e.sigma[2]"])
hist(e.sigma.low, prob=T, breaks=20)
hist(e.sigma.hi, prob=T, breaks=20)

plot(e.sigma.low, e.sigma.hi)
#hist2d(e.sigma.low, e.sigma.hi, nbins=200, same.scale=FALSE, na.rm=TRUE, show=TRUE,col=c("black", heat.colors(12)), ... )
hist2d(e.sigma.low, e.sigma.hi, same.scale=TRUE, nbins=200)
#hist2d(sim$BUGSoutput$sims.matrix[,"e.sigma[1]"], sim$BUGSoutput$sims.matrix[,"e.sigma[2]"], same.scale=TRUE, nbins=200)
