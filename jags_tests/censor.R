generate.data <- function(Ndata)
{
r.min.det <- 1.0
r.sigma <- 0.3
r.prec <- 1.0/r.sigma^2
r.obs = rep(0.,Ndata)
r.true = runif(Ndata,0.,2.)
r.obs = rnorm(Ndata,r.true,0.1)
is.det = ifelse(r.obs>r.min.det,1,0)
for (i in 1:Ndata) { 
 if(r.obs[i]<r.min.det) { r.obs[i] = NA }
 }
list(r.obs=r.obs,is.det=is.det,Ndata=Ndata)
}

ignore.censoring <- function()
{
r.min.det ~ dunif(0.0,2.0)
r.sigma <- 0.3
r.prec <- 1.0/r.sigma^2
for (i in 1:Ndata) { 
 r.true[i] ~ dunif(0.0,2.0)
 r.obs[i] ~ dnorm(r.true[i],r.prec)
 }
}

censored_broken <- function()  # doesn't compile due to NA
{
r.min.det ~ dunif(0.0,2.0)
r.sigma <- 0.3
r.prec <- 1.0/r->sigma^2
for (i in 1:Ndata) { 
 r.true[i] ~ dunif(0.0,2.0)
 is.det[i] ~ dinterval(r.true[i],r.min.det)
 r.obs.if.det[i] ~ dnorm(r.true[i],r.prec)
 r.obs[i] ~ ifelse(is.det[i],r.obs.if.det[i],NA)
 }
}

censored <- function()  
{
r.min.det ~ dunif(0.0,2.0)
r.sigma <- 0.3
r.prec <- 1.0/r.sigma^2
for (i in 1:Ndata) { 
 r.true[i] ~ dunif(0.0,2.0)
 r.obs[i] ~ dnorm(r.true[i],r.prec)
 is.det[i] ~ dinterval(r.obs[i],r.min.det)
 }
}

inits = NULL  # initial values
#parameters.to.save = c("r.true", "is.det","r.obs","r.min.det")
parameters.to.save = c("r.min.det")
testdata = generate.data(100)

model.file = "uncensored.txt"
write.model(ignore.censoring, model.file)
sim = jags(list(r.obs=testdata$r.obs, Ndata=testdata$Ndata), inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=1000)
print(sim)

model.file = "censor.txt"
write.model(censored, model.file)
sim = jags(testdata, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=1000)
print(sim)

