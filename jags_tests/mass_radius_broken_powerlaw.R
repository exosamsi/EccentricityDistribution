library(R2jags)
load.module("glm") #improves performance

# Use data from input file
#inputdata <- read.table("weiss_mr.txt", header=T)
#Ndata <- length(inputdata$PERIOD)
#mass.true.na <-rep(NA, Ndata)

# KOI PERIOD RADIUS_PLANET_OBS RADIUS_PLANET_SIGMA MASS_PLANET_OBS MASS_PLANET_SIGMA FLUX_OBS FLUX_SIGMA
#data = list(
#radius.obs=inputdata$RADIUS_PLANET_OBS,
#sigma.radius=inputdata$RADIUS_PLANET_SIGMA,
#mass.obs=inputdata$MASS_PLANET_OBS,
#sigma.mass=inputdata$MASS_PLANET_SIGMA,
#mass.true=mass.true.na,
#Ndata=Ndata)
## other variables that we could add to model
##flux.obs=inputdata$FLUX_OBS,
##sigma.flux=inputdata$FLUX_SIGMA,
##P=inputdata$PERIOD,


# input data/measurements: radius.obs, sigma.radius, mass.obs, sigma.mass, Ndata
# latent variables params: mass.true, radius.true,
# hyperparam:  radius.const.lo, radius.const.hi, mass.radius.exp.lo, mass.radius.exp.hi, sigma.radius.phys, mass.prior.exp
# WARNING: priors not thought through yet


mass.radius.broken.powerlaw.model.1 <- function() {
sigma.radius.phys ~ dunif(0.,1.0)
radius.const.lo ~ dunif(0.,2.0)
radius.const.hi ~ dunif(0.,2.0)  # make deterministic to enforce continuity?
mass.radius.exp.lo ~ dunif(-1.0,1.0) 
mass.radius.exp.hi ~ dunif(-1.0,1.0) 
mass.break ~ dunif(50.0,600.)
mass.prior.exp <- 0.5
min.mass <- 1.0
max.mass <- 600

for (i in 1:Ndata) { 
  mass.true[i] ~ dpar( mass.prior.exp,min.mass) %_% T(min.mass,max.mass)
  mass.obs[i] ~ dnorm(mass.true[i], 1.0/(mass.true[i]*mass.true[i]*sigma.mass[i]*sigma.mass[i])) %_% T(0,)
  radius.ref[i] <- ifelse(mass.true[i]<mass.break,radius.const.lo*mass.true[i]^mass.radius.exp.lo,radius.const.hi*mass.true[i]^mass.radius.exp.hi)
  radius.true[i] ~ dnorm(radius.ref[i],1.0/(radius.ref[i]*radius.ref[i]*sigma.radius.phys*sigma.radius.phys)) %_% T(0,)
  radius.obs[i] ~ dnorm(radius.true[i],1.0/(radius.true[i]*radius.true[i]*sigma.radius[i]*sigma.radius[i])) %_% T(0,)
  }
}

model.file = "mass_radius_broken_powerlaw_model.txt"
write.model(mass.radius.broken.powerlaw.model.1, model.file)
#file.show(model.file)

# function to draw from truncated powerlaw
rpareto.bound <- function(Ndata, scale, shape, lower.bound = scale, upper.bound = Inf)
{
	mass.true.gen = rep(0.,Ndata)
	for (i in 1:Ndata) { 
	repeat {
	  mass.true.gen[i] = scale/(1.0-runif(1))^(1.0/shape)	  
	  if( (mass.true.gen[i]>lower.bound) && (mass.true.gen[i]<upper.bound) ) {break}
      }
	}
	mass.true.gen
}

# function to draw from truncated normal
rnorm.bound <- function(Ndata, mu, prec, lower.bound = 0, upper.bound = Inf)
{
	x = rep(0.,Ndata)
	for (i in 1:Ndata) { 
	repeat {
	  x[i] = rnorm(1,mu,prec)
	  if( (x[i]>lower.bound) && (x[i]<upper.bound) ) {break}
      }
	}
	x
}

# Generate data
Ndata <- 100
#   Masses 
mass.true.gen.min <- 1.0
mass.true.gen.max <- 3000
mass.true.gen.alpha <- 0.125
mass.true.gen <- rpareto.bound(Ndata,mass.true.gen.min,mass.true.gen.alpha,upper.bound=mass.true.gen.max)
mass.obs = rep(0.,Ndata)
for (i in 1:Ndata) { 
 mass.obs[i] = rnorm.bound(1,mass.true.gen[i],sigma.mass.obs*mass.true.gen[i])
 }
#   Radii
mass.break <- 150.0
mass.radius.exp.lo <- 0.5
mass.radius.exp.hi <- 0.0
radius.const.lo <- 1.0
radius.const.hi <- 1.0
sigma.radius.phys <- 0.3
sigma.radius.obs <- 0.1
sigma.mass.obs <- 0.3
radius.ref = rep(0.,Ndata)
radius.true = rep(0.,Ndata)
radius.obs = rep(0.,Ndata)
for (i in 1:Ndata) { 
 radius.ref[i] = ifelse(mass.true.gen[i]<=mass.break,radius.const.lo*mass.true.gen[i]^mass.radius.exp.lo,radius.const.hi*mass.true.gen[i]^mass.radius.exp.hi)
 radius.true[i] = rnorm.bound(1,radius.ref[i],sigma.radius.phys*radius.ref[i])
 radius.obs[i] = rnorm.bound(1,radius.true[i],sigma.radius.obs*radius.true[i])
}
# Observational uncertainties
sigma.radius = rep(sigma.radius.obs,Ndata)
sigma.mass = rep(sigma.mass.obs,Ndata)


data = list(radius.obs=radius.obs,sigma.radius=sigma.radius,mass.obs=mass.obs, sigma.mass=sigma.mass,Ndata=Ndata)

#inits = NULL  # initial values
inits = function() {
	list(
	radius.const.lo = 0.5,
	radius.const.hi = 2.0,
	mass.radius.exp.lo = 0.75, 
	mass.radius.exp.hi = 0.75,
    mass.break = 300,
	sigma.radius.phys = 0.1
	) 
	}

parameters.to.save = c("mass.true", "radius.true", "radius.const.lo", "radius.const.hi", "mass.radius.exp.lo", "mass.radius.exp.hi", "mass.break", "sigma.radius.phys")
sim = jags(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=10000)

print(sim)
#plot(sim)

radius.const.lo.post <- sim$BUGSoutput$sims.matrix[,"radius.const.lo"]
hist(radius.const.lo.post, prob=T, breaks=20)
mass.radius.exp.lo.post <- sim$BUGSoutput$sims.matrix[,"mass.radius.exp.lo"]
hist(mass.radius.exp.lo.post, prob=T, breaks=20)
radius.const.hi.post <- sim$BUGSoutput$sims.matrix[,"radius.const.hi"]
hist(radius.const.hi.post, prob=T, breaks=20)
mass.radius.exp.hi.post <- sim$BUGSoutput$sims.matrix[,"mass.radius.exp.hi"]
hist(mass.radius.exp.hi.post, prob=T, breaks=20)
sigma.radius.phys.post <- sim$BUGSoutput$sims.matrix[,"sigma.radius.phys"]
hist(sigma.radius.phys.post, prob=T, breaks=20)
