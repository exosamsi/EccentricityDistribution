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

mass.radius.powerlaw.model.1 <- function() {
sigma.radius.phys ~ dunif(0.,1)
radius.const ~ dunif(0.,2.0)
mass.radius.exp ~ dunif(0.0,1.0) 
mass.prior.exp <- 0.5
min.mass <- 1.0
max.mass <- 300

for (i in 1:Ndata) { 
  mass.true[i] ~ dpar( mass.prior.exp,min.mass) %_% T(min.mass,max.mass)
  mass.obs[i] ~ dnorm(mass.true[i], 1.0/(mass.true[i]*mass.true[i]*sigma.mass[i]*sigma.mass[i])) %_% T(0,)
  radius.ref[i] <- radius.const*mass.true[i]^mass.radius.exp
  radius.true[i] ~ dnorm(radius.ref[i],1.0/(radius.ref[i]*radius.ref[i]*sigma.radius.phys*sigma.radius.phys)) %_% T(0,)
  radius.obs[i] ~ dnorm(radius.true[i],1.0/(radius.true[i]*radius.true[i]*sigma.radius[i]*sigma.radius[i])) %_% T(0,)
  }
}

model.file = "mass_radius_powerlaw_model.txt"
write.model(mass.radius.powerlaw.model.1, model.file)
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
Ndata <- 500
#   Masses 
mass.true.gen.min <- 1.0
mass.true.gen.max <- 300
mass.true.gen.alpha <- 0.25
mass.true.gen <- rpareto.bound(Ndata,mass.true.gen.min,mass.true.gen.alpha,upper.bound=mass.true.gen.max)
mass.obs = rep(0.,Ndata)
for (i in 1:Ndata) { 
 mass.obs[i] = rnorm.bound(1,mass.true.gen[i],sigma.mass.obs*mass.true.gen[i])
 }
#   Radii
mass.radius.exp <- 0.5
radius.const <- 1.0
sigma.radius.phys <- 0.3
sigma.radius.obs <- 0.1
sigma.mass.obs <- 0.3
radius.ref = rep(0.,Ndata)
radius.true = rep(0.,Ndata)
radius.obs = rep(0.,Ndata)
for (i in 1:Ndata) { 
 radius.ref[i] = radius.const*mass.true.gen[i]^mass.radius.exp
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
	radius.const = 0.5,
	mass.radius.exp = 1.0, 
	sigma.radius.phys = 0.1
	) 
	}

parameters.to.save = c("mass.true", "radius.true", "radius.const", "mass.radius.exp", "sigma.radius.phys")
sim = jags(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=1000)

print(sim)
#plot(sim)

radius.const.post <- sim$BUGSoutput$sims.matrix[,"radius.const"]
hist(radius.const.post, prob=T, breaks=20)
mass.radius.exp.post <- sim$BUGSoutput$sims.matrix[,"mass.radius.exp"]
hist(mass.radius.exp.post, prob=T, breaks=20)
sigma.radius.phys.post <- sim$BUGSoutput$sims.matrix[,"sigma.radius.phys"]
hist(sigma.radius.phys.post, prob=T, breaks=20)
