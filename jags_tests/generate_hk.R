

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

# Generate a simulated data set for h & k for testing purposes
generate.ecc.mixture.disc <- function(Ndata,frac,sigmae,sigmahobs,sigmakobs) {
h = rep(0.,Ndata)
k = rep(0.,Ndata)
hhat = rep(0.,Ndata)
khat = rep(0.,Ndata)
P = rpareto.bound(Ndata,1.0,0.1, upper.bound=365.0) 
for (i in 1:Ndata) { 
 c = sample(1:length(frac), 1, prob = frac, replace = TRUE)
 h[i] = rnorm.bound(1,0,sigmae[c],lower.bound=-1,upper.bound=1)
 k[i] = rnorm.bound(1,0,sigmae[c],lower.bound=-sqrt(1-h^2),upper.bound=sqrt(1-h^2)) 
 hhat[i] = rnorm.bound(1,h[i],sigmahobs,lower.bound=-1,upper.bound=1)
 khat[i] = rnorm.bound(1,k[i],sigmakobs,lower.bound=-sqrt(1-h^2),upper.bound=sqrt(1-h^2))
 }
list(hhat=hhat,khat=khat,htrue=h,ktrue=k,P=P)
}
# access via
# x = gener....
# x$hhat
# x[[1]]


# Generate a simulated data set for h & k for testing purposes
generate.ecc.uniform <- function(Ndata,emax,sigmahobs,sigmakobs) {
e = runif(Ndata,min=0.,max=emax)
w = runif(Ndata,min=0.,max=2*pi)
h = rep(0.,Ndata)
k = rep(0.,Ndata)
hhat = rep(0.,Ndata)
khat = rep(0.,Ndata)
P = rpareto.bound(Ndata,1.0,0.1, upper.bound=365.0) 
for (i in 1:Ndata) { 
 h[i] = e[i]*cos(w[i])
 k[i] = e[i]*sin(w[i])
 hhat[i] = rnorm.bound(1,h[i],sigmahobs,lower.bound=-1,upper.bound=1)
 khat[i] = rnorm.bound(1,k[i],sigmakobs,lower.bound=-sqrt(1-h^2),upper.bound=sqrt(1-h^2))
 }
list(hhat=hhat,khat=khat,htrue=h,ktrue=k,P=P)
}

# Generate a simulated data set for h & k for testing purposes
generate.ecc.mixture.disc.correlated <- function(Ndata,frac,sigmae,sigmahobs,sigmakobs,Pcrit=10.,sigmaecirc=0.05) {
h = rep(0.,Ndata)
k = rep(0.,Ndata)
hhat = rep(0.,Ndata)
khat = rep(0.,Ndata)
P = rpareto.bound(Ndata,1.0,0.1, upper.bound=365.0) 
for (i in 1:Ndata) { 
 if(P[i]<Pcrit) { c = 1 }
 else { c = sample(1:length(frac), 1, prob = frac, replace = TRUE) }
 h[i] = rnorm.bound(1,0,sigmae[c],lower.bound=-1,upper.bound=1)
 k[i] = rnorm.bound(1,0,sigmae[c],lower.bound=-sqrt(1-h^2),upper.bound=sqrt(1-h^2)) 
 hhat[i] = rnorm.bound(1,h[i],sigmahobs,lower.bound=-1,upper.bound=1)
 khat[i] = rnorm.bound(1,k[i],sigmakobs,lower.bound=-sqrt(1-h^2),upper.bound=sqrt(1-h^2))
 }
list(hhat=hhat,khat=khat,htrue=h,ktrue=k,P=P)
}

# Generate a simulated data set for h & k for testing purposes
generate.ecc.uniform.correlated <- function(Ndata,emax,sigmahobs,sigmakobs,Pcrit=10.,emaxcirc=0.05) {
e = runif(Ndata,min=0.,max=emax)
w = runif(Ndata,min=0.,max=2*pi)
h = rep(0.,Ndata)
k = rep(0.,Ndata)
hhat = rep(0.,Ndata)
khat = rep(0.,Ndata)
P = rpareto.bound(Ndata,1.0,0.1, upper.bound=365.0) 
for (i in 1:Ndata) { 
 if(P[i]<Pcrit) { e[i] = runif(1,min=0.,max=emaxcirc) }
 h[i] = e[i]*cos(w[i])
 k[i] = e[i]*sin(w[i])
 hhat[i] = rnorm.bound(1,h[i],sigmahobs,lower.bound=-1,upper.bound=1)
 khat[i] = rnorm.bound(1,k[i],sigmakobs,lower.bound=-sqrt(1-h^2),upper.bound=sqrt(1-h^2))
 }
list(hhat=hhat,khat=khat,htrue=h,ktrue=k,P=P)
}

