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

# Generate a simulated data set for h & k for testing purposes
generate.ecc.mixture.disc <- function(Ndata,frac,sigmae,sigmahobs,sigmakobs) {
h = rep(0.,Ndata)
k = rep(0.,Ndata)
hhat = rep(0.,Ndata)
khat = rep(0.,Ndata)
for (i in 1:Ndata) { 
 c = sample(1:length(frac), 1, prob = frac, replace = TRUE)
 h[i] = rnorm.bound(1,0,sigmae[c],lower.bound=-1,upper.bound=1)
 k[i] = rnorm.bound(1,0,sigmae[c],lower.bound=-sqrt(1-h^2),upper.bound=sqrt(1-h^2)) 
 hhat[i] = rnorm.bound(1,h[i],sigmahobs,lower.bound=-1,upper.bound=1)
 khat[i] = rnorm.bound(1,k[i],sigmakobs,lower.bound=-sqrt(1-h^2),upper.bound=sqrt(1-h^2))
 }
c(hhat,khat)
}
