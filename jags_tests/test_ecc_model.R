source("calc_ecc_metric_1.R")

test.ecc.model.w.disc.mixture <- function(model.file, Ndata, ftrue, sigmaetrue, sigmahobs, sigmakobs, data.to.add = list(), inits=NULL, n.iter=1000)
{
hktest <- generate.ecc.mixture.disc(Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs)
data.base <- list(hhat=hktest$hhat, khat=hktest$khat, sigmahobs=rep(sigmahobs,Ndata), sigmakobs=rep(sigmahobs,Ndata),Ndata=Ndata )
data = c(data.base, data.to.add)
parameters.to.save = c("h", "k")
sim = jags(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=n.iter)
calc.ecc.metric.1(hktest$hhat,hktest$khat,sim)
}

# All code below is outdated, since now we want to analyze data from data files, rather than generating each time
# But I left it for now, just in case it might be useful for someone to look at for some reason

test.ecc.model.1 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 500
sigmahobs <- 0.001
sigmakobs <- 0.001
ftrue <- c(1.0)
sigmaetrue <- c(0.1)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.2 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 500
sigmahobs <- 0.001
sigmakobs <- 0.001
ftrue <- c(0.7,0.3)
sigmaetrue <- c(0.02,0.3)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.3 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 500
sigmahobs <- 0.001
sigmakobs <- 0.001
ftrue <- c(0.6,0.3,0.1)
sigmaetrue <- c(0.02,0.1,0.5)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.4 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 80
sigmahobs <- 0.001
sigmakobs <- 0.001
ftrue <- c(1.0)
sigmaetrue <- c(0.1)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.5 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 80
sigmahobs <- 0.001
sigmakobs <- 0.001
ftrue <- c(0.7,0.3)
sigmaetrue <- c(0.02,0.3)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.6 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 80
sigmahobs <- 0.001
sigmakobs <- 0.001
ftrue <- c(0.6,0.3,0.1)
sigmaetrue <- c(0.02,0.1,0.5)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.7 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 80
sigmahobs <- 0.04
sigmakobs <- 0.08
ftrue <- c(1.0)
sigmaetrue <- c(0.1)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.8 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 80
sigmahobs <- 0.04
sigmakobs <- 0.08
ftrue <- c(0.7,0.3)
sigmaetrue <- c(0.02,0.3)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.9 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 80
sigmahobs <- 0.04
sigmakobs <- 0.08
ftrue <- c(0.6,0.3,0.1)
sigmaetrue <- c(0.02,0.1,0.5)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.10 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 90
sigmahobs <- 0.25
sigmakobs <- 0.1
ftrue <- c(1.0)
sigmaetrue <- c(0.1)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.11 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 90
sigmahobs <- 0.25
sigmakobs <- 0.1
ftrue <- c(0.7,0.3)
sigmaetrue <- c(0.02,0.3)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

test.ecc.model.12 <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000)
{
Ndata <- 90
sigmahobs <- 0.25
sigmakobs <- 0.1
ftrue <- c(0.6,0.3,0.1)
sigmaetrue <- c(0.02,0.1,0.5)
test.ecc.model.w.disc.mixture(model.file,Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs, data.to.add=data.to.add, inits=inits, n.iter=n.iter)
}

run_tests_old <- function(model.file, data.to.add = list(), inits=NULL, n.iter=1000) {
Niter <- 500
# Test model 1
metric1 <- matrix(data=NA,nrow=15,ncol=2)
print("Model 1, Test 1")
metric1[1,] = test.ecc.model.1("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric[1,,])
print("Model 1, Test 2")
metric1[2,] = test.ecc.model.2("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[1,])
print("Model 1, Test 3")
metric1[3,] = test.ecc.model.3("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[3,])
print("Model 1, Test 4")
metric1[4,] = test.ecc.model.4("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[4,])
print("Model 1, Test 5")
metric1[5,] = test.ecc.model.5("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[5,])
print("Model 1, Test 6")
metric1[6,] = test.ecc.model.6("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[6,])
print("Model 1, Test 7")
metric1[7,] = test.ecc.model.7("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[7,])
print("Model 1, Test 8")
metric1[8,] = test.ecc.model.8("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[8,])
print("Model 1, Test 9")
metric1[9,] = test.ecc.model.9("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[9,])
print("Model 1, Test 10")
metric1[10,] = test.ecc.model.10("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[10,])
print("Model 1, Test 11")
metric1[11,] = test.ecc.model.11("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[11,])
print("Model 1, Test 12")
metric1[12,] = test.ecc.model.12("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[12,])
print(metric1)

print("Model 2, Test 1")
metric2 <- matrix(data=NA,nrow=15,ncol=2)
metric2[1,] = test.ecc.model.1("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[1,])
print("Model 2, Test 2")
metric2[2,] = test.ecc.model.2("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[2,])
print("Model 2, Test 3")
metric2[3,] = test.ecc.model.3("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[3,])
print("Model 2, Test 4")
metric2[4,] = test.ecc.model.4("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[4,])
print("Model 2, Test 5")
metric2[5,] = test.ecc.model.5("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[5,])
print("Model 2, Test 6")
metric2[6,] = test.ecc.model.6("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[6,])
print("Model 2, Test 7")
metric2[7,] = test.ecc.model.7("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[7,])
print("Model 2, Test 8")
metric2[8,] = test.ecc.model.8("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[8,])
print("Model 2, Test 9")
metric2[9,] = test.ecc.model.9("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[9,])
print("Model 2, Test 10")
metric2[10,] = test.ecc.model.10("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[10,])
print("Model 2, Test 11")
metric2[11,] = test.ecc.model.11("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[11,])
print("Model 2, Test 12")
metric2[12,] = test.ecc.model.12("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[12,])
print(metric2)

print("Model 3, Test 1")
metric3 <- matrix(data=NA,nrow=15,ncol=2)
metric3[1,] = test.ecc.model.1("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[1,])
print("Model 3, Test 2")
metric3[2,] = test.ecc.model.2("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[2,])
print("Model 3, Test 3")
metric3[3,] = test.ecc.model.3("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[3,])
print("Model 3, Test 4")
metric3[4,] = test.ecc.model.4("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[4,])
print("Model 3, Test 5")
metric3[5,] = test.ecc.model.5("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[5,])
print("Model 3, Test 6")
metric3[6,] = test.ecc.model.6("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[6,])
print("Model 3, Test 7")
metric3[7,] = test.ecc.model.7("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[7,])
print("Model 3, Test 8")
metric3[8,] = test.ecc.model.8("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[8,])
print("Model 3, Test 9")
metric3[9,] = test.ecc.model.9("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[9,])
print("Model 3, Test 10")
metric3[10,] = test.ecc.model.10("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[10,])
print("Model 3, Test 11")
metric3[11,] = test.ecc.model.11("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[11,])
print("Model 3, Test 12")
metric3[12,] = test.ecc.model.12("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[12,])
print(metric3)

print("Model 4, Test 1")
metric4 <- matrix(data=NA,nrow=15,ncol=2)
metric4[1,] = test.ecc.model.1("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[1,])
print("Model 4, Test 2")
metric4[2,] = test.ecc.model.2("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[2,])
print("Model 4, Test 3")
metric4[3,] = test.ecc.model.3("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[3,])
print("Model 4, Test 4")
metric4[4,] = test.ecc.model.4("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[4,])
print("Model 4, Test 5")
metric4[5,] = test.ecc.model.5("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[5,])
print("Model 4, Test 6")
metric4[6,] = test.ecc.model.6("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[6,])
print("Model 4, Test 7")
metric4[7,] = test.ecc.model.7("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[7,])
print("Model 4, Test 8")
metric4[8,] = test.ecc.model.8("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[8,])
print("Model 4, Test 9")
metric4[9,] = test.ecc.model.9("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[9,])
print("Model 4, Test 10")
metric4[10,] = test.ecc.model.10("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[10,])
print("Model 4, Test 11")
metric4[11,] = test.ecc.model.11("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[11,])
print("Model 4, Test 12")
metric4[12,] = test.ecc.model.12("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[12,])
print(metric4)

print("Testing Complete!")
print(metric1)
print(metric2)
print(metric3)
print(metric4)








print("Model 1, Test 10")
metric1[10,] = test.ecc.model.10("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[10,])
print("Model 1, Test 11")
metric1[11,] = test.ecc.model.11("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[11,])
print("Model 1, Test 12")
metric1[12,] = test.ecc.model.12("ecc_heir_rayleigh.txt",n.iter=Niter)
print(metric1[12,])
print("Model 2, Test 10")
metric2[10,] = test.ecc.model.10("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[10,])
print("Model 2, Test 11")
metric2[11,] = test.ecc.model.11("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[11,])
print("Model 2, Test 12")
metric2[12,] = test.ecc.model.12("ecc_heir_disc.txt",data.to.add=list(Ncomp=2),n.iter=Niter)
print(metric2[12,])
print(metric2)
print("Model 3, Test 10")
metric3[10,] = test.ecc.model.10("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[10,])
print("Model 3, Test 11")
metric3[11,] = test.ecc.model.11("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[11,])
print("Model 3, Test 12")
metric3[12,] = test.ecc.model.12("ecc_heir_disc.txt",data.to.add=list(Ncomp=3),n.iter=Niter)
print(metric3[12,])
print(metric3)
print("Model 4, Test 10")
metric4[10,] = test.ecc.model.10("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[10,])
print("Model 4, Test 11")
metric4[11,] = test.ecc.model.11("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[11,])
print("Model 4, Test 12")
metric4[12,] = test.ecc.model.12("ecc_heir_cont.txt",n.iter=Niter)
print(metric4[12,])
print(metric4)
print("Testing Complete!")
print(metric1)
print(metric2)
print(metric3)
print(metric4)
}

