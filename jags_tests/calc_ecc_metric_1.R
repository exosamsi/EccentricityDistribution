# sum of squares
sumsq <- function(x) { sum(x^2) }

# Calculate metric for root mean squared difference between posterior and actual values for (e, h & k)
# Input hpred and kpred directly
calc.ecc.metric.from.pred.1 <- function(h,k,hpred,kpred) {
Nobj <- length(h)
Nsim <- length(hpred[,1])
e <- sqrt(h^2+k^2)
epred <- sqrt(hpred^2+kpred^2)
sse = 0.0
sshk = 0.0
for (i in 1:Nobj) {  
 sse[i] = sumsq(e[i]-epred[,i])/Nsim
 sshk[i] = (sumsq(h[i]-hpred[,i]) + sumsq(k[i]-kpred[,i]))/Nsim
}
rmssse = sqrt(sum(sse) / Nobj)
rmssshk = sqrt(sum(sshk) / Nobj)
c(rmssse,rmssshk)
}

# Calculate metric for root mean squared difference between posterior and actual values for (e, h & k)
# Find hpred and kpred from sim structure
calc.ecc.metric.1 <- function(h,k,sim) {
Nobj <- length(h)
hstr = paste("h[",1,"]",sep='') 
Nsim <- length(sim$BUGSoutput$sims.matrix[,hstr])
e <- sqrt(h^2+k^2)
sse = 0.0
sshk = 0.0
for (i in 1:Nobj) {  
 hstr = paste("h[",i,"]",sep='') 
 kstr = paste("k[",i,"]",sep='') 
 hpred <- sim$BUGSoutput$sims.matrix[,hstr]
 kpred <- sim$BUGSoutput$sims.matrix[,kstr]
 epred <- sqrt(hpred^2+kpred^2)
 sshk[i] = (sumsq(h[i]-hpred) + sumsq(k[i]-kpred))/Nsim
 sse[i] = sumsq(e[i]-epred)/Nsim
}
rmssse = sqrt(sum(sse) / Nobj)
rmssshk = sqrt(sum(sshk) / Nobj)
c(rmssse,rmssshk)
}
#calc.ecc.metric.1(htest,ktest,sim)

