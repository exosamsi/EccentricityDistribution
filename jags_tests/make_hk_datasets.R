source("generate_hk.R")

make.ecc.datafile.w.disc.mixture <- function(Ndata, ftrue, sigmaetrue, sigmahobs, sigmakobs, data.to.add = list(), modelname="1", dataname="R", Nrealizations=1, inits=NULL)
{
 for(i in 1:Nrealizations) {
  hktest <- generate.ecc.mixture.disc(Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs)
  data.base <- list(H_OBS=hktest$hhat, K_OBS=hktest$khat, H_SIGMA=rep(sigmahobs,Ndata), K_SIGMA=rep(sigmahobs,Ndata), H_TRUE=hktest$htrue, K_TRUE=hktest$ktrue, P=hktest$P )
  filename <- paste("../datasets/hk_",modelname,"_",dataname,"_",i,".dat",sep='') 
  write.table( data.base , file=filename, sep=' ',row.names=FALSE)
  }
}

make.ecc.datafile.w.disc.mixture.correlated <- function(Ndata, ftrue, sigmaetrue, sigmahobs, sigmakobs, data.to.add = list(), modelname="1", dataname="RC", Nrealizations=1, inits=NULL)
{
 for(i in 1:Nrealizations) {
  hktest <- generate.ecc.mixture.disc.correlated(Ndata,ftrue,sigmaetrue,sigmahobs,sigmakobs)
  data.base <- list(H_OBS=hktest$hhat, K_OBS=hktest$khat, H_SIGMA=rep(sigmahobs,Ndata), K_SIGMA=rep(sigmahobs,Ndata), H_TRUE=hktest$htrue, K_TRUE=hktest$ktrue, P=hktest$P )
  filename <- paste("../datasets/hk_",modelname,"_",dataname,"_",i,".dat",sep='') 
  write.table( data.base , file=filename, sep=' ',row.names=FALSE)
  }
}

make.ecc.datafile.w.uniform <- function(Ndata, emax, sigmahobs, sigmakobs, data.to.add = list(), modelname="1", dataname="U", Nrealizations=1, inits=NULL)
{
 for(i in 1:Nrealizations) {
  hktest <- generate.ecc.uniform(Ndata,emax,sigmahobs,sigmakobs)
  data.base <- list(H_OBS=hktest$hhat, K_OBS=hktest$khat, H_SIGMA=rep(sigmahobs,Ndata), K_SIGMA=rep(sigmahobs,Ndata), H_TRUE=hktest$htrue, K_TRUE=hktest$ktrue, P=hktest$P )
  filename <- paste("../datasets/hk_",modelname,"_",dataname,"_",i,".dat",sep='') 
  write.table( data.base , file=filename, sep=' ',row.names=FALSE)
  }
}

make.ecc.datafile.w.uniform.correlated <- function(Ndata, emax, sigmahobs, sigmakobs, data.to.add = list(), modelname="1", dataname="UC", Nrealizations=1, inits=NULL)
{
 for(i in 1:Nrealizations) {
  hktest <- generate.ecc.uniform.correlated(Ndata,emax,sigmahobs,sigmakobs)
  data.base <- list(H_OBS=hktest$hhat, K_OBS=hktest$khat, H_SIGMA=rep(sigmahobs,Ndata), K_SIGMA=rep(sigmahobs,Ndata), H_TRUE=hktest$htrue, K_TRUE=hktest$ktrue, P=hktest$P )
  filename <- paste("../datasets/hk_",modelname,"_",dataname,"_",i,".dat",sep='') 
  write.table( data.base , file=filename, sep=' ',row.names=FALSE)
  }
}

make.datasets <- function() {
 set.seed(42)
 make.ecc.datafile.w.disc.mixture(500,(1.), (0.3), 0.001, 0.001, modelname="R1", dataname="best", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(500,c(0.7,0.3), c(0.05,0.3), 0.001, 0.001, data.to.add=list(Ncomp=2), modelname="R2", dataname="best", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(500,c(0.6,0.3,0.1), c(0.05,0.2,0.5), 0.001, 0.001, data.to.add=list(Ncomp=3), modelname="R3", dataname="best", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(500,c(0.6,0.3,0.1), c(0.05,0.2,0.5), 0.001, 0.001, modelname="RC", dataname="best", Nrealizations=20)
 make.ecc.datafile.w.uniform(500,0.6, 0.001, 0.001, modelname="U6", dataname="best", Nrealizations=20)
 make.ecc.datafile.w.uniform.correlated(500,0.6, 0.001, 0.001, modelname="U6C", dataname="best", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture.correlated(500,c(0.5,0.5), c(0.05,0.3), 0.001, 0.001, data.to.add=list(Ncomp=2), modelname="R2C", dataname="best", Nrealizations=20)
#
 make.ecc.datafile.w.disc.mixture(50,(1.), (0.3), 0.001, 0.001, modelname="R1", dataname="better", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(50,c(0.7,0.3), c(0.05,0.3), 0.001, 0.001, data.to.add=list(Ncomp=2), modelname="R2", dataname="better", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(50,c(0.6,0.3,0.1), c(0.05,0.2,0.5), 0.001, 0.001, data.to.add=list(Ncomp=3), modelname="R3", dataname="better", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(50,c(0.6,0.3,0.1), c(0.05,0.2,0.5), 0.001, 0.001, modelname="RC", dataname="better", Nrealizations=20)
 make.ecc.datafile.w.uniform(50,0.6, 0.001, 0.001, modelname="U6", dataname="better", Nrealizations=20)
 make.ecc.datafile.w.uniform.correlated(50,0.6, 0.001, 0.001, modelname="U6C", dataname="better", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture.correlated(50,c(0.5,0.5), c(0.05,0.3), 0.001, 0.001, data.to.add=list(Ncomp=2), modelname="R2C", dataname="better", Nrealizations=20)
#
 make.ecc.datafile.w.disc.mixture(60,(1.), (0.3), 0.04, 0.08, modelname="R1", dataname="occ", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(60,c(0.7,0.3), c(0.05,0.3), 0.04, 0.08, data.to.add=list(Ncomp=2), modelname="R2", dataname="occ", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(60,c(0.6,0.3,0.1), c(0.05,0.2,0.5), 0.04, 0.08, data.to.add=list(Ncomp=3), modelname="R3", dataname="occ", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(60,c(0.6,0.3,0.1), c(0.05,0.2,0.5), 0.04, 0.08, modelname="RC", dataname="occ", Nrealizations=20)
 make.ecc.datafile.w.uniform(60,0.6, 0.04, 0.08, modelname="U6", dataname="occ", Nrealizations=20)
 make.ecc.datafile.w.uniform.correlated(60,0.6, 0.04, 0.08, modelname="U6C", dataname="occ", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture.correlated(60,c(0.5,0.5), c(0.05,0.3), 0.04, 0.08, data.to.add=list(Ncomp=2), modelname="R2C", dataname="occ", Nrealizations=20)
#
 make.ecc.datafile.w.disc.mixture(90,(1.), (0.3), 0.25, 0.12, modelname="R1", dataname="ast", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(90,c(0.7,0.3), c(0.05,0.3), 0.25, 0.12, data.to.add=list(Ncomp=2), modelname="R2", dataname="ast", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(90,c(0.6,0.3,0.1), c(0.05,0.2,0.5), 0.25, 0.12, data.to.add=list(Ncomp=3), modelname="R3", dataname="ast", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture(90,c(0.6,0.3,0.1), c(0.05,0.2,0.5), 0.25, 0.12, modelname="RC", dataname="ast", Nrealizations=20)
 make.ecc.datafile.w.uniform(90,0.6, 0.25, 0.12, modelname="U6", dataname="ast", Nrealizations=20)
 make.ecc.datafile.w.uniform.correlated(90,0.6, 0.25, 0.12, modelname="U6C", dataname="ast", Nrealizations=20)
 make.ecc.datafile.w.disc.mixture.correlated(90,c(0.5,0.5), c(0.05,0.3), 0.25, 0.12, data.to.add=list(Ncomp=2), modelname="R2C", dataname="ast", Nrealizations=20)
}


# make.datasets()