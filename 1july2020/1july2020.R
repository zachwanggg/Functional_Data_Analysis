library(zoo)
library(fda.usc)
library(dplyr)
library(xlsx)

#####1. import data

#data_15may2020 <- read.csv("/Users/hanwang/desktop/git_user/Functional_Data_Analysis/data_15may2020.csv", header=TRUE)
data_15may2020 <- read.csv("C:/Users/Han Wang/Desktop/Git_desktop/Functional_Data_Analysis/data_15may2020.csv", header=TRUE)
z <- read.zoo(data_15may2020)
data_15may2020 <- select(data_15may2020, -c(X))
data_mat <- as.matrix(data_15may2020)

#####2. same function used from last time
fPCA_subset <- function(time_subset, data_mat, node_subset, k, nharm){
  basis <- create.fourier.basis(c(time_subset[1],time_subset[length(time_subset)]), k)
  smoothfd <- smooth.basis(time_subset, data_mat[time_subset,node_subset], basis)$fd
  plot(smoothfd)
  title(main="smoothed curves")
  pcalist = pca.fd(smoothfd, nharm, harmfdPar=fdPar(smoothfd))
  rotpcalist = varmx.pca.fd(pcalist)
  par(mfrow=c(nharm,1))
  plot.pca.fd(rotpcalist)
  return(rotpcalist)
}

rotpcalist = fPCA_subset(time_subset=c(1:600), data_mat, node_subset = c(1,6,7,9,15)
                         , k=281, nharm=2)
coefdf=rotpcalist$harmonics$coefs

######
######
######
###### evaluate fourier basis functions

tt=c(1:600)
basis = create.fourier.basis(c(1,600),7)
phimat = eval.basis(c(1:600),basisobj=basis)
phi.frame = data.frame(cbind(phimat,tt))
melt.phi = melt(data=phi.frame,id.vars="tt")
plot(melt.phi$tt, melt.phi$value)

#####
#####
basis7 = create.fourier.basis(rangeval = range(tt),nbasis = 281)
fourier7.fd = smooth.basis(argvals = tt, y = data_mat[,10],fdParobj =basis7)$fd
ovi7 = eval.fd(tt,fourier7.fd)

## export coefdf
write.csv(as.data.frame(coefdf), file = "C:/Users/Han Wang/Desktop/Git_desktop/Functional_Data_Analysis/coef.csv")
