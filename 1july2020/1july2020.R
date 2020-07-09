library(zoo)
library(fda.usc)
library(dplyr)
library(reshape)
library(xlsx)
library(ggplot2)
#####1. import data

data_15may2020 <- read.csv("/Users/hanwang/desktop/git_user/Functional_Data_Analysis/data_15may2020.csv", header=TRUE)
#data_15may2020 <- read.csv("C:/Users/Han Wang/Desktop/Git_desktop/Functional_Data_Analysis/data_15may2020.csv", header=TRUE)
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
par(mfrow=c(1,1))
plot(melt.phi$tt, melt.phi$value)

########################
########################
result_obj <- f_fourier_smooth(time_subset=c(1:600),data_mat
                               ,node_subset=c(1), k=50)

smoothed_curve = eval.fd(c(1:600),result_obj$fd)
plot(smoothed_curve)



### function
plot.periodicCycle = function(data, original){
  x=diff(ifelse(data>0,1,0))       #crossed 0---> -1: pos to neg,    1: neg to pos
  z_idx=(1:599)[x!=0]             #returns: location index where curve crosses X-axis
  N=idivide(length(z_idx),3)
  i=1
  result=data.frame(cycle=integer(), time=integer(), y_value=integer())
  while (i<=N){
    if(original==1){
      tmp=data.frame(cycle=i, time=seq(z_idx[1+(i-1)*3],z_idx[3+(i-1)*3]), y_value=smoothed_curve[z_idx[1+(i-1)*3]:z_idx[3+(i-1)*3]])
    }
    else{
      tmp=data.frame(cycle=i, time=seq(1,length(seq(z_idx[1+(i-1)*3],z_idx[3+(i-1)*3]))), y_value=smoothed_curve[z_idx[1+(i-1)*3]:z_idx[3+(i-1)*3]])  
    }
    result=rbind(result,tmp)
    i=i+1
  }
  ggplot(result, aes(time, y_value,group=cycle, colour=cycle)) + geom_line() + theme(legend.position="top")
}



#result_obj <- f_fourier_smooth(time_subset=c(1:600), data_mat, node_subset = c(1)
#                         ,k=selection_result[which.min(selection_result[,2]),1],   
#                         nharm=2)

result_obj <- f_fourier_smooth(time_subset=c(1:600), data_mat, node_subset=c(1), k=50)
smoothed_curve = eval.fd(c(1:600),result_obj$fd)
plot.periodicCycle(data=smoothed_curve, original=1)
