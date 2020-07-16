library(zoo)
library(fda.usc)
library(dplyr)
library(reshape)
library(pracma)
library(ggplot2)
#####1. import data

data_15may2020 <- read.csv("/Users/hanwang/desktop/Git_desktop/Functional_Data_Analysis/data_15may2020.csv", header=TRUE)
#data_15may2020 <- read.csv("C:/Users/Han Wang/Desktop/Git_desktop/Functional_Data_Analysis/data_15may2020.csv", header=TRUE)
z <- read.zoo(data_15may2020)
data_15may2020 <- select(data_15may2020, -c(X))
data_mat <- as.matrix(data_15may2020)


### function
plot.periodicCycle = function(data, register){
  # obtain index at which curve crosses 0
  x=diff(ifelse(data>0,1,0))       #crossed 0---> -1: pos to neg,    1: neg to pos
  z_idx=(1:599)[x!=0]             #returns: location index where curve crosses X-axis
  
  # skip first crossing if it is from positive to negative
  if (x[z_idx[1]]==-1){
    z_idx=z_idx[-1]
  }
  
  #put every complete cycle in a Dataframe
  i=1
  cl=1
  result=data.frame(cycle=integer(), time=integer(), y_value=integer())
  while (i+2<=length(z_idx)){
    
      if(register==0){
        tmp=data.frame(cycle=cl, time=seq(z_idx[i],z_idx[i+2]), y_value=smoothed_curve[z_idx[i]:z_idx[i+2]])
      }
      else{
        tmp=data.frame(cycle=cl, time=seq(1,length(seq(z_idx[i],z_idx[i+2]))), y_value=smoothed_curve[z_idx[i]:z_idx[i+2]])  
        tmp[,2]=tmp[,2]-length(seq(z_idx[i],z_idx[i+1]))-1
      }
      result=rbind(result,tmp)
      i=i+2
      cl=cl+1
    }
  ggplot(result, aes(time, y_value,group=cycle, colour=cycle)) + geom_line() + theme(legend.position="top")
}


f_fourier_smooth <- function(time_subset, data_mat, node_subset, k){
  basis <- create.fourier.basis(c(time_subset[1],time_subset[length(time_subset)]), k)
  fd_obj <- smooth.basis(time_subset, data_mat[time_subset,node_subset], basis)
  smoothfd <- fd_obj$fd
  plot(smoothfd)
  title(main=paste("Fourier Basis Smoothing of node:", node_subset, ", Basis_number:",k ))
  return(fd_obj)
}

result_obj <- f_fourier_smooth(time_subset=c(1:600), data_mat, node_subset=c(1), k=50)
smoothed_curve = eval.fd(c(1:600),result_obj$fd)
plot.periodicCycle(data=smoothed_curve, register=1, standardized=1)
plot.periodicCycle(data=smoothed_curve, register=1)
