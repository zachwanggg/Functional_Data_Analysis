# plot 32 plots instead
for(node in 1:32){
    result_obj <- f_fourier_smooth(time_subset=c(1:600), data_mat, node_subset=c(node), k=32)
    smoothed_curve = eval.fd(c(1:600),result_obj$fd)
    transformed_node = transform.Cycle(smoothed_curve, register=1)
    df_tmp = data.frame(cycle=integer(), time=integer(), y_value=integer())
    for(i in 1:length(unique(transformed_node$cycle))){
      tmp=subset(transformed_node, cycle==i)
      tmp$time=(tmp$time)/max(tmp$time)
      df_tmp=rbind(df_tmp,tmp)
    }
    df_new = data.frame(matrix(nrow=80))
    for(i in 1:length(unique(df_tmp$cycle))){
      xx=seq(0,1,length.out=80)
      tmp=subset(df_tmp, cycle==i)
      s=smooth.spline(x=tmp$time, y=tmp$y_value,  df = 10)
      df_new[,ncol(df_new)+1]=predict(s,xx)$y
    }
    df_new=df_new[,2:(length(unique(df_tmp$cycle))+1)]
    ## FPCA now
    fPCA_subset <- function(data_mat, k, nharm, plt){
      basis <- create.fourier.basis(c(1,nrow(data_mat)), k) 
      smoothfd <- smooth.basis(1:nrow(data_mat), data_mat, basis)$fd 
      pcalist = pca.fd(smoothfd, nharm, harmfdPar=fdPar(smoothfd))
      rotpcalist = varmx.pca.fd(pcalist)
      par(mfrow=c(nharm,1))
      if(plt==1){
        plot.pca.fd(rotpcalist)
      } 
      return(rotpcalist)
    }
    df_new <- as.matrix(df_new)
    rotpcalist = fPCA_subset(df_new, k=11, nharm=2, plt=0)
    par(mfrow=c(1,1))
    plot(rotpcalist$harmonics)  
}
