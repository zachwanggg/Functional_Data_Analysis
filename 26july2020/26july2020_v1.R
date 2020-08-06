# find y value based on PC coef and sin/cos functions
PC1_df = data.frame(matrix(nrow=80))
PC2_df = data.frame(matrix(nrow=80))
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
    PCA_df <- data.frame(matrix(nrow=rotpcalist$harmonics$basis$rangeval[2]))
    # Calculation for Harmonics/eigenvectors
    for(x in (rotpcalist$harmonics$basis$rangeval[1]:rotpcalist$harmonics$basis$rangeval[2])){
      PC1_coef = rotpcalist$harmonics$coefs[,1]
      PC2_coef = rotpcalist$harmonics$coefs[,2]
      flag = ifelse(index(PC1_coef)%%2==0, 1, 0)
      even_idx = (1:length(PC1_coef))[flag==1]
      odd_idx = (1:length(PC1_coef))[flag==0][-1]
      PCA_df[x, 1] = PC1_coef[1] + sin(x)*sum(PC1_coef[even_idx]) + cos(x)*sum(PC1_coef[odd_idx]) # fixme
      PCA_df[x, 2] = PC2_coef[1] + sin(x)*sum(PC2_coef[even_idx]) + cos(x)*sum(PC2_coef[odd_idx]) # fixme
    
        ## calculation trial 2
      #PCA_df[x, 1] = PC1_coef[1]
      #PCA_df[x, 2] = PC2_coef[1]
      #t=1
      #for(i in 1:length((even_idx))){
      #  PCA_df[x, 1]=PCA_df[x, 1]+sin(t*x*PC1_coef[even_idx[i]])
      #  PCA_df[x, 2]=PCA_df[x, 2]+sin(t*x*PC2_coef[even_idx[i]])
      #  t=t+1
      #}
      #t=1
      #for(j in 1:length((odd_idx))){
      #  PCA_df[x, 1]=PCA_df[x, 1]+sin(t*x*PC1_coef[odd_idx[j]])
      #  PCA_df[x, 2]=PCA_df[x, 2]+sin(t*x*PC2_coef[odd_idx[j]])
      #  t=t+1
      #}
      
    }
PC1_df[,ncol(PC1_df)+1]=PCA_df[,1]    
PC2_df[,ncol(PC2_df)+1]=PCA_df[,2]
}

PC1_df = data.frame(PC1_df[,2:(ncol(PC1_df))])
names(PC1_df)=colnames(data_mat)

PC2_df = data.frame(PC2_df[,2:(ncol(PC2_df))])
names(PC2_df)=colnames(data_mat)

z_1 = read.zoo(PC1_df, index='index')
z_2 = read.zoo(PC2_df, index='index')
autoplot(z_1, facet = NULL)

#fPCA_subset(mean_df, k=4, nharm=2, plt=0)
