pc.df <- function(patient_df){
  PC1_df = data.frame(matrix(nrow=80))
  PC2_df = data.frame(matrix(nrow=80))
  mean_df = data.frame(matrix(nrow=80))
  out = list()
  for(node in 1:32){
    node.df = node.scaler(patient_df, node, k=21)
    node.df = as.matrix(row.check(node.df))
    rotpcalist = fPCA.nodes(data_mat=node.df, k=11, nharm=2, plt=0)
    # PC1 & PC2
    harmfd <- rotpcalist[[1]]
    basisfd <- harmfd$basis
    rangex <- basisfd$rangeval
    x <- seq(rangex[1], rangex[2], length = harmfd$basis$rangeval[2])
    meanmat <- eval.fd(x, rotpcalist$meanfd)
    fdmat <- eval.fd(x, harmfd)
    fac1 <- sqrt(rotpcalist$values[1]) 
    fac2 <- sqrt(rotpcalist$values[2]) 
    
    PC1_df[,ncol(PC1_df)+1]=2*fac1*fdmat[,1]    
    PC2_df[,ncol(PC2_df)+1]=2*fac2*fdmat[,2]
    mean_df[,ncol(mean_df)+1]=meanmat
  }
  PC1_df = data.frame(PC1_df[,2:(ncol(PC1_df))])
  names(PC1_df)=colnames(patient1)
  PC2_df = data.frame(PC2_df[,2:(ncol(PC2_df))])
  names(PC2_df)=colnames(patient1)
  mean_df = data.frame(mean_df[,2:(ncol(mean_df))])
  names(mean_df)=colnames(patient1)
  out$pc1 = PC1_df
  out$pc2 = PC2_df
  out$mean = mean_df
  return(out)
}

#######
pd_placebo <- c(1,4,6,10,14,15,18,20,22,23,31,46,47,50,56,60,61,64,65,80,81,91,97)
hc_placebo <- c(11,25,29,33,38,40,41,43,51,58,67,70,71,73,86,90,94,96,100)
####
for (i in hc_placebo){
  df_name <- paste("patient", i, sep = "")
  assign(df_name, ReadFile(paste('/Users/hanwang/desktop/Git_desktop/Functional_Data_Analysis/Data for Zach/Data for Zach ', i,'.csv', sep=""),
                           time_subset=c(1:600), node_subset=c(1:32)))
  tmp=pc.df(ReadFile(paste('/Users/hanwang/desktop/Git_desktop/Functional_Data_Analysis/Data for Zach/Data for Zach ', i,'.csv', sep=""),
                     time_subset=c(1:600), node_subset=c(1:32)))
  a =tmp$pc1
  a$index = c(1:80)
  d <- melt(data = a, id.vars = c("index"), measure.vars = colnames(patient1))
  print(ggplot(d, aes(x = index, y = value)) + 
    geom_line(aes(color = variable, linetype = variable)))
}
