#######
##### Plot variance on all nodes for one patient
PC1_df = data.frame(matrix(nrow=80))
PC2_df = data.frame(matrix(nrow=80))
mean_df = data.frame(matrix(nrow=80))
for(node in 1:32){
  node.df = node.scaler(patient5, node, k=32)
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
## plot
### PC1
z_1 = read.zoo(PC1_df, index='index')
### PC2
z_2 = read.zoo(PC2_df, index='index')
### Mean Functional Curves
z_3 = read.zoo(mean_df, index='index')

autoplot(z_1, facet = NULL, main='Patients 80, variance of all nodes on PC1')
autoplot(z_2, facet = NULL, main='Patients 80, variance of all nodes on PC2')
autoplot(z_3, facet = NULL, main='Patients 80, mean curves')

#########
#### plot fPCA on variance along PC1 ######
fPCA.nodes(as.matrix(PC1_df), k=11, 2, 1)

