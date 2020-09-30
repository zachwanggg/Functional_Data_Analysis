node1 = Wave(node1[,3], register=1)
ggplot(node1, aes(time, y_value,group=cycle, colour=cycle)) + geom_line() + theme(legend.position="right")+ggtitle("Patient 1, Node 1")


a = node.scaler(tmp, node=c(1))
b = row.check(node_data=a)
b = read.zoo(b, index='index')
autoplot(b, facet = NULL, main='Patient 9, Node 1')
rotpcalist = fPCA.nodes(as.matrix(b), k=11, nharm=2, plt=1)



#### plot Variance on single node
PC1_df = data.frame(matrix(nrow=80))
PC2_df = data.frame(matrix(nrow=80))
mean_df = data.frame(matrix(nrow=80))

node.df = node.scaler(patient28, node=1, k=32)
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
PC1_df = data.frame(PC1_df[,2:(ncol(PC1_df))])
PC2_df = data.frame(PC2_df[,2:(ncol(PC2_df))])
mean_df = data.frame(mean_df[,2:(ncol(mean_df))])
z_1 = read.zoo(PC1_df, index='index')
### PC2
z_2 = read.zoo(PC2_df, index='index')
### Mean Functional Curves
z_3 = read.zoo(mean_df, index='index')

autoplot(z_1, facet = NULL, main='Variance on PC1')
autoplot(z_2, facet = NULL, main='Variance on PC2')
autoplot(z_3, facet = NULL, main='mean curves')