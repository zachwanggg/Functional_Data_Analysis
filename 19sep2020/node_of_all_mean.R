
node1 = Wave(patient10[,1], register=0)
ggplot(node1, aes(time, y_value,group=cycle, colour=cycle)) + geom_line() + theme(legend.position="right")+ggtitle("Patient 10, Node 1")


pd_placebo <- c(1,4,6,10,14,15,18,20,22,23,31,46,47,50,56,60,61,64,65,80,81,91,97)
hc_placebo <- c(11,25,29,33,38,40,41,43,51,58,67,70,71,73,86,90,94,96,100)
#### put node1 into dataframe
node1 = data.frame(matrix(nrow=600))
for (i in pd_placebo){
  tmp = ReadFile(paste('/Users/hanwang/desktop/Git_desktop/Functional_Data_Analysis/Data for Zach/Data for Zach ', i,'.csv', sep=""))
  tmp1 = Wave(tmp[,1], register=0)
  node1[,ncol(node1)+1]=spline(index(tmp1), tmp1$y_value, n = 600, method = "natural")$y
}
node1=node1[,2:length(node1)]
names(node1)=paste('patient', pd_placebo, sep = "")
node1$index = c(1:600)
##
### plot
d <- melt(data = node1, id.vars = c("index"), measure.vars = paste('patient', pd_placebo, sep = ""))
print(ggplot(d, aes(x = index, y = value)) + 
        geom_line(aes(color = variable, linetype = variable)) + ggtitle("PD-placebo, Node 1"))
### 
rotpcalist = fPCA.nodes(as.matrix(node1[,1:23]), k=11, nharm=2, plt=1)

######## function
nodes.compare <- function(patient_list, node_number){
  nodes_df = data.frame(matrix(nrow=600))
  for (i in patient_list){
    tmp = ReadFile(paste('/Users/hanwang/desktop/Git_desktop/Functional_Data_Analysis/Data for Zach/Data for Zach ', i,'.csv', sep=""))
    tmp1 = Wave(tmp[,node_number], register=0)
    nodes_df[,ncol(nodes_df)+1]=spline(index(tmp1), tmp1$y_value, n = 600, method = "natural")$y
  }
  nodes_df=nodes_df[,2:length(nodes_df)]
  names(nodes_df)=paste('patient', pd_placebo, sep = "")
  nodes_df$index = c(1:600)
  return(nodes_df)
}

node1=nodes.compare(pd_placebo, node_number=1)
d <- melt(data = node1, id.vars = c("index"), measure.vars = paste('patient', pd_placebo, sep = ""))
print(ggplot(d, aes(x = index, y = value)) + 
        geom_line(aes(color = variable, linetype = variable)) + ggtitle("PD-placebo, Node 1"))
