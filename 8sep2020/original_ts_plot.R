# plot original time series of one patient
i=100
df<-ReadFile(paste('/Users/hanwang/desktop/Git_desktop/Functional_Data_Analysis/Data for Zach/Data for Zach ', i,'.csv', sep=""),
         time_subset=c(1:600), node_subset=c(1,2,3,8,11,31))
# plot patient data
d <- melt(data = df, id.vars = c("index"), measure.vars = colnames(patient1))
print(ggplot(d, aes(x = Var1, y = value)) + 
        geom_line(aes(linetype=Var2,color = Var2,))+ggtitle(paste("patient", i)))

