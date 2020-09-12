# plot original time series of one patient

z <- read.csv(file_loc28, header=TRUE)
z <- read.zoo(z)
autoplot(z, facet = NULL, main='Patient 28')

