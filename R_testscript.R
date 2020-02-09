library(foreach)
library(doParallel)

cl<-makeCluster(no_cores)
registerDoParallel(cl)
clusterExport(cl, "base")
base <- 4

foreach(exponent = 2:4, 
        .combine = c)  %dopar%  
  base^exponent

