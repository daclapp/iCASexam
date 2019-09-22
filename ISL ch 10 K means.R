
if (!require("pacman")) install.packages("pacman")
pacman::p_load()
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

set.seed(2)
x = matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

kmeans_output = kmeans(x,
                       centers = 2, #=K number of clusters
                       nstart=20)# number of times to randomly select starting point and re-run algorithm. R only outputs the best starting point (lowest variance)
kmeans_output
kmeans_output$cluster
plot(x,col=kmeans_output$cluster+1,#+1 just shifts starting color
     main = "K-Means Clustering Results with K=2",xlab="",ylab="",pch=20, #filled in points
     cex=2 #bigger points
     )1
