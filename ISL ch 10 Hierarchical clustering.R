if (!require("pacman")) install.packages("pacman")
pacman::p_load(ISLR)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

set.seed(2)
x=matrix (rnorm (50*2) , ncol =2)
x[1:25 ,1]=x[1:25 ,1]+3
x[1:25 ,2]=x[1:25 ,2] -4


hc_complete = hclust(d = dist(x),method="complete")
hc_average = hclust(d = dist(x),method="average")
hc_single = hclust(d = dist(x),method="single")

par(mfrow=c(1,3))
plot(hc_complete)
plot(hc_average)
plot(hc_single)

cutree(hc_complete,2)
cutree(hc_average,2)
cutree(hc_single,4)

#scale before clustering:
x_scaled = scale(x)


par(mfrow=c(1,2))
plot(hclust(dist(x_scaled),method="complete")) #diff height, but dendogram looks the same
plot(hc_complete)

#use correlation as dist, but need at least 3 vars
x_3vars = matrix(rnorm(30*3),ncol=3)
dd = as.dist(1-cor(t(x_3vars)))
plot(hclust(dd,method="complete"))