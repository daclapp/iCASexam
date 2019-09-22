
if (!require("pacman")) install.packages("pacman")
pacman::p_load()
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

head(USArrests)

apply(USArrests,2,mean) #1 is rows, 2 is columns
apply(USArrests,2,sd)

pca = prcomp(USArrests,scale=T) #centering automatic but need to specify scaling each var to unit variance
names(pca) # or pca$
pca$center #the means that it centered it at
pca$scale #the sd 
pca$rotation #or "loadings"
head(pca$x)

#to replicate fig 10.1
pca$rotation = -pca$rotation #same results, but you can change the sign without affecting model. some programs might produce different signs
pca$x=-pca$x
biplot(pca,scale=0) # ?biplot.prcomp
#scale=0 ensures arrows are scaled to represent the loadings
pca_var = pca$sdev^2 #variance explained by each principal component
pve = pca_var/sum(pca_var)#Proportion of Variance Explained by each principal compononent
pve

plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",
     ylim=c(0,1),type='b') #b = both points and lines

plot(cumsum(pve),xlab="Principal Component",ylab="Proportion of Variance Explained",
     ylim=c(0,1),type='b')


