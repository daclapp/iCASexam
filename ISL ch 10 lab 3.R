
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ISLR)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

nci_labels = NCI60$labs
nci_data = NCI60$data

table(nci_labels)


#PCA

pca = prcomp(nci_data,scale=T)

COLORS = function(vec){
  colors = rainbow(length(unique(vec)))
  return(colors[as.numeric(as.factor(vec))])
}
#pca$  
par(mfrow=c(1,2))
plot(pca$x[,1:2],col=COLORS(nci_labels),pch=19)
plot(pca$x[,c(1,3)],col=COLORS(nci_labels),pch=19)

summary(pca)

plot(pca) #height = pca$sdev^2
#scree plot more informative (plots PVE)
pve=100*pca$sdev^2/sum(pca$sdev^2)

par(mfrow=c(1,2))
plot(pve,xlab="Principal Component")
plot(cumsum(pve),xlab="Principal Component")



#Hierarchical clustering

nci_data_scaled = scale(nci_data)
par(mfrow=c(1,3))
distance = dist(nci_data_scaled)
plot(hclust(distance),labels=nci_labels)
plot(hclust(distance,method="average"),labels=nci_labels)
plot(hclust(distance,method="single"),labels=nci_labels)

hc_out = hclust(dist(nci_data_scaled))
hc_clusters = cutree(hc_out,4)
table(hc_clusters,nci_labels)

par(mfrow=c(1,1))
plot(hc_out,labels=nci_labels)    
abline(h=139,col="red")

hc_out

#K means

set.seed(2)
km_out = kmeans(nci_data_scaled,4,nstart=20)
km_clusters = km_out$cluster
table(km_clusters,hc_clusters)


#can also do hclust() and kmeans() on just the first few principal components to remove noise from the data

hc_pca_out = hclust(dist(pca$x[,1:5]))
plot(hc_pca_out)
table(cutree(hc_pca_out,4),nci_labels)