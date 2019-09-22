#https://instruction.bus.wisc.edu/jfrees/jfreesbooks/PredictiveModelingVol1/unsupervised-predictive-modeling-methods/v2-chapter-7.html

# Test of Random Forest fot Clustering
# Uses RFClusterTutorial
# source http://www.genetics.ucla.edu/labs/horvath/RFclustering/RFclustering/RFclusterTutorialTheory.txt
#source("C:\Cluster\FunctionsRFclustering.txt")
#source("FunctionsRFclustering.txt")
# read in raw data
# dat1<-read.csv("C:/ClusterData/SimPIPPridData.csv",header=TRUE)
# read simulated questionable claims data. This data has output fro PRIDIT analysis and original data
dat1<-read.csv("C:/ClusterData/ICA/PIPPriditOut.csv",header=TRUE)


# totcols is number of variables
totcols=ncol(dat1)
totcols
# Total number of records, also gives size of proximity matrix
totrows <-nrow(dat1)
totrows
# print variable names
names(dat1)

# what number of questionable claims in each category
table(dat1$Suspicion)
# 1 = not questionable
# 2 = suspicious


# crosstab for suspicion vs legalrep
table(dat1$Suspicion,dat1$legalrep)

# subset data for predictor variables only if needed
datRF = dat1[,4:28]

#names(datRF)

# load needed libraries for randomForest clustering
library(randomForest)
library(Hmisc)
library(cluster)

# First RF Ensemble Tree unsupervised
Suspicious<-randomForest(datRF,ntree=500,proximity=T)
# output proximity to a folder on computer
# write.csv(Suspicious$proximity,"C:/ClusterData/SuspRFproximity.csv")
# print importance
TestSuspImp<-as.matrix(Suspicious$importance)
#TestSuspImp


Suspicious2<-randomForest(Suspicion~legalrep+ sprain+ chiropt+ emtreat+police+prior+ NumProv+NumTreat+RptLag+TrtLag+PolLag+Thresh+ Fault+ambulcost, dat1,ntree=500,proximity=T)
# output proximity to a folder on computer
# write.csv(Suspicious$proximity,"C:/ClusterData/SuspRFproximity.csv")
# print importance
TestSuspImp<-as.matrix(Suspicious$importance)
TestSuspImp




quantile(PolLag,probs=seq(0,1,.5))
  min(PolLag)

  breaks<-c(0,60,250, 750)
  table(cut(PolLag,breaks=breaks))
  

MDSplot(Suspicious,cut(PolLag,breaks=breaks),k=2)
  
  
MDSplot(Suspicious,as.factor(Inj01),k=2)

  table(datRF$Sprain)
   
MDSplot(Suspicious,Sprain,k=2)
  
  names(datRF)
  
MDSplot(Suspicious,as.factor(chiropt),k=2)
  
write.csv(TestSuspImp,"C:/ClusterData/ICA/PIPImportance.csv")

# Run multiple forests per recommendation of Shi and Horvath. This is 2nd
Suspicious2<-randomForest(datRF,ntree=500,proximity=T)

write.csv(Suspicious2$proximity,"C:/ClusterData/ICA/Susp2RFproximity.csv")
TestSusp2Imp<-as.matrix(Suspicious2$importance)
TestSusp2Imp
# output importance
write.csv(TestSusp2Imp,"C:/ClusterData/Suspicious2Importanceb.csv")


# Add proximities. Get dissimilarities from proximities

proximity<-(Suspicious2$proximity+Suspicious$proximity)/2

# a function to fix flaws in distances or dissimilarities from Horvath
 cleandist <- function(x) { 
        x1 <- as.dist(x)
        x1[x1<=0] <- 0.0000000001
        as.matrix(x1)
    }


#RFdissim<-data.frame(matrix(nrow=totrows,ncol=totrows))
proximity<-data.frame(proximity)
# Use formula from Horvath to get dissimilarity
RFdissim<-cleandist(sqrt(1-proximity))

 # This is from Horvath RFClustering Tutorial - not used here as it is not loaded



## PAM clustering based on the RF dissimilarity
# no.clusters = 2, 3, 4, 5
#labelRF = pamNew(distRF$cl1, no.clusters)

# load cluster library to do clustering
library(cluster)
# use pam function for clustering
ClusterlabelRF2 = pam(RFdissim,diss=T, 2)
ClusterlabelRF3 = pam(RFdissim,diss=T, 3)
ClusterlabelRF4 = pam(RFdissim,diss=T, 4)
ClusterlabelRF5 = pam(RFdissim,diss=T, 5)
ClusterlabelRF3$clustering<-as.factor(ClusterlabelRF3$clustering)
# what class are new variables
class(ClusterlabelRF3$clustering)

# convert to factor variables
ClusterlabelRF4$clustering<-as.factor(ClusterlabelRF4$clustering)
ClusterlabelRF3$clustering<-as.factor(ClusterlabelRF3$clustering)
ClusterlabelRF2$clustering<-as.factor(ClusterlabelRF2$clustering)


# tabulate counts in each cluster
table(ClusterlabelRF2$clustering)
table(ClusterlabelRF3$clustering)
table(ClusterlabelRF4$clustering)
table(ClusterlabelRF5$clustering)




## PAM clustering based on Euclidean distance
#labelEuclid = pamNew(dist(datRF), no.clusters)
# The Euclidean clusters to be compared to RF clusters
nrow(datRF)
distanceEuclid<-as.matrix(daisy(datRF, metric= "euclidean"))
# output distance 
write.csv(distanceEuclid,"C:/ClusterData/ICA/PIPDistanceICA.csv")

#labelEuclid = pamNew(distanceEuclid, no.clusters)
labelEuclid2 = pam(distanceEuclid,diss=T, 2)
labelEuclid3 = pam(distanceEuclid,diss=T, 3)
labelEuclid4 = pam(distanceEuclid,diss=T, 4)
labelEuclid5 = pam(distanceEuclid,diss=T, 5)

labelEuclid3$clustering<-as.factor(labelEuclid3$clustering)
labelEuclid4$clustering<-as.factor(labelEuclid4$clustering)
labelEuclid2$clustering<-as.factor(labelEuclid2$clustering)



table(labelEuclid2$clustering)
table(labelEuclid3$clustering)
table(labelEuclid4$clustering)
table(labelEuclid5$clustering)


plot(sample(labelEuclid2$clustering,100))


## Check the agreement between RF cluster and Euclidean distance cluster
table(labelEuclid2$clustering)
table(ClusterlabelRF2$clustering)

table(ClusterlabelRF2$clustering, labelEuclid2$clustering)  ## Crosstab of Clustering

fisher.test(table(ClusterlabelRF2$clustering, labelEuclid2$clustering))  ## Fisher’s exact p value


#write.csv(ClusterlabelRF2$clustering,ClusterlabelRF3$clustering,ClusterlabelRF4$clustering,labelEuclid2$clustering,labelEuclid3$clustering,labelEuclid4$clustering ,"C:/Cluster/ICAPipRFClusOut.csv")
ICAClusterRFdata<-data.frame(ClusterlabelRF2$clustering,ClusterlabelRF3$clustering,ClusterlabelRF4$clustering,labelEuclid2$clustering,labelEuclid3$clustering,labelEuclid4$clustering)
nrow(ICAClusterRFdata)

ICAClusterRFdata[1:10,]


write.csv(ICAClusterRFdata,"C:/ClusterData/ICA/ICAPipRFClusOut.csv")


#write.csv(labelRF2$clustering,labelRF3$clustering,labelEuclid2$clustering,labelEuclid3$clustering,"C:/Cluster/PIPClusOut.csv")

PIPDatawClus<-data.frame(dat1,ICAClusterRFdata)
nrow(datRF)
nrow(PIPDatawClus)

write.csv(PIPDatawClus,file="C:/ClusterData/PIPDatawClus.csv")

# Do a tree forImportance
# load rpart library for tree fitting
library(rpart)
library(rpart.plot)
# What is best number of clusters
#TreeTest<-rpart(Suspicion~ClusterlabelRF2$clustering+as.factor(ClusterlabelRF3$clustering)+as.factor(ClusterlabelRF4$clustering),data=PIPDatawClus)
TreeTest<-rpart(Suspicion~ClusterlabelRF2$clustering+(ClusterlabelRF3$clustering)+(ClusterlabelRF4$clustering),data=PIPDatawClus)
#print importance
TreeTest$variable.importance

rpart.plot(TreeTest,type=4,extra=101,under=T) 
TreeTest<-rpart(Suspicion~ClusterlabelRF2$clustering+as.factor(ClusterlabelRF3$clustering)+as.factor(ClusterlabelRF4$clustering),data=PIPDatawClus)

# importance ranking of the clusters
TreeTest<-rpart(Suspicion~ClusterlabelRF2$clustering+ClusterlabelRF3$clustering+ClusterlabelRF4$clustering+labelEuclid2$clustering+labelEuclid3$clustering+labelEuclid4$clustering,data=PIPDatawClus)
#TreeTest<-rpart(Suspicion~ClusterlabelRF2$clustering+as.factor(ClusterlabelRF3$clustering)+as.factor(ClusterlabelRF4$clustering+as.factor(labelEuclid2$clustering)+as.factor(labelEuclid3$clustering)+as.factor(labelEuclid4$clustering),data=PIPDatawClus)


summary(TreeTest)
TreeTest$variable.importance
rpart.plot(TreeTest,type=4,extra=101,under=T) 

# add pridit to tree
PRIDIT<-Factor1
TreeTest<-rpart(Suspicion~Factor1+Factor2+PriditScores1,data=PIPDatawClus)
PRIDIT<-PIPDatawClus$Factor1
TreeTest<-rpart(Suspicion~PRIDIT+ClusterlabelRF2$clustering+ClusterlabelRF3$clustering+ClusterlabelRF4$clustering+labelEuclid2$clustering+labelEuclid3$clustering+labelEuclid4$clustering,data=PIPDatawClus)


summary(TreeTest)
TreeTest$variable.importance
rpart.plot(TreeTest,type=4,extra=101,under=T) 

# get nice tree
prp(TreeTest,type=4,extra=101,under=T)

TreeTest<-rpart(Suspicion~PRIDIT+ClusterlabelRF2$clustering+ClusterlabelRF3$clustering+ClusterlabelRF4$clustering+labelEuclid2$clustering+labelEuclid3$clustering+labelEuclid4$clustering,,method="class",data=PIPDatawClus)

#summary(TreeTest)
ImportanceTest<-TreeTest$variable.importance
write.csv(ImportanceTest,file="C:/ClusterData/ICA/ImportanceTest.csv")
ImportanceTest

rpart.plot(TreeTest,type=4,extra=101,under=T) 
tapply(PIPDatawClus$Factor1,Suspicion,mean)
library(MASS)
boxplot(PRIDIT~Suspicion)

# get nice tree
prp(TreeTest,type=4,extra=101,under=T)


TreeForestTest<-randomForest(Suspicion~PRIDIT+ClusterlabelRF2$clustering+ClusterlabelRF3$clustering+ClusterlabelRF4$clustering+labelEuclid2$clustering+labelEuclid3$clustering+labelEuclid4$clustering,data=PIPDatawClus)

TreeForestTest$importance

TreeExplore<-rpart(ClusterlabelRF2$clustering~PRIDIT,data=PIPDatawClus)

##############



