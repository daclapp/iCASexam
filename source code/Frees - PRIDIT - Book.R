#https://instruction.bus.wisc.edu/jfrees/jfreesbooks/PredictiveModelingVol1/unsupervised-predictive-modeling-methods/v2-chapter-7.html

# Code to calculate RIDITs and PRIDITs on Questionable Claims Data
# Read in questionable claims data
# this version of data has dependent var and is 1500 lines
mydata1<-read.csv("data/SimPIP.csv",header=TRUE)
# get variable names
names(mydata1)

# get number of rows and columns
nrow(mydata1)
ncol(mydata1)
table(Suspicion,legalrep)

mydata=mydata1[,3:27]
mydata[1:5,]


####### PRIDIT ####### 

totalcols = ncol(mydata)
totalrows = nrow(mydata)
# create data frame to hold the RIDITS
riditsheet = data.frame(matrix(nrow=totalrows,ncol=totalcols))
# use same variable names as for original data
names(riditsheet) = names(mydata)


for (i in 1:totalcols) {
    i=1
    worksheet = data.frame(table(mydata[,i]))  # converts results of frequency table to data frame
    temp = cumsum(worksheet$Freq) # cumulative sum of frequwncies from table
    worksheet$CumSum = union(0,temp[-length(temp)])   # cumulative sum of all prior freq table entries 
    # compute RIDIT
    worksheet$Ridit = (0.5*worksheet$Freq + worksheet$CumSum)/totalrows
    worksheet$Ridit_scaled = 2 * worksheet$Ridit - 1

    # create hash table mapping original data to corresponding RIDIT value
    hashtable = new.env(hash=TRUE)
    nrows = nrow(worksheet)
    for (x in 1:nrows) {
        dummy = toString(worksheet$Var1[x])
        hashtable[[dummy]] = worksheet$Ridit_scaled[x]
    }

    # find RIDIT values for original data
    for (j in 1:totalrows) {
        dummy = toString(mydata[j,i])
        riditsheet[j,i] = hashtable[[dummy]]
    }
}

# get principal component wit prcomp
pca_out = prcomp(riditsheet, scale.=TRUE) 
plot(pca_out, main="Scree Plot of PRIDITs")
loadings1<-as.matrix(pca_out$rotation)

write.csv(loadings1,file="c:/ClusterData/PcaLoad.csv")

biplot(pca_out)
pca_out2 = prcomp(riditsheet, cor.=TRUE) 
Pcascores<-pca_out2$scores
Pcascores
scoresout<-data.frame(Pcascores)
scoresout[1:10,]

write.csv(scoresout,file="c:/ClusterData/Pcascores.csv")

# use princomp to get the PRIDIT
pca_out2 = princomp(riditsheet, cor=TRUE,scores=TRUE) 
plot(pca_out2, main="Scree Plot of PRIDITs")
# get loadings of variables on principal components
loadings2<-as.matrix(pca_out2$loadings)
loadings2
summary(pca_out2)
PRIDIT<-pca_out2$scores[,1:1]
#Top2[,1:2]
Top2[1:10,]

Suspicion<-mydata1$Suspicion
TestPCA<-data.frame(Suspicion,Top2)
TestPCA[1:10,]

TreePridit<-rpart(Suspicion~PRIDIT,data=TestPCA)
rpart.plot(TreePridit)

# write the RIDIRS to file
write.csv(riditsheet,file="c:/ClusterData/PIPriditSim1.csv")
# Write the PRIDIT
write.csv(PRIDIT,file="c:/ClusterData/PIPRIDIT.csv")
write.csv(Totalscore,file="c:/ClusterData/PIPScoreSim1.csv")

#-----------------------------------------
