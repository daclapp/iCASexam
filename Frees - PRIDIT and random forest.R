
if (!require("pacman")) install.packages("pacman")
pacman::p_load(princomp,rpart)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

#re-write pridit code saved in "source code/Frees - PRIDIT - BOOK.R"

pip_data = read.csv("data/SimPIP.csv",header=TRUE)
glimpse(pip_data)
table(pip_data$Suspicion,pip_data$legalrep)

pip_data_xvars = pip_data %>% 
  select(-ID,-Suspicion)


apply_ridit = function(data) {
  # data = pip_data_xvars$legalrep #testing
  ridits = as.data.frame(data) %>%  #so we can use dplyr lag()
    count(data) %>% 
    mutate(cumsum_prior = lag(cumsum(n),1)) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% #to fix first entry that gets NA for lag(,-1)
    mutate(ridit_scaled = 2*(n*.5/sum(n)+ cumsum_prior/sum(n))-1) #bross version of ridit with transformation on p186
#join ridit back onto data.frame to get vector of length 1500  
  out = as.data.frame(data) %>% 
    left_join(ridits, by = "data") #"data" is the name of the vector that is the default column name it gives when converting from vector to data.frame
  return(out %>% select(ridit_scaled) %>% pull)
}

  #debugonce(apply_ridit)
  pip_data_xvars_ridit = pip_data_xvars %>% 
  mutate_all(apply_ridit)  #check that this matches riditsheet in source code using hash values

pca_out = prcomp(pip_data_xvars_ridit, scale.=TRUE) 
plot(pca_out, main="Scree Plot of PRIDITs")
loadings1<-as.matrix(pca_out$rotation)
summary(pca_out)
# write.csv(loadings1,file="c:/ClusterData/PcaLoad.csv")

biplot(pca_out)
pca_out_cor = prcomp(pip_data_xvars_ridit, cor.=TRUE) 
Pcascores<-pca_out_cor$scores #this doesn't exist!! does exist if using princomp below instead of pr comp
Pcascores
scoresout<-data.frame(Pcascores)
head(scoresout)

# write.csv(scoresout,file="c:/ClusterData/Pcascores.csv")

# use princomp to get the PRIDIT
pca_out2 = princomp(pip_data_xvars_ridit, cor=TRUE,scores=TRUE) 
plot(pca_out2, main="Scree Plot of PRIDITs")
# get loadings of variables on principal components
loadings2<-as.matrix(pca_out2$loadings)
loadings2
summary(pca_out2)
PRIDIT<-pca_out2$scores[,1:1]
Top2 = pca_out2$scores[,1:2] #I'm guessing this was what orig code was trying to do. But wasn't in that code 
head(Top2) 

TestPCA<-data.frame(pip_data$Suspicion,Top2)
head(TestPCA)

TreePridit<-rpart(pip_data$Suspicion~PRIDIT,data=TestPCA)
plot(TreePridit) #this is useless. no labels, but it's what source code has... not sure how to fix



################################
#Random Forest
#re-write pridit code saved in "source code/Frees - RFClustering BOOK.R"

pacman::p_load(randomForest,Hmisc,cluster)
head(pip_data) #assuming this is dat1<-read.csv("C:/ClusterData/ICA/PIPPriditOut.csv",header=TRUE). no other datasets on the books website

table(pip_data$Suspicion)
# 1 = not questionable
# 2 = suspicious

# per line 33. I'm of source code, data should have 28 columns, i have 27. source code also says this data should have 

# I'm going to give up on replicating on just review source code high level. If I have more time later maybe I'll email them on this

