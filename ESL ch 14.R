

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ElemStatLearn)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

rm(list=ls())
# https://cran.r-project.org/web/packages/ElemStatLearn/ElemStatLearn.pdf

#14.2.3 example
data(marketing)
glimpse(marketing)# y = Income

support = function(vec1 = NULL,cond1 = NULL,vec2 = NULL,cond2 = NULL){
  if(is.null(vec2)){
    vec1_eval = vec1 == cond1
    
    return(mean(vec1_eval)) 
  }
  else{
  vec1_eval = vec1 == cond1
  vec2_eval = vec2 == cond2 
  
  return(mean(vec1_eval*vec2_eval))
  }
}
# debugonce(support)
support(marketing$Sex,1,marketing$Age,5)

support(marketing$Sex,1)

market_analysis = function(vec1,cond1,vec2,cond2){
  support_all = support(vec1,cond1,vec2,cond2)
  support_1 = support(vec1,cond1)
  support_2 = support(vec2,cond2)
  
  confidence =  support_all /support_1#should be a % less than 1

  lift = confidence / support_2
  
  return(list(support = support_all,confidence = confidence, lift = lift))
}
# debugonce(market_analysis)
market_analysis(marketing$Sex,1,marketing$Age,5)




