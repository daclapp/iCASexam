
if (!require("pacman")) install.packages("pacman")
pacman::p_load(MASS)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)


#simulate survey with 200 responders and 16 designs with 4 roller coaster attributes 
set.seed(12814)

nques = 16 #conjoint ratings per respondent

#create the 16 combos asked to each respondent
coaster_options = data.frame(speed = sample(as.factor(c("40","50","60","70")),size = nques,replace = T),
                             height = sample(as.factor(c("200","300","400")),size = nques,replace = T),
                             construction = sample(as.factor(c("Wood","Steel")),size = nques,replace = T),
                             theme = sample(as.factor(c("Dragon","Eagle")),size = nques,replace = T)
                             )

matrix = model.matrix(~ speed + height + construction + theme,
                      data = coaster_options)
#simulate 200 responses. roduces 8 columns for the 8 levels above (matrix also has 8 columns). 
# For example speed has 4 levels but one is used as base level. So 3 columns. 4-1= 3 columns for height and 2-1=1 column each for construction and theme
weights = mvrnorm(n = 200,
            mu = c(-3,.5,1,3,2,1,0,-.5),
            Sigma = diag(c(.2,.1,.1,.1,.2,.3,1,1))) #simulating underlying importance of each characteristic to each indiv. not there overall rating of that combination of a coaster

conjoint_data = NULL
for (i in 1:200){
  #create on respondents overall ratings of the 16 combinations, plus error
  utility = matrix %*% weights[i,] + rnorm(16) #16x8 * 1x8 (vector so must be treat as 8x1) = 16x1
  rating = as.numeric(cut(utility,10)) #converts to rating you would expect on a normal survey
  temp = cbind(responder = rep(i,nques),rating,coaster_options) #use book code to get same results
  conjoint_data = rbind(conjoint_data,temp)
}

summary(conjoint_data)

conjoint_data %>% 
  group_by(height) %>% 
  summarize(avg_rating = mean(rating))

model_lm = lm(rating ~ speed + height + construction + theme,
              data = conjoint_data)
summary(model_lm)










