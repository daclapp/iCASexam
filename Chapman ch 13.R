
if (!require("pacman")) install.packages("pacman")
pacman::p_load(MASS,mlogit)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

#code generating data not on syllabus objectives so might be able to just skip to modeling part
attributes = list(seat = c("6","7","8"),
                  cargo = c("2ft","3ft"),
                  engine = c("gas","hyb","elec"),
                  price = c("30","35","40")
                  )
#generate part worths for the attributes. (latent values a customer places on levels of an attribute when making choices) or the value over the base level - sounds like what the Beta is
#latent means not directly observed, but inferred from other variables

coef_names = NULL
for (i in 1:length(attributes)){
  coef_names = c(coef_names,
                 paste0(names(attributes)[i],attributes[[i]][-1]))
}

# simulate data:
mu = c(-1,-1,.5,-1,-2,-1,-2)
names(mu) = coef_names
mu

#assume each respondent has unique part worths:
sigma = diag(c(.3,1,.1,.3,1,.2,.3))
dimnames(sigma) = list(coef_names,coef_names)
sigma["enginehyb","engineelec"] = sigma["engineelec","enginehyb"] = .3 #creates correlation btw part worth for engelec and enghyb
sigma

set.seed(33040)
responder_ids = 1:200
carpool = sample(c("yes","no"), size = length(responder_ids),replace = T)

coefs = mvrnorm(length(responder_ids),mu=mu,Sigma=sigma)
colnames(coefs) = coef_names

# increase part worth for minivans for carpoolers:
coefs[carpool=="yes","seat8"] = coefs[carpool=="yes","seat8"] +2
coefs[carpool=="yes","seat7"] = coefs[carpool=="yes","seat7"] +1.5

num_questions = 15
num_alternatives = 3

profiles = expand.grid(attributes) #all possible combinations
profiles_coded = model.matrix(~seat+cargo+engine+price,
                              data = profiles)[,-1] #-1 drops intercept

#generate survey questions and responses:
survey_data = data.frame(NULL)
for (i in 1:length(responder_ids)){
  profiles_i = sample(1:nrow(profiles),size = num_questions*num_alternatives) #total profiles shown to each respondent
  utility = profiles_coded[profiles_i,] %*% coefs[i,]#what we expect each profile to be in value for each responders using their coefs
  utility_wide = matrix(data=utility, ncol=num_alternatives,byrow = T) #converts to 15 questions with 3 alternatives each
  
  probs = exp(utility_wide)/rowSums(exp(utility_wide)) #prob of them picking each alternative (multinomial logit probabilities)
  
  choice_temp = apply(probs,1,function(x) sample(1:num_alternatives,size = 1, prob = x)) 
  #1 means row wise apply. random choice for which alternative they will choose. 
  # WHY AREN'T WE USING THE PROBS ABOVE TO PICK THIS?
  
  choice = rep(choice_temp,each=num_alternatives) == rep(1:num_alternatives,num_questions) 
  
  conjoint_i = data.frame(resp_id = rep(i,num_questions),
                         question = rep(1:num_questions, each = num_alternatives), #repeats 1:15 3 times
                         alternative = rep(1:num_alternatives,num_questions), #repeats 1,2,3 15 times
                         carpool = rep(carpool[i],num_questions),
                         profiles[profiles_i,], #the 45 alternatives shown to this respondent
                         choice = as.numeric(choice)
                         )#re-write using dplyr and 
  survey_data = rbind(survey_data,conjoint_i)
  }

survey_data = survey_data %>% 
  mutate(price_numeric = as.numeric(as.character(price)))

head(survey_data)
summary(survey_data)

survey_data %>% 
  group_by(price) %>% 
  summarize(choice = sum(choice)) #instead of xtabs()

data_mlogit = mlogit.data(data = survey_data,
                           choice = "choice",
                           shape = "long",
                           varying = 3:6,
                           alt.levels = paste("pos",1:3),
                           id.var = "resp_id")

model_mlogit = mlogit(choice ~ 0 + seat + cargo + engine + price, #0 drops the intercept. p373-374. otherwise you get intercepts for each alternative which just indicates if they always picked left right or middle
                      data = data_mlogit)
summary(model_mlogit)

model_mlogit_3 = mlogit(choice ~ 0 + seat + cargo + engine + price_numeric, #0 drops the intercept. p373-374. otherwise you get intercepts for each alternative which just indicates if they always picked left right or middle
                      data = data_mlogit) #needed to calc willingness to pay. also has fewer parameters
summary(model_mlogit_3)
coef(model_mlogit_3)["cargo3ft"]/(-coef(model_mlogit_3)["price_numeric"]/1000) #div 1k since orig data in thousands


#no predict function for mlogit, so text wrote their own:
predict.mlogit = function(model,data){
  #model needs to be mlogit object
  #data needs to be same format as data used for model specification
  data = data %>% 
    mutate(price_numeric = as.numeric(as.character(price)))
  
  data_matrix = model.matrix(update(model$formula, 0 ~ .),data = data)[,-1] #-1 drops intercept
  #0 ~ . just extracts the xvars out of the model object. I would do this by having an xvar object at top of script
  
  utility = data_matrix %*% model$coef
  share = exp(utility)/sum(exp(utility)) #convert to prob that add up to 100% across all alternatives
  cbind(share,data)
  }

new_data = expand.grid(attributes)[c(8,1,3,41,49,26),]
# debugonce(predict.mlogit)
predict.mlogit(model_mlogit_3,new_data)



#sensitivity plots
#see how share would change (assuming competitor set fixed) if you change attributes one at a time

#loop through all attribute leves, compute share prediction, save prediction
sensitivity.mlogit = function(model,attributes, base_data, competitor_data){
  base_share = predict.mlogit(model,rbind(base_data,competitor_data))[1,1]
  
  share = NULL
  for (a in 1:length(attributes)){
    for (i in attributes[[a]]){ #each level of seats, price, engine etc.
      # data[1,] = base_data #reset first row back to orig data
      base_data_mod = base_data 
      base_data_mod[,a] = i #change column of base_data to a different level of attribute
      share = c(share,predict.mlogit(model,rbind(base_data_mod,competitor_data))[1,1]) #[1,1] gets share prediction for just base_data_mod
    }
  }
  data.frame(level = unlist(attributes),share = share, increase = share - base_share)
}

base_data = expand.grid(attributes)[c(8),]
competitor_data = expand.grid(attributes)[c(1,3,41,49,26),]
# debugonce(sensitivity.mlogit)
tradeoff = sensitivity.mlogit(model_mlogit,attributes,base_data,competitor_data)
tradeoff

#not sure why this doesn't match book, especially for electric engines
barplot(tradeoff$increase,horiz=F,names.arg = tradeoff$level, ylab = "Change in Share for Baseline Product")













