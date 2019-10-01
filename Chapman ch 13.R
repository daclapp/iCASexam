
if (!require("pacman")) install.packages("pacman")
pacman::p_load(MASS)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

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
  choice = apply(probs,1,function(x) sample(1:num_alternatives,size = 1, prob = x)) #1 means row wise apply. random choice for which alternative they will choose. WHY AREN'T WE USING THE PROBS ABOVE TO PICK THIS?
  
  }













