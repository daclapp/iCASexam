
if (!require("pacman")) install.packages("pacman")
pacman::p_load(lme4,arm)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

# see p. 568 for more examples of code

library(readr)

#Minnesota radon data: http://www.stat.columbia.edu/~gelman/arm/
county <- read_csv("data/Gelman/radon/cty.dat")
srrs2 <- read_csv("data/Gelman/radon/srrs2.dat")

df = srrs2 %>% 
  filter(state == "MN") %>% 
  rename(radon = activity) %>% 
  mutate(radon_log = log(if_else(radon==0, .1, radon)),
         stfips = as.numeric(stfips),
         cntyfips = as.numeric(cntyfips)) 

county = county %>% 
  mutate(stfips = as.numeric(stfips),
         ctfips = as.numeric(ctfips))
  
combined = df %>% 
  left_join(county,by = c("stfips" = "stfips","cntyfips" ="ctfips")) %>% 
  mutate(uranium_log = log(Uppm))


model_0 = lmer(radon_log ~ 1 + (1|county),data = df) # varying intercept, no predictors
model_0 #display(model) from arm pacage only adds AIC, DIC and deviance

model_1 = lmer(radon_log ~ floor + (1|county),data = df) # varying-intercept with one predictor. same slope
model_1



fixef(model_1) #extract just fixed effects from summary

coef(model_1) #can see equal slopes in floor column
ranef(model_1) #add this to overall intercept from coef() to get the coef for that county

# book has code with custom fuction for calculating confidence intervals

model_2 = lmer(radon_log ~ floor + uranium_log + (1|county),data = combined)
display(model_2)

#predictive simulation for new obs in existing group. p. 273
x_tilde = 1
sigma_y_hat = sigma.hat(model_2)$sigma$data
coef_hat = as.matrix(coef(model_2)$county)[26,] #Hennepin county
uranium_log_hennepin = log(county %>% filter(cty == "HENNEPIN") %>% pull(Uppm))

y_tilde = exp(rnorm(1000,coef_hat %*% c(1,x_tilde,uranium_log_hennepin),sigma_y_hat)) # random predictions given mean and variance
summary(y_tilde)
hist(y_tilde)

#predictive simulation for new obs in NEW group. p. 274
#assume new county has uranium levels equal to global mean

uranium_log_tilde
intercept = fixef(model_2)["Intercept"]
g1 = fixef(model_2)["uranium_log"]
sigma_a_hat = sigma.hat(model_2)$sigma$county

#not working below. rebuild code in 12.6_group-level predictors.R if I have time to get u. Not sure what that is doing
# a_tilde = rnorm(1000,intercept + g1*mean(log(county$Uppm))) #simulate intercpets for the new county

#chapter 13
#varying intercept AND SLOPE
model_3 = lmer(radon_log ~ floor + (1+floor|county),data = combined)
display(model_3)
coef(model_3) #combines fixef() and ranef()






