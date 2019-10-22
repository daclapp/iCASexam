
if (!require("pacman")) install.packages("pacman")
pacman::p_load(lme4,arm)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

# see p. 568 for more examples of code

library(readr)

#Minnesota radon data: http://www.stat.columbia.edu/~gelman/arm/
city <- read_csv("data/Gelman/radon/cty.dat")
srrs2 <- read_csv("data/Gelman/radon/srrs2.dat")

df = srrs2 %>% 
  filter(state == "MN") %>% 
  rename(radon = activity) %>% 
  mutate(radon_log = log(if_else (radon==0, .1, radon))) 

model_0 = lmer(radon_log ~ 1 + (1|county),data = df) # varying intercept, no predictors
model_0 #display(model) from arm pacage only adds AIC, DIC and deviance

model_1 = lmer(radon_log ~ floor + (1|county),data = df) # varying-intercept with one predictor. same slope
model_1

coef(model_1) #can see equal slopes in floor column
















