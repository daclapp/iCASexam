

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ISLR)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

wage = ISLR::Wage
wage

model_lm = lm(wage~poly(age,4,raw=T),
              data = wage)
coef(summary(model_lm))

age_grid = seq(from = min(wage$age), to = max(wage$age))
preds = predict(model_lm,newdata = list(age=age_grid),se=T)
se_bands = cbind(preds$fit + 2*preds$se.fit,
                 preds$fit - 2*preds$se.fit)

plot(wage$age,wage$wage,xlim=range(wage$age))
lines(age_grid,preds$fit,lwd=2,col="blue")
matlines(age_grid,se_bands,lwd=1,col="blue",lty=3)

model_lm_1 = lm(wage~age,data = wage)
model_lm_2 = lm(wage~poly(age,2),data = wage)
model_lm_3 = lm(wage~poly(age,3),data = wage)
model_lm_4 = lm(wage~poly(age,4),data = wage)
model_lm_5 = lm(wage~poly(age,5),data = wage)

#two ways to test which degree to use:
anova(model_lm_1,model_lm_2,model_lm_3,model_lm_4,model_lm_5)
coef(summary(model_lm_5)) #doesn't require coding out each fit. but doesn't work when other variables in the model
#could also choose using cross validation

wage = wage %>% 
  mutate(high_earner = if_else(wage>250,1,0))

model_glm = glm(high_earner ~ poly(age,4),
                data=wage,
                family=binomial)
summary(model_glm)

age_data = as.data.frame(age_grid) %>% 
  rename(age = age_grid)

age_data_augment = age_data %>% 
  mutate(preds = predict(model_glm,newdata = age_data,type="response"))
#can't use SEs out of predict when using type="response" though. need to calc directly

plot(wage$age,wage$high_earner,ylim=c(0,.2))
lines(age_data_augment$age,age_data_augment$preds)

#fit step function




















