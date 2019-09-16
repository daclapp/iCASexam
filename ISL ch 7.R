

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ISLR,gam,locfit,splines,akima)
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

plot(wage$age,wage$high_earner,ylim=c(0,.2)) #text modifies this plot further
lines(age_data_augment$age,age_data_augment$preds)

#splines
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage) #bs() is B-Spline
pred=predict(fit,newdata=list(age=age_grid),se=T)

plot(wage$age,wage$wage,col="gray")
lines(age_grid,pred$fit,lwd=2)
lines(age_grid,pred$fit+2*pred$se,lty="dashed")
lines(age_grid,pred$fit-2*pred$se,lty="dashed")

dim(bs(age,knots=c(25,40,60))) #specifyintervals
dim(bs(age,df=6))#knots as uniform intervals
attr(bs(age,df=6),"knots")

fit2=lm(wage~ns(age,df=4),data=Wage) #ns() is natural spline
pred2=predict(fit2,newdata=list(age=age_grid),se=T)
lines(age_grid, pred2$fit,col="red",lwd=2) #adds line to plot up a few lines


plot(wage$age,wage$wage,xlim=range(wage$age),cex=.5,col="darkgrey")
title("Smoothing Spline") 
fit=smooth.spline(wage$age,wage$wage,df=16)
fit2=smooth.spline(wage$age,wage$wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

plot(wage$age,wage$wage,xlim=range(wage$age),cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age_grid,predict(fit,data.frame(age=age_grid)),col="red",lwd=2)
lines(age_grid,predict(fit2,data.frame(age=age_grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)




#gams

par(mfrow=c(1,3))

gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage) #without using gam package. same as ns() above for one var. limited though
plot.Gam(gam1, se=TRUE, col="red") #can use generic plot() as well like above
title("Natural Spines")

gam.m1=gam(wage~          s(age,5)+education,data=Wage)
gam.m2=gam(wage~ year    +s(age,5)+education,data=Wage) #add year
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage) #s() is smoothing spline. similar to smooth.spline() above
plot(gam.m3, se=TRUE,col="blue")
title("Smoothing Spines")


anova(gam.m1,gam.m2,gam.m3,test="F")
summary(gam.m3)

preds=predict(gam.m2,newdata=ISLR::Wage)

gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage) #lo() is local regression
plot(gam.lo, se=TRUE, col="green")

gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage) #fit interaction between year and age

# library(akima)
plot(gam.lo.i)
gam.lr=gam(high_earner~year+s(age,df=5)+education,family=binomial,data=wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

table(wage$education,wage$high_earner)
#no highe earers less than HS so refit excluding so plot looks better:
gam.lr.s=gam(high_earner~year+s(age,df=5)+education,family=binomial,data=wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")

















