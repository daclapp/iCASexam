
source("load_packages.R")
pacman::p_load(car,MASS) # had to run this in my linux terminal: sudo apt-get install libcurl4-openssl-dev libssl-dev 

glimpse(Transact)

model_gamma = glm(time ~ t1 + t2,
                  family = Gamma(link = identity),
                  data = Transact)
summary(model_gamma)

gamma.shape(model_gamma)

mod.ornstein = glm(interlocks  ~ log2(assets) + nation + sector,family = "poisson", data = Ornstein)
summary(mod.ornstein) #from other fox script


phihat = sum(residuals(mod.ornstein, type = "pearson")^2)/df.residual(mod.ornstein) #usual estimator for the dispersion parameter. 

summary(mod.ornstein,dispersion = phihat)

Anova(mod.ornstein, test = "F")

#produces identical output:
update(mod.ornstein,family = quasipoisson)


#negative binomial regression

mod.ornstein.neg_binomial = update(mod.ornstein,family = negative.binomial(1.5))

#test thetas by minimizing in sample AIC. Should be cross validated instead
thetas = seq(.5,2.5,by=.5)
aics = rep(0,5) #create empty vector
for (i in 1:length(thetas)){
  aics[i] = AIC(update(mod.ornstein.neg_binomial,family = negative.binomial(thetas[i])))
}

rbind(thetas,aics)
#min AIC is at theta = 1.5, orig value picked

summary(mod.ornstein.neg_binomial)

#alternatively estimate theta with the coefs. not a traditional GLM though. produces 1.639 theta which is close to the grid search value of 1.5
summary(glm.nb(interlocks ~ log2(assets) + nation + sector, data = Ornstein)) #don't need to specify family as glm.nb specific to negative binomial

#roll your own glm


#6.6 diagnostic plots for glms
glimpse(Womenlf)
model = glm(partic != "not.work" ~ hincome + children,
            family = "binomial", data = Womenlf)

summary(model)

residualPlots(model,layout=c(1,3))

#cooks distance and hat values
influenceIndexPlot(model,vars=c("Cook","hat"),id.n=3)

mod.ornstein.poisson = glm(interlocks ~ assets + nation + sector,
                           family = poisson, data = Ornstein)
crPlots(mod.ornstein.poisson, "assets") #fig 6.19. hard to read. logging assets produces 6.20 




















