if (!require("pacman")) install.packages("pacman")
pacman::p_load(ISLR,readxl,lme4)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

data = read_excel("study note - buhlman credibility.xlsx", 
                     sheet = "raw_data", range = "A3:D15")


model_lmer = lmer(average_cost ~ 1 + (1|territory),data = data, weights = risk_count) #1 + doesn't seem to change the output

summary(model_lmer)
# Residual - Variance column similar to buhlmann number in excel
# fixed estimate similar to mu hat new.
ranef(model_lmer) #+intercept of 977.4 matches Best Linear Unbiased Predictions
















