#test linux

if (!require("pacman")) install.packages("pacman")
pacman::p_load(cem)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

#3.1 not on syllabus
data("LeLonde")
Le <- data.frame(na.omit(LeLonde))

# mean(LeLonde %>% filter(treated == 1) %>% pull(re78),na.rm=T) - mean(LeLonde %>% filter(treated == 0) %>% pull(re78),na.rm=T)
# 
# vars <- c("age", "education", "black", "married", "nodegree",
#             "re74", "re75", "hispanic", "u74", "u75", "q1")


#Automated coarsening
mat <- cem(treatment = "treated", data = Le, drop = "re78")
mat #in paper this prints out more 

mat$w # weights for use in the evaluation of imbalance measures and estimates of the causal effect
levels(Le$q1)
q1_grp <- list(c("strongly agree", "agree"), c("neutral", "no opinion"),
               c("strongly disagree", "disagree"))

table(Le$education)
educut = c(0,6.5,8.5,12.5,17)
mat1 = cem(treatment = "treated", data = Le, drop = "re78",
           cutpoints = list(education = educut, grouping = list(q1_grp)))


