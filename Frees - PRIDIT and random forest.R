
if (!require("pacman")) install.packages("pacman")
pacman::p_load(princomp)
source("load_packages.R") #load these last so overwrite functions other packages (like dplyr::select)

#re-write pridit code saved in "source code/Frees - PRIDIT - BOOK.R"

pip_data = read.csv("data/SimPIP.csv",header=TRUE)
glimpse(pip_data)
table(pip_data$Suspicion,pip_data$legalrep)

pip_data_xvars = pip_data %>% 
  select(-ID,-Suspicion)


apply_ridit = function(data) {
  # data = pip_data_xvars$legalrep #testing
  ridits = as.data.frame(data) %>%  #so we can use dplyr lag()
    count(data) %>% 
    mutate(cumsum_prior = lag(cumsum(n),1)) %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% #to fix first entry that gets NA for lag(,-1)
    mutate(ridit_scaled = 2*(n*.5/sum(n)+ cumsum_prior/sum(n))-1) #matches sample code for legalrep!!
#join ridit back onto data.frame to get vector of length 1500  
  out = as.data.frame(data) %>% 
    left_join(ridits, by = "data") #"data" is the name of the vector that is the default column name it gives when converting from vector to data.frame
  return(out %>% select(ridit_scaled) %>% pull)
}

#debugonce(apply_ridit)
pip_data_xvars_ridit = pip_data_xvars %>% 
  mutate_all(apply_ridit) 

 #check that this matches riditsheet in source code using hash values
