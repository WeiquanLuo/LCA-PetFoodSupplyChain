# set wd
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()

library(dplyr)
library(leaps) 
library(purrr)
# print.regsub.R
# Obtain from STAT 401 Dr.Philip Dixon 
print.regsub <- function(l, sort='BIC', best=NULL) {
  # function written by PMD, 12 April 2015
  # print a table with model selection stats 
  #   based on information produced by summary.regsubsets()
  # l is an object returned by summary() of a regsubsets() result
  # sort is a character string with the variable to sort by
  #   must be one of the names in the print.regsub() output
  # best is the number of results to print, NULL prints all
  
  var <- apply(l$which, 1, function(x){
    paste(l$obj$xnames[x][-1],collapse=' ' )})
  nvar <- apply(l$which[,-1], 1, sum)
  
  aic <- l$bic - log(l$obj$nn)*nvar + 2*nvar
  
  temp <- data.frame(var=var, nvar=nvar, Rsq=l$rsq, AdjRsq=l$adjr2, 
                     Cp=l$cp, AIC = aic, BIC=l$bic)
  o <- order(temp[,sort])
  if (!is.null(best)) {
    o <- o[1:best] 
  }
  temp[o,]
}

# data input
dat <- read.csv("data/dat_311111_1M_v2.csv")
colnames(dat)

# outlier removal
dat <- dat %>% filter(X != 17)

# target columns 
CPA <- dat %>% select(CO.t, NH3.t, NOx.t, PM10.t, PM2.5.t, SO2.t, VOC.t)
GHG <- dat %>% select(Total.t.CO2e, CO2.Fossil.t.CO2e, CO2.Process.t.CO2e, CH4.t.CO2e, HFC.PFCs.t.CO2e)
TOX <- dat %>% select(Fugitive.kg, Stack.kg, Total.Air.kg, Surface.water.kg, U.ground.Water.kg, Land.kg, Offiste.kg, POTW.Metal.kg)
target_list <- tibble(target = c(colnames(CPA),colnames(GHG),colnames(TOX))); target_list

# single example
lm_y <- regsubsets(NH3.t ~ Sector + Coal.TJ + NatGase.TJ + Petrol.TJ + Bio.Waste.TJ + NonFossElec.TJ + Water.Withdrawals.Kgal, data = dat, method = "exhaustive", nbest = 5); lm.CPA_CO.t %>% summary()
print.regsub(lm_y %>% summary, sort='AIC', best=1)

# write a function can iterate regsubset()
# test: target_nm = "CO.t"
regsubsets_map <- function(target_nm, dat){
  # Input columns
  X <- dat %>% select(Sector, Coal.TJ, NatGase.TJ, Petrol.TJ, Bio.Waste.TJ, NonFossElec.TJ, Water.Withdrawals.Kgal)
  # targe columns
  y <- dat %>% select(target_nm)
  lm_y <- regsubsets(as.matrix(X), y[,1], method = "exhaustive", nbest = 1)
  }

regsubsets_list <- target_list %>% 
  mutate(top_model = target %>% 
           map(function(target_nm) regsubsets_map(target_nm = target_nm,
                                                  dat= dat))) %>% 
  mutate(best_model = top_model %>% 
           map(function(top_model) print.regsub(l = top_model %>% summary, 
                                            sort='AIC', 
                                            best=1))) %>% 
  tidyr::unnest(best_model) %>% 
  mutate(var = var %>% as.character()); regsubsets_list

hist(regsubsets_list$Rsq)

fit_model <- function(data, dat){
  f <- paste(data$target, data$var %>% stringr::str_replace_all(" ", " + "), sep = " ~ ")
  lm_y <- lm(f, data = dat)
  return(lm_y) 
}


lm_list <- regsubsets_list %>% select(c(target, var)) %>% 
  tibble::rowid_to_column("index") %>% 
  tidyr::nest(data = c(target, var)) %>% 
  mutate(lm = data %>% map(function(data) fit_model(data = data, 
                                                              dat = dat))) %>% 
  tidyr::unnest(data) %>% 
  mutate(anv = lm %>% map(anova)) %>% 
  mutate(statisics = lm %>% purrr::map(.f = function(m) broom::glance(m))) %>% 
  tidyr::unnest(statisics); lm_list

regsubsets_list$var

find_var_list <- function(regsubsets_list){
  vars <- regsubsets_list$var
  var_nm <- vars %>% stringr::str_split(" ") %>% unlist %>% unique()
  var_list <- purrr::map_dfc(vars, function(var) stringr::str_detect(string = var, pattern = var_nm))
  colnames(var_list) <- regsubsets_list$target
  var_list <- var_list %>% t
  colnames(var_list) <- var_nm
  var_list[var_list == 0] <- NA
  return(var_list)
}
var_list <- regsubsets_list %>% find_var_list

# result
regsubsets_list # result of model selection
lm_list # result of selected linear model
var_list # result of parameter placeholder






