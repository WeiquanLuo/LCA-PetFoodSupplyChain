# set wd
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()

library(dplyr)
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
dat <- dat %>% filter(X != 5)
dat <- dat %>% filter(X != 8)
dat <- dat %>% filter(X != 15)
dat <- dat %>% filter(X != 16)
dat <- dat %>% filter(X != 17)
dat <- dat %>% filter(X != 19)
#dat <- dat %>% mutate(Sector = Sector %>% as.factor())
dat %>% tibble()

plot(dat$Sector, dat$NH3.t)

# target columns 
CPA <- dat %>% select(CO.t, NH3.t, NOx.t, PM10.t, PM2.5.t, SO2.t, VOC.t)
GHG <- dat %>% select(Total.t.CO2e, CO2.Fossil.t.CO2e, CO2.Process.t.CO2e, CH4.t.CO2e, HFC.PFCs.t.CO2e)
TOX <- dat %>% select(Fugitive.kg, Stack.kg, Total.Air.kg, Surface.water.kg, U.ground.Water.kg, Land.kg, Offiste.kg, POTW.Metal.kg)
target_list <- tibble(target = c(colnames(CPA),colnames(GHG),colnames(TOX))); target_list

# single example
lm_y <- leaps::regsubsets(CO.t ~ Sector + Coal.TJ + NatGase.TJ + Petrol.TJ + Bio.Waste.TJ + NonFossElec.TJ + Water.Withdrawals.Kgal, data = dat, method = "exhaustive", nbest = 5)
print.regsub(lm_y %>% summary, sort='AIC', best=1)

# write a function can iterate regsubset()
# test: target_nm = "CO.t"； regsubsets_map(target_nm, dat = dat)
regsubsets_map <- function(target_nm, dat){
  # Input columns
  X <- dat %>%
    select(Coal.TJ, NatGase.TJ, Petrol.TJ, Bio.Waste.TJ, NonFossElec.TJ, Water.Withdrawals.Kgal) %>% 
    as.matrix()
  trt <- dat %>% select(Sector) %>% mutate(Sector = Sector %>% as.factor())
  # targe columns
  y <- dat %>% select(target_nm)
  lm_y <- leaps::regsubsets(x=X, y=y[,1], method = "exhaustive", nbest = 1)
  }


fit_model <- function(data, dat){
  f <- paste(data$target, data$var %>% stringr::str_replace_all(" ", " + "), sep = " ~ ")
  lm_y <- lm(f, data = dat)
  return(lm_y) 
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
plan(sequential) # back to sequential processing

lm_list <- regsubsets_list %>% select(c(target, var)) %>% 
  tibble::rowid_to_column("index") %>% 
  tidyr::nest(data = c(target, var)) %>% 
  mutate(model = data %>% map(function(data) fit_model(data = data, 
                                                              dat = dat))) %>% 
  tidyr::unnest(data) %>% 
  mutate(anv = model %>% map(anova)) %>% 
  mutate(statisics = model %>% purrr::map(.f = function(m) broom::glance(m))) %>% 
  tidyr::unnest(statisics)

coef_list <- lm_list %>% 
  mutate(coefs = model %>% purrr::map(.f=broom::tidy)) %>% 
  select(target, coefs) %>% 
  tidyr::unnest(coefs) %>% 
  select(target, term, estimate) %>% 
  tidyr::spread(key= term, value = estimate)

signif_list <- lm_list %>% 
  mutate(coefs = model %>% purrr::map(.f=broom::tidy)) %>% 
  select(target, coefs) %>% 
  tidyr::unnest(coefs) %>% 
  select(target, term, p.value) %>% 
  tidyr::spread(key= term, value = p.value)


bind_coef_star <- function(x) {
  if (stringr::str_detect(x[2] , "\\*")) {
    paste0(x[1], "(",x[2], ")")
  } else if (!is.na(x[1])){
    paste0(x[1])
  } else{
    ""
  }
}

datArray <- abind::abind(coef_list %>% 
                    select(-target) %>% 
                    mutate_if(is.numeric, signif, digits = 3) %>% 
                    mutate_all(as.character),
                  signif_list %>% 
                    select(-target) %>% 
                    mutate_if(is.numeric, gtools::stars.pval),along=3)
coef_signif_list <- cbind(coef_list$target,
                         apply(datArray,1:2, bind_coef_star) %>% as_tibble())

# result
regsubsets_list # result of model selection
lm_list # result of selected linear model
coef_list # result of coef placeholder
signif_list # result of signif of coefs placeholder
coef_signif_list # aggregation view of coef and signif

