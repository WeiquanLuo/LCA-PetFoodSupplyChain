# set wd
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()

library(dplyr)
library(purrr)

# data input
dat <- read.csv("data/dat_311111_1M_v2.csv")
colnames(dat)

# outlier removal
dat[which(dat$X %in% c(16, 78, 88)),]
dat <- dat[-which(dat$X %in% c(16, 78, 88)),]
dat$X
dat %>% tibble()

# target columns 
ECO <- dat %>% select(Total.Economic.mill.dollar, Total.value.Added.mill.dollar, 
                      Employee.Com.mill.dollar, Net.Tax.mill.dollar,
                      Profits.mill.dollar, Direct.Economic.mill.dollar,Direct.Economic)
CPA <- dat %>% select(CO.t, NH3.t, NOx.t, PM10.t, PM2.5.t, SO2.t, VOC.t)
GHG <- dat %>% select(Total.t.CO2e, CO2.Fossil.t.CO2e, CO2.Process.t.CO2e, CH4.t.CO2e, HFC.PFCs.t.CO2e)
TOX <- dat %>% select(Fugitive.kg, Stack.kg, Total.Air.kg, Surface.water.kg, U_ground.Water.kg, Land.kg, Offiste.kg, POTW.Metal.kg)
target_list <- tibble(target = c(colnames(CPA),colnames(GHG),colnames(TOX), colnames(ECO))); target_list

# test: target_nm = "CO.t"
makedata_map <- function(target_nm, dat){
  # Input columns
  Xy = dat %>%
    select(Coal.TJ, NatGase.TJ, Petrol.TJ, 
           Bio.Waste.TJ, NonFossElec.TJ, 
           Water.Withdrawals.Kgal,
           target_nm)
  # retin all inf by log(x + min/100)
  #Xy <- cbind(dat %>% select(Sector) %>% mutate(Sector= Sector %>% as.factor()), log10(Xy + min(Xy[Xy!=0])/100)) 
  # remove all inf= log(0)
  Xy <- cbind(dat %>% select(Sector), log(Xy+1)) 
  Xy <- Xy[!is.infinite(rowSums(Xy)),] %>% mutate(Sector= Sector %>% as.factor())
  colnames(Xy) <- colnames(Xy) %>% stringr::str_replace_all("\\.","") 
  return(Xy)
}

#######################################
bestglm_list <- target_list %>% 
  mutate(data = target %>% 
           map(function(target_nm) makedata_map(target_nm,
                                                dat= dat))) %>% 
  mutate(rowdata = data %>% map_dbl(nrow)) %>% 
  filter(rowdata > 100) %>% 
  select(-rowdata) 

data <- bestglm_list$data[[1]]
# bestglm_map <- function(data, ...)
data <- data %>% select_if(is.numeric)

bestglm::bestglm(Xy=data, 
                 family = gaussian, 
                 method = "exhaustive",
                 IC = "BIC",
                 TopModels = 1)
  

#######################################

bestglm_list <- target_list %>% 
  mutate(data = target %>% 
           map(function(target_nm) makedata_map(target_nm,
                                                dat= dat))) %>% 
  mutate(rowdata = data %>% map_dbl(nrow)) %>% 
  filter(rowdata > 100) %>% 
  select(-rowdata) %>% 
  mutate(top_model = data %>% 
           map(function(data) bestglm::bestglm(Xy=data %>% select_if(is.numeric), family = gaussian, method = "exhaustive", IC = "BIC", TopModels = 1))) %>% 
  mutate(best_model = top_model %>% map(function(top_model) top_model[[1]])) %>% 
  mutate(anv = best_model %>% map(anova)) %>% 
  mutate(statisics = best_model %>% purrr::map(.f = function(m) broom::glance(m))) %>% 
  tidyr::unnest(statisics)
