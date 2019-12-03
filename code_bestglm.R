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
dat[which(dat$X %in% c(16,78)),]

#dat <- dat[-which(dat$X %in% c(16,78)),]
dat$X
dat %>% tibble()

# target columns 
CPA <- dat %>% select(CO.t, NH3.t, NOx.t, PM10.t, PM2.5.t, SO2.t, VOC.t)
GHG <- dat %>% select(Total.t.CO2e, CO2.Fossil.t.CO2e, CO2.Process.t.CO2e, CH4.t.CO2e, HFC.PFCs.t.CO2e)
TOX <- dat %>% select(Fugitive.kg, Stack.kg, Total.Air.kg, Surface.water.kg, U_ground.Water.kg, Land.kg, Offiste.kg, POTW.Metal.kg)
target_list <- tibble(target = c(colnames(CPA),colnames(GHG),colnames(TOX))); target_list

# single example
lm_y <- leaps::regsubsets(CO.t ~ Sector + Coal.TJ + NatGase.TJ + Petrol.TJ + Bio.Waste.TJ + NonFossElec.TJ + Water.Withdrawals.Kgal, data = dat, method = "exhaustive", nbest = 5)
print.regsub(lm_y %>% summary, sort='AIC', best=1)


# test: target_nm = "CO.t"
makedata_map <- function(target_nm, dat){
  # Input columns
  Xy = dat %>%
    select(Coal.TJ, NatGase.TJ, Petrol.TJ, 
           Bio.Waste.TJ, NonFossElec.TJ, 
           Water.Withdrawals.Kgal,
           target_nm)
  
  Xy <- log10(Xy) %>% filter_all(all_vars(!is.infinite(.)))
  colnames(Xy) <- colnames(Xy) %>% stringr::str_replace_all("\\.","") 
  return(Xy)
}

bestglm_list <- target_list %>% 
  mutate(data = target %>% 
           map(function(target_nm) makedata_map(target_nm = target_nm,
                                                dat= dat))) %>% 
  mutate(rowdata = data %>% map_dbl(nrow)) %>% 
  filter(rowdata > 100) %>% 
  select(-rowdata) %>% 
  mutate(top_model = data %>% 
           map(function(data) bestglm::bestglm(Xy=data, family = gaussian, method = "exhaustive", IC = "BIC", TopModels = 1))) %>% 
  mutate(best_model = top_model %>% map(function(top_model) top_model[[1]])) %>% 
  mutate(anv = best_model %>% map(anova)) %>% 
  mutate(statisics = best_model %>% purrr::map(.f = function(m) broom::glance(m))) %>% 
  tidyr::unnest(statisics)

coef_list <- bestglm_list %>% 
  mutate(coefs = best_model %>% purrr::map(.f=broom::tidy)) %>% 
  select(target, coefs) %>% 
  tidyr::unnest(coefs) %>% 
  select(target, term, estimate) %>% 
  tidyr::spread(key= term, value = estimate)

signif_list <- bestglm_list %>% 
  mutate(coefs = best_model %>% purrr::map(.f=broom::tidy)) %>% 
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
coef_signif_list <- bestglm_list %>% 
  select(target, r.squared, adj.r.squared, p.value) %>% 
  cbind(apply(datArray,1:2, bind_coef_star) %>% as_tibble())

# result
bestglm_list # result of model selection
coef_list # result of coef placeholder
signif_list # result of signif of coefs placeholder
coef_signif_list # aggregation view of coef and signif

good_lm <- bestglm_list %>% filter(adj.r.squared >0.75); good_lm
par(mfrow=c(2,3))
plot(good_lm$best_model[[1]], which=1:6)
plot(good_lm$best_model[[2]], which=1:6)
plot(good_lm$best_model[[3]], which=1:6)
plot(good_lm$best_model[[4]], which=1:6)
plot(good_lm$best_model[[5]], which=1:6)
plot(good_lm$best_model[[6]], which=1:6)
plot(good_lm$best_model[[7]], which=1:6)
dev.off()


plot(log(dat$CO.t), log10(dat$Coal.TJ))

plot(dat$Sector %>% as.factor(), log10(dat$PM2.5.t))
plot(dat$Sector %>% as.factor(), log10(dat$CO2.Fossil.t.CO2e))

# PM2.5.t: Emissions of Particulate Matter (less than 2.5 microns in diameter) to Air from each sector. Value is Primary PM. t = meric tons 
# CO2.Fossil.t.CO2e	C: Emissions of Carbon Dioxide (CO2) into the air from each sector from fossil fuel combustion sources. t CO2e = metric tons of CO2 equivalent.
