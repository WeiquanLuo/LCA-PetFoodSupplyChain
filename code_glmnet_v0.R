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
#dat <- dat[-which(dat$X %in% c(16, 78, 88)),]
dat$X
dat %>% tibble()

# zero value resut in a greater intercept in regression
# need to make sure the data are greater than 1 before log transform.
xy <- dat %>% select(Petrol.TJ, Coal.TJ) %>% as_tibble();xy
xy_1 <- xy[!is.infinite(rowSums(log(xy))),]; xy %>% dim
xy_0 <- xy[is.infinite(rowSums(log(xy))),]; xy %>% dim
xy_1[,1] <- xy_1[,1]*1000
xy_0[,1] <- xy_0[,1]*1000

plot(log(xy_1+1), type="p",col="blue", 
     xlab = 'Coal.GJ', ylab= "Petrol.TJ")
points(log(xy_0+1), type="p",col="red")


# calculate and rename
calculate_formula_replace_nm <- function(data, formula = y~1000*x, pattern= pattern, replacement= replacement){
  
  calculate_formula<- function(data, formula = formula){
    as.function <- function(formula) {
      cmd <- tail(as.character(formula),1)
      exp <- parse(text=cmd)
      function(...) eval(exp, list(...))
    }
    formula.function <- as.function(formula)
    result<- formula.function(x=data)
    return(result)
  }
  data <- data %>% dplyr::mutate_all(calculate_formula, formula = formula) %>% 
    stats::setNames(stringr::str_replace_all(names(.), pattern= pattern, replacement= replacement )) 
  return(data)
}


# Sort environment impact group and input resouce; unit convertion to result no 0<.<1; rename by unit
CPA <- dat %>% select(CO.t, NH3.t, NOx.t, PM10.t, PM2.5.t, SO2.t, VOC.t) %>% 
  calculate_formula_replace_nm(formula = y~x*10^6, pattern= "\\.t", replacement= ".g")
GHG <- dat %>% select(Total.t.CO2e, CO2.Fossil.t.CO2e, CO2.Process.t.CO2e, CH4.t.CO2e, HFC.PFCs.t.CO2e) %>% 
  calculate_formula_replace_nm(formula = y~x*10^6, pattern= "\\.t", replacement= ".g")
TOX <- dat %>% select(Fugitive.kg, Stack.kg, Total.Air.kg, Surface.water.kg, U_ground.Water.kg, Land.kg, Offiste.kg, POTW.Metal.kg) %>% 
  calculate_formula_replace_nm(formula = y~x*10^6, pattern= "\\.kg", replacement= ".mg")
resource <- dat %>% select(Coal.TJ, NatGase.TJ, Petrol.TJ,Bio.Waste.TJ, NonFossElec.TJ, Water.Withdrawals.Kgal) %>% 
  calculate_formula_replace_nm(formula = y~x*10^6, pattern= "\\.TJ", replacement= ".MJ")
ID <- dat %>% select(Sector, Description, name_sub, Sector_sub) %>% mutate_all(as.factor)
dat <- cbind(ID, CPA, GHG, TOX, resource)

# test: target_nm = "CO.g",  X = resource
makedata_map <- function(target_nm, dat, X){
  # loglog transform
  y <- dat %>% select(target_nm)
  Lny <- log(y) %>% stats::setNames(paste0("Ln", names(.)))
  LnX <- log(X+1) %>% stats::setNames(paste0("Ln", names(.)))
  # combine, filter log(y)=0; add ID:Sector
  Xy <- cbind(LnX, Lny)
  Xy <- cbind(dat %>% select(Sector), Xy) 
  Xy <- Xy[!is.infinite(rowSums(Lny)),]
  colnames(Xy) <- colnames(Xy) %>% stringr::str_replace_all("\\.","") 
  return(Xy)
}
# bestglm
fit_model <- function(data){
  best_model <- bestglm::bestglm(Xy=data %>% 
                                   select_if(is.numeric), 
                                 family = gaussian, 
                                 method = "exhaustive", 
                                 IC = "AIC", 
                                 TopModels = 1)
  return(best_model[[1]])
}
bind_coef_star <- function(x) {
  if (stringr::str_detect(x[2] , "\\*")) {
    paste0(x[1], "(",x[2], ")")
  } else if (!is.na(x[1])){
    paste0(x[1])
  } else{
    ""
  }
}

# test: model <- bestglm_list$best_model[[1]], null <- 1
waldtest_map <- function(model, null= NULL){
  test.terms <- paste0("~", names(coef(model))[-1] %>% paste(collapse = "+")) %>% 
    as.formula()
  test_result <- survey::regTermTest(model = model, test.terms ,null = null)
  pval_wald <- test_result[['p']] %>% as.numeric()
  return(pval_wald)
}

target_list <- tibble(target = c(colnames(CPA),colnames(GHG),colnames(TOX))); target_list

# test:
# data <- bestglm_list$data[[9]]; data %>% as_tibble()
# family = "gaussian"
# glmnet
fit_model <- function(data, family = "gaussian"){
  id <- data %>% select_if(is.factor)
  Xy <- data %>% select_if(is.numeric)
  y <- Xy[,ncol(Xy)]
  X <- Xy[,-ncol(Xy)]
  best_model <- glmnet::cv.glmnet(x = X %>% as.matrix(), y =y, family = family, alpha = 1)
  # plot(best_model) 
  # best_model$lambda.min
  # coef(best_model, s = "lambda.min") # lambda.min give the minimum mean cross-validated error to the model
  return(best_model)
}
# model selection for each impact variable
bestglm_list <- target_list %>% 
  mutate(data = target %>% 
           map(function(target_nm) makedata_map(target_nm,
                                                dat= dat, 
                                                X = resource))) %>% 
  mutate(rowdata = data %>% map_dbl(nrow)) %>% 
  filter(rowdata > 100) %>% 
  select(-rowdata) %>% 
  mutate(best_model = data %>% map(function(data) fit_model(data = data, family = "gaussian"))) %>% 
  mutate(lambda.min = best_model %>% purrr::map_dbl(.f = function(m) m$lambda.min))

coef_list <- bestglm_list %>% 
  mutate(coefs = best_model %>% purrr::map(function(best_model) coef(best_model, s = "lambda.min") %>% as.matrix %>% t %>% as.data.frame)) %>% 
  select(target, coefs) %>% 
  tidyr::unnest(coefs) 

# Result
bestglm_list
coef_list