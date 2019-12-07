##############################################################################

# model selection for each impact variable
bestglm_list <- target_list %>% 
  mutate(data = target %>% 
           map(function(target_nm) makedata_map(target_nm,
                                                dat= dat, 
                                                X = resource))) %>% 
  mutate(rowdata = data %>% map_dbl(nrow)) %>% 
  filter(rowdata > 100) %>% 
  select(-rowdata)

# test:
data <- bestglm_list$data[[1]]; data %>% as_tibble()
family = "gaussian"
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
##############################################################################
