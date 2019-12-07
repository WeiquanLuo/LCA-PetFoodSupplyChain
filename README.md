
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Interesting question

The goal of this project is to study the environmental impact of a
certain amount of production with Economic Input-Output Life Cycle
Assessment (EIO-LCA) method, which estimates activities in our economy
in the materials and energy resources required for and the environmental
impact resulting from. The environmental impacts involove conventional
air poluten (CAP), greenhouse gass (GHG), and toix release (TOX).
Cradle-to-grave is the full Life Cycle Assessment from resource
extraction to use phase and disposal phase. Specificallym, this analysis
is base on the Cradle-to-grave EIO-LCA result to further understand how
all industrial stages of producing Million Dollars product *in Dog and
Cat Food Manufacturing* (code 311111 in NAICS 2002) are different in
environmental impact. The study aim to answer the following questions:

  - which industry(s) have larger impact among all industries?
  - what are the relationship between some impact relative to the input
    (ie. Energy, water withdraw)?
  - how the outlier industry(s) behave in linear regression models

## Hightlight

1.  for dog and cat food Manufacturing, the greatest environmental
    impacts come frome any raw material production industries such as
    agricultural farming.

2.  we successfully fit linear regression model on the following
    environmental impact using energy and water withdraw as input:

<!-- end list -->

  - Emissions of Nitrogen Oxides to Air,
  - Emissions of Sulfur Dioxide to Air,
  - weighting of greenhouse gas emissions into the air from the
    production,
  - Emissions of Carbon Dioxide (CO2) into the air from fossil fuel
    combus.

<!-- end list -->

3.  most of industries use either NonFossoil Eletrecity or Fossoil
    Eletrecity
4.  for those industries using biowaste as energy source have higher
    impact in toxic.

## Workflow

<center>

![LCA:Pet Food Supply
Chain](doc/LCA_%20Pet%20Food%20Supply%20Chain%20.png)

</center>

# Data description

The dataset for this project is the first pass life cycle assessment
results for cat and dog food manufacturing. It provides the
environmental impact information into the pet food supply chain. The LCA
data was generated through EIO-LCA
website(<http://www.eiolca.net/cgi-bin/dft/use.pl>). The model for
getting the LCA data is US 2002 producer price benchmark. In order to
get the dog and cat food manufacturing LCA data, the user needs to
select “Food beverage and tobacco” sector group, dog and cat food
manufacturing sector and then the amount of economic activity for this
sector(e.g. 1 millon dollars). After setting up the sector and economic
parameters, The user could select different economical and environmental
impact to get the LCA results. The LCA results are ready for downloading
as excel files. The raw data was lacking the columns name for different
impact feature and index for identifying the different sectors and
sector group. The web scraping is necessary for the columns name and
NAICS sector code.

# Explore the data

After webscaping, combinding the raw data, and manually making minor
modification, we result a datafarme stored as ’dat\_311111\_1M\_v2.csv.

``` r
# data input
dat <- read.csv("data/dat_311111_1M_v2.csv")
colnames(dat)
#>  [1] "X"                             "Sector"                       
#>  [3] "Description"                   "name_sub"                     
#>  [5] "Sector_sub"                    "Total.Economic.mill.dollar"   
#>  [7] "Total.value.Added.mill.dollar" "Employee.Com.mill.dollar"     
#>  [9] "Net.Tax.mill.dollar"           "Profits.mill.dollar"          
#> [11] "Direct.Economic.mill.dollar"   "Direct.Economic"              
#> [13] "CO.t"                          "NH3.t"                        
#> [15] "NOx.t"                         "PM10.t"                       
#> [17] "PM2.5.t"                       "SO2.t"                        
#> [19] "VOC.t"                         "Total.Energy.TJ"              
#> [21] "Coal.TJ"                       "NatGase.TJ"                   
#> [23] "Petrol.TJ"                     "Bio.Waste.TJ"                 
#> [25] "NonFossElec.TJ"                "Total.t.CO2e"                 
#> [27] "CO2.Fossil.t.CO2e"             "CO2.Process.t.CO2e"           
#> [29] "CH4.t.CO2e"                    "N2O.t.CO2e"                   
#> [31] "HFC.PFCs.t.CO2e"               "Fugitive.kg"                  
#> [33] "Stack.kg"                      "Total.Air.kg"                 
#> [35] "Surface.water.kg"              "U_ground.Water.kg"            
#> [37] "Land.kg"                       "Offiste.kg"                   
#> [39] "POTW.Metal.kg"                 "POTW.Nonmetal.kg"             
#> [41] "Water.Withdrawals.Kgal"
```

``` r
# zero value resut in a greater intercept in regression
# need to make sure the data are greater than 1 before log transform.
xy <- dat %>% select(Petrol.TJ, Coal.TJ) %>% as_tibble();xy
#> # A tibble: 402 x 2
#>    Petrol.TJ  Coal.TJ
#>        <dbl>    <dbl>
#>  1  0.0350   0.000091
#>  2  0.000572 0.000017
#>  3  0.00749  0.000044
#>  4  0.00186  0.000024
#>  5  0.0562   0       
#>  6  0.000718 0       
#>  7  0.00112  0       
#>  8  0.000176 0       
#>  9  0.000077 0.000002
#> 10  0.000001 0       
#> # … with 392 more rows
xy_1 <- xy[!is.infinite(rowSums(log(xy))),]; xy %>% dim
#> [1] 402   2
xy_0 <- xy[is.infinite(rowSums(log(xy))),]; xy %>% dim
#> [1] 402   2
xy_1[,1] <- xy_1[,1]*1000
xy_0[,1] <- xy_0[,1]*1000

plot(log(xy_1+1), type="p",col="blue", 
     xlab = 'Coal.GJ', ylab= "Petrol.TJ")
points(log(xy_0+1), type="p",col="red")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
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


# Sort environment impact group and input resouce; 
# unit convertion to result no 0<.<1; 
# rename by unit
CPA <- dat %>% select(CO.t, NH3.t, NOx.t, PM10.t, PM2.5.t, SO2.t, VOC.t) %>% 
  calculate_formula_replace_nm(formula = y~x*10^6, pattern= "\\.t", replacement= ".g")
GHG <- dat %>% select(Total.t.CO2e, CO2.Fossil.t.CO2e, CO2.Process.t.CO2e, CH4.t.CO2e, HFC.PFCs.t.CO2e) %>% 
  calculate_formula_replace_nm(formula = y~x*10^6, pattern= "\\.t", replacement= ".g")
TOX <- dat %>% select(Fugitive.kg, Stack.kg, Total.Air.kg, Surface.water.kg, U_ground.Water.kg, Land.kg, Offiste.kg, POTW.Metal.kg) %>% 
  calculate_formula_replace_nm(formula = y~x*10^6, pattern= "\\.kg", replacement= ".mg")
resource <- dat %>% select(Coal.TJ, NatGase.TJ, Petrol.TJ,Bio.Waste.TJ, NonFossElec.TJ, Water.Withdrawals.Kgal) %>% 
  calculate_formula_replace_nm(formula = y~x*10^6, pattern= "\\.TJ", replacement= ".MJ")
ID <- dat %>% select(Sector, Description, name_sub, Sector_sub)
dat <- cbind(ID, CPA, GHG, TOX, resource)

ys <- cbind(CPA, GHG, TOX)
psych::describe(ys) %>% knitr::kable(format = "markdown") 
```

|                    | vars |   n |         mean |           sd |  median |      trimmed |        mad | min |       max |     range |      skew |  kurtosis |           se |
| :----------------- | ---: | --: | -----------: | -----------: | ------: | -----------: | ---------: | --: | --------: | --------: | --------: | --------: | -----------: |
| CO.g               |    1 | 402 |   22214.7438 |   295207.762 |    38.5 | 6.645031e+02 |    57.0801 |   0 |   5888105 |   5888105 | 19.523839 | 384.79885 |   14723.6250 |
| NH3.g              |    2 | 402 |    9829.9876 |   138921.135 |     1.0 | 8.649068e+00 |     1.4826 |   0 |   2687352 |   2687352 | 18.084851 | 341.44049 |    6928.7564 |
| NOx.g              |    3 | 402 |    8035.9080 |    54044.815 |    28.0 | 3.363199e+02 |    41.5128 |   0 |    707607 |    707607 |  9.977676 | 106.60356 |    2695.5104 |
| PM10.g             |    4 | 402 |   17716.5000 |   285050.372 |     5.5 | 1.408199e+02 |     8.1543 |   0 |   5695606 |   5695606 | 19.666983 | 388.60310 |   14217.0204 |
| PM2.5.g            |    5 | 402 |    4723.6443 |    69454.677 |     4.0 | 7.199689e+01 |     5.9304 |   0 |   1382822 |   1382822 | 19.453251 | 382.59517 |    3464.0845 |
| SO2.g              |    6 | 402 |    5711.2488 |    55906.619 |    14.0 | 2.276739e+02 |    20.7564 |   0 |   1082028 |   1082028 | 17.908593 | 339.19226 |    2788.3687 |
| VOC.g              |    7 | 402 |    4737.9104 |    40628.764 |    22.5 | 2.172981e+02 |    33.3585 |   0 |    723802 |    723802 | 15.184308 | 251.89230 |    2026.3786 |
| Total.g.CO2e       |    8 | 402 | 3816211.4975 | 30772366.891 | 17144.5 | 1.191106e+05 | 25197.5283 |   0 | 512173950 | 512173950 | 13.299869 | 198.03556 | 1534786.1634 |
| CO2.Fossil.g.CO2e  |    9 | 402 | 1834585.9353 | 14412095.613 | 16962.5 | 9.019479e+04 | 24927.6951 |   0 | 257559803 | 257559803 | 14.832228 | 248.47182 |  718809.9963 |
| CO2.Process.g.CO2e |   10 | 402 |  124910.3731 |  1301289.377 |     0.0 | 0.000000e+00 |     0.0000 |   0 |  18929557 |  18929557 | 12.407185 | 160.89258 |   64902.4151 |
| CH4.g.CO2e         |   11 | 402 |  545257.2090 |  5900697.887 |     0.0 | 0.000000e+00 |     0.0000 |   0 | 108460987 | 108460987 | 15.916464 | 278.74807 |  294300.0616 |
| HFC.PFCs.g.CO2e    |   12 | 402 |   21954.0572 |   250994.325 |     0.0 | 0.000000e+00 |     0.0000 |   0 |   4161163 |   4161163 | 13.617934 | 201.14719 |   12518.4591 |
| Fugitive.mg        |   13 | 402 |  122187.4279 |   915621.053 |   156.0 | 3.679661e+03 |   231.2856 |   0 |  12189404 |  12189404 | 11.026986 | 130.43964 |   45667.0275 |
| Stack.mg           |   14 | 402 |  420683.3582 |  2733120.398 |   309.0 | 9.008301e+03 |   458.1234 |   0 |  33321666 |  33321666 |  8.712720 |  82.72182 |  136315.6557 |
| Total.Air.mg       |   15 | 402 |  542871.1891 |  3445903.489 |   567.5 | 1.356760e+04 |   841.3755 |   0 |  37092916 |  37092916 |  8.377980 |  73.86411 |  171866.0451 |
| Surface.water.mg   |   16 | 402 |  128163.1841 |  1064869.900 |     0.0 | 3.836894e+02 |     0.0000 |   0 |  16404607 |  16404607 | 12.738129 | 175.01346 |   53110.8833 |
| U\_ground.Water.mg |   17 | 402 |   64391.2164 |   563431.638 |     0.0 | 0.000000e+00 |     0.0000 |   0 |   8839104 |   8839104 | 12.131672 | 164.85908 |   28101.4159 |
| Land.mg            |   18 | 402 |  301267.4900 |  3280450.751 |     0.0 | 3.429286e+02 |     0.0000 |   0 |  59730055 |  59730055 | 15.784280 | 271.04069 |  163614.0125 |
| Offiste.mg         |   19 | 402 |  117607.6294 |   991341.570 |    88.5 | 2.997488e+03 |   131.2101 |   0 |  18394299 |  18394299 | 16.006124 | 285.75539 |   49443.6236 |
| POTW.Metal.mg      |   20 | 402 |     706.3532 |     3661.431 |     0.0 | 3.324224e+01 |     0.0000 |   0 |     40218 |     40218 |  7.481724 |  61.88361 |     182.6156 |

``` r
psych::pairs.panels(resource %>% cbind(dat$SO2.g), 
                    method = "spearman")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r
psych::pairs.panels(log(resource+1) %>% cbind(log(dat$SO2.g)), 
                    method = "spearman")
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

# Clustering for the data

# Regression

## user-define function

``` r
# test: target_nm = "CO.g",  X = resource
makedata_map <- function(target_nm, dat, X){
  # loglog transform
  y <- dat %>% select(target_nm)
  Lny <- log(y) %>% stats::setNames(paste0("Ln", names(.)))
  LnX <- log(X+1) %>% stats::setNames(paste0("Ln", names(.)))
  # combine, filter log(y)=0; add ID:Sector
  Xy <- cbind(LnX, Lny)
  Xy <- cbind(dat %>% select(Sector), Xy) 
  Xy <- Xy[!is.infinite(rowSums(Lny)),] %>% mutate(Sector= Sector %>% as.factor())
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
```

## Linear Regression Modeling with log10 Transformation on Input and Output

``` r
# create a dataframe with a column with impact variable names 
target_list <- tibble(target = c(colnames(CPA),colnames(GHG),colnames(TOX))); target_list %>% flatten() %>% unlist()
#>  [1] "CO.g"               "NH3.g"              "NOx.g"             
#>  [4] "PM10.g"             "PM2.5.g"            "SO2.g"             
#>  [7] "VOC.g"              "Total.g.CO2e"       "CO2.Fossil.g.CO2e" 
#> [10] "CO2.Process.g.CO2e" "CH4.g.CO2e"         "HFC.PFCs.g.CO2e"   
#> [13] "Fugitive.mg"        "Stack.mg"           "Total.Air.mg"      
#> [16] "Surface.water.mg"   "U_ground.Water.mg"  "Land.mg"           
#> [19] "Offiste.mg"         "POTW.Metal.mg"

# model selection for each impact variable
bestglm_list <- target_list %>% 
  mutate(data = target %>% 
           map(function(target_nm) makedata_map(target_nm,
                                                dat= dat, 
                                                X = resource))) %>% 
  mutate(rowdata = data %>% map_dbl(nrow)) %>% 
  filter(rowdata > 100) %>% 
  select(-rowdata) %>% 
  mutate(best_model = data %>% map(fit_model)) %>% 
  mutate(anv = best_model %>% map(anova)) %>% 
  mutate(statisics = best_model %>% purrr::map(.f = function(m) broom::glance(m))) %>% 
  tidyr::unnest(statisics) %>% 
  mutate(wald_pval = best_model %>% 
           purrr::map_dbl(function(model) waldtest_map(model= model, null= 1))) %>% 
  mutate(nrows_data = data %>% purrr::map_dbl(nrow)) %>% 
  arrange(desc(r.squared))

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

datArray <- abind::abind(coef_list %>% 
                           select(-target) %>% 
                           mutate_if(is.numeric, signif, digits = 3) %>% 
                           mutate_all(as.character),
                         signif_list %>% 
                           select(-target) %>% 
                           mutate_if(is.numeric, gtools::stars.pval),along=3)
coef_signif_list <- bestglm_list %>% 
  select(target, adj.r.squared, p.value, wald_pval) %>% 
  mutate_at(c("adj.r.squared", "p.value", "wald_pval"), signif, digits = 3) %>% 
  cbind(apply(datArray,1:2, bind_coef_star) %>% as_tibble())

coef_signif_list$exponent_sum <- coef_list[,3:8] %>% rowSums(na.rm = TRUE) %>% signif(digits = 3)


# result
coef_signif_list %>% knitr::kable(format = "markdown") 
```

| target            | adj.r.squared | p.value | wald\_pval | (Intercept)     | LnBioWasteMJ  | LnCoalMJ       | LnNatGaseMJ   | LnNonFossElecMJ | LnPetrolMJ    | LnWaterWithdrawalsKgal | exponent\_sum |
| :---------------- | ------------: | ------: | ---------: | :-------------- | :------------ | :------------- | :------------ | :-------------- | :------------ | :--------------------- | ------------: |
| CO2.Fossil.g.CO2e |         0.921 |       0 |          0 | \-0.234         | 0.145(\*\*\*) |                |               | 0.431(\*\*\*)   | 0.534(\*\*\*) |                        |         1.110 |
| Total.g.CO2e      |         0.918 |       0 |          0 | 4.35(\*\*\*)    | 0.0418        |                | 0.268(\*\*\*) | 0.414(\*\*\*)   | 0.479(\*\*\*) |                        |         1.200 |
| NOx.g             |         0.774 |       0 |          0 | 1.35(\*\*)      | 0.388(\*\*\*) |                |               | 0.622(\*\*\*)   | \-0.116       | 0.175(\*)              |         1.070 |
| SO2.g             |         0.766 |       0 |          0 | \-0.368         |               | 0.229(\*)      | \-0.403       | 0.711(\*\*\*)   | 0.321(\*)     | 0.245(\*)              |         1.100 |
| CO.g              |         0.656 |       0 |          0 | \-3.89(\*\*\*)  | 0.148(\*\*)   | \-0.165(\*\*)  |               | 0.345(\*\*\*)   | 0.389(\*\*\*) | 0.178(\*\*\*)          |         0.896 |
| Fugitive.mg       |         0.648 |       0 |          0 | \-0.745(\*\*\*) | 0.164(\*\*\*) |                | 0.0932        | 0.348(\*\*\*)   | 0.541(\*\*\*) |                        |         1.150 |
| PM10.g            |         0.647 |       0 |          0 | 1.55(\*\*\*)    |               | 0.118          | 0.339(\*)     | 0.792(\*\*\*)   | \-0.228(\*\*) |                        |         1.020 |
| PM2.5.g           |         0.646 |       0 |          0 | \-1.71(\*\*\*)  | 0.225(\*\*\*) |                |               | 0.136           | 0.539(\*\*\*) | 0.147(\*\*)            |         1.050 |
| Total.Air.mg      |         0.628 |       0 |          0 | \-1.56(\*\*\*)  | 0.21(\*\*\*)  |                |               | 0.149(\*)       | 0.509(\*\*\*) | 0.108(\*)              |         0.976 |
| VOC.g             |         0.600 |       0 |          0 | 0.28            | 0.269(\*\*\*) |                |               | 0.519(\*\*\*)   |               |                        |         0.788 |
| Stack.mg          |         0.580 |       0 |          0 | \-0.566(\*)     | 0.221(\*\*\*) | 0.245(\*\*\*)  |               | 0.204(\*\*\*)   | 0.526(\*\*\*) |                        |         1.200 |
| NH3.g             |         0.544 |       0 |          0 | 3.12(\*\*\*)    | 0.417(\*\*\*) | 0.151          | 0.31          | 0.451(\*\*)     | \-0.211(\*)   |                        |         1.120 |
| Surface.water.mg  |         0.541 |       0 |          0 | \-2.76(\*\*)    |               |                | 0.675(\*\*\*) |                 | 0.199         | 0.293(\*\*)            |         1.170 |
| Offiste.mg        |         0.535 |       0 |          0 | 3.82(\*\*\*)    | 0.438(\*\*\*) | 0.142          | 0.256         | 0.441(\*\*)     | \-0.168(\*)   |                        |         1.110 |
| Land.mg           |         0.476 |       0 |          0 | 4.12(\*\*\*)    | 0.0373        | \-0.0822(\*\*) | 0.32(\*\*\*)  | 0.442(\*\*\*)   | 0.508(\*\*\*) |                        |         1.230 |
| POTW.Metal.mg     |         0.466 |       0 |          0 | \-0.119         | 0.249(\*\*\*) | \-0.137(\*\*)  |               | 0.471(\*\*\*)   | 0.358(\*\*\*) |                        |         0.941 |

# Communciate and visualize the results

## The linear model with R^2 \>0.75

``` r
good_lm <- bestglm_list %>% filter(adj.r.squared >0.75)
good_lm %>% arrange(desc(adj.r.squared)) %>% select(target) %>% flatten() %>% unlist
#> [1] "CO2.Fossil.g.CO2e" "Total.g.CO2e"      "NOx.g"            
#> [4] "SO2.g"
```

## The diagnosis of linear model

``` r
par(mfrow=c(2,3))
plot(good_lm$best_model[[2]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

# Result

## Impact between Sector

``` r
# descriptive analysis
good_lm$target
#> [1] "CO2.Fossil.g.CO2e" "Total.g.CO2e"      "NOx.g"            
#> [4] "SO2.g"
# NOx.t: Emissions of Nitrogen Oxides to Air from each sector. t = meric tons
# SO2.t: Emissions of Sulfur Dioxide to Air from each sector. t = meric tons 
# Total.t.CO2e: Global Warming Potential (GWP) is a weighting of greenhouse gas emissions into the air from the production of each sector. Weighting factors are 100-year GWP values from the IPCC Second Assessment Report (IPCC 2001). t CO2e = metric tons of CO2 equivalent emissions. 
# CO2.Fossil.t.CO2e C: Emissions of Carbon Dioxide (CO2) into the air from each sector from fossil fuel combustion sources. t CO2e = metric tons of CO2 equivalent.
par(mfrow=c(1,2))
i=2
good_lm$data[[i]][,ncol(good_lm$data[[i]])] %>% boxplot()
plot(good_lm$data[[i]][,1], good_lm$data[[i]][,ncol(good_lm$data[[i]])])
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />
