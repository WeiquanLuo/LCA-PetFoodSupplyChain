EIO-LCA Analysis: Pet Food Supply Chain
================
Weiquan Luo, Mingjun Ma
2019-12-09

  - [Background information](#background-information)
  - [Interesting question](#interesting-question)
  - [Hightlight of result](#hightlight-of-result)
  - [Data description](#data-description)
  - [Workflow](#workflow)
  - [Explore the data](#explore-the-data)
      - [Correlation](#correlation)
  - [Regression](#regression)
      - [User-defined Functions](#user-defined-functions)
      - [LogLog Linear Regression](#loglog-linear-regression)
      - [Diagnosis for GHG CO2 Equvivalent
        model:](#diagnosis-for-ghg-co2-equvivalent-model)
      - [Partial-residual plots](#partial-residual-plots)
      - [Random effect: Impact among
        sectors](#random-effect-impact-among-sectors)
  - [Clustering for CO2 Equvivalent](#clustering-for-co2-equvivalent)
      - [Dendrogram](#dendrogram)
      - [Elbow plot](#elbow-plot)
      - [DBSCAN at eps = 4](#dbscan-at-eps-4)
      - [Visualize clusters](#visualize-clusters)
  - [Discussion](#discussion)
  - [Final Note](#final-note)
  - [Exercise](#exercise)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Background information

According to the APPA (American Pet Product Association) 2019-2020 pet
owner survey, around 84.9 million U.S. households own pet, which is
about 67% of the U.S. homes (APPA, 2019). In 2018, the U.S. customer
spent 72.56 billion dollars on their pets, of which 30.32 billion
dollars is for pet food. From a 2017 U.S. pet owners survey, 2% of dog
and cat owners select their pet food based on product claim of
sustainable or eco-friendly pet food formula. Although the percentage is
low, the interest in the sustainability of the pet food has grown a lot
recently.

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

1.  which industry(s) have larger impact among all industries?
2.  what are the relationship between some impact relative to the input
    (i.e. Energy, water withdraw)?
3.  how the outlier industry(s) behave in linear regression models

# Hightlight of result

  - for dog and cat food Manufacturing, the greatest environmental
    impacts come frome any raw material production industries such as
    agricultural farming.
  - most of industries use either NonFossoil Eletrecity or Fossoil
    Eletrecity
  - for those industries using biowaste as energy source have higher
    impact in toxic.

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

# Workflow

<center>

![EIO-LCA:Pet Food Supply
Chain](img/EIO-LCA_%20Pet%20Food%20Supply%20Chain.png)

</center>

# Explore the data

After webscaping, combinding the raw data, and manually making minor
modification, we result a datafarme stored as ’dat\_311111\_1M\_v2.csv.

``` r
# data input
dat <- read.csv("data/dat_311111_1M_v2.csv")
dim(dat)
#> [1] 402  41
```

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
# piping: Sort environment impact group and input resouce, unit convertion to result no 0<.<1, rename by unit
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

# summary statistic for X variable
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

## Correlation

Observation from correlation plot

  - The scatter plots show most information are concentrate at the
    origin of the feature space, but sparse anywhere else
    (heteroskedastic);
  - The distribution plots show variables are highly skewed (not
    normally distributed);
  - The spearman correlations show that most independent variables are
    moderate correlative, but some are unlikly and some are likly
    correlative (multicollinearity).

<!-- end list -->

``` r
# raw X variable
psych::pairs.panels(resource, method = "spearman")
```

<img src="man/figures/README-corplot-1.png" width="100%" />

The data is suitable for log transformation and could be modelled in
LogLog transformation.

con

  - the interpretation of each parameter distorted from scalar to ratio
    of the scalar quantity (i.e. For regressions, the coefficent is
    interpret as the reletive change, in clusterings, the cluster is the
    group of same magnitude).
  - it is not able to fix the issue of multicollinearity, so
    variable/model selection are required.

pro

  - the information at the origin can be stretched out.  
  - the resulting linear regression can be interpreted at elastisity
    (similar to the idea cobb-douglas utility function in Econometrics)

<!-- end list -->

``` r
# Ln X variable
psych::pairs.panels(log(resource+1), method = "spearman")
```

<img src="man/figures/README-corplot_LN-1.png" width="100%" />

After the log transformation, let’s take a look at the visulization of a
5-dimemtional plot: Total CO2 Equvivalent as the target variable by
Sectors, three resources as input variables as example (click
[Here](https://weiquanluo.github.io/img/plotly_GHG_TotalCO2.html)).

# Regression

We are going to natural log transform both X and Y and fit regression
models to each of 20 environmental impact variables. The six input
resouce vairables consist of Coal.MJ, NatGase.MJ, Petrol.MJ,
Bio.Waste.MJ, NonFossElec.MJ, and Water.Withdrawals.Kgal. The
environmental impact variables are 7 variables for conventional air
polutant, 5 variables for greenhouse gases, 8 variables for Toxic.
Druding the data processing, if the count of datapoint for a target
variable is less than 100, then we abandon the corresponding models.
After data processing and stepwise model selections, it result 16 model
candidates, where one for each valid target variable. The following
user-defined function are prepared for functional programming with using
purrr style lamda function.

## User-defined Functions

``` r
# test: target_nm = "CO.g",  X = resource
makedata_map <- function(target_nm, dat, X){
  # loglog transform
  y <- dat %>% select(target_nm)
  Lny <- log(y) %>% stats::setNames(paste0("Ln", names(.)))
  LnX <- log(X+1) %>% stats::setNames(paste0("Ln", names(.)))
  # combine, filter log(y)=0; add ID:Sector
  Xy <- cbind(LnX, Lny)
  Xy <- cbind(dat %>% select(Sector, Description, name_sub, Sector_sub), Xy) 
  Xy <- Xy[!is.infinite(rowSums(Lny)),]
  colnames(Xy) <- colnames(Xy) %>% stringr::str_replace_all("\\.","") 
  return(Xy)
}
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

## LogLog Linear Regression

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

# piping: variale selection, anova, extract statistic, wald test
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
  arrange(desc(adj.r.squared))
# extract coef from each model
coef_list <- bestglm_list %>% 
  mutate(coefs = best_model %>% purrr::map(.f=broom::tidy)) %>% 
  select(target, coefs) %>% 
  tidyr::unnest(coefs) %>% 
  select(target, term, estimate) %>% 
  tidyr::spread(key= term, value = estimate)
# extract pval for all coef from each model
signif_list <- bestglm_list %>% 
  mutate(coefs = best_model %>% purrr::map(.f=broom::tidy)) %>% 
  select(target, coefs) %>% 
  tidyr::unnest(coefs) %>% 
  select(target, term, p.value) %>% 
  tidyr::spread(key= term, value = p.value)
# combind coef and pval for visual
coef_signif_list <- coef_list %>% 
  select(target) %>% 
  cbind(apply(abind::abind(coef_list %>% 
                             select(-target) %>% 
                             mutate_if(is.numeric, signif, digits = 3) %>% 
                             mutate_all(as.character),
                           signif_list %>% 
                             select(-target) %>% 
                             mutate_if(is.numeric, gtools::stars.pval),along=3),
              1:2, bind_coef_star))
# add statistic to the coef and pval
coef_signif_list <- bestglm_list %>% 
  select(target, adj.r.squared, p.value, wald_pval) %>% 
  mutate_at(c("adj.r.squared", "p.value", "wald_pval"), signif, digits = 3) %>% 
  left_join(coef_signif_list, by="target")
# get exponent_sum
coef_signif_list$exponent_sum <- coef_list[,3:8] %>% rowSums(na.rm = TRUE) %>% signif(digits = 3)
```

``` r
# result
coef_signif_list %>% 
  arrange(desc(adj.r.squared)) %>% 
  knitr::kable(format = "markdown") 
```

| target            | adj.r.squared | p.value | wald\_pval | (Intercept)     | LnBioWasteMJ  | LnCoalMJ       | LnNatGaseMJ   | LnNonFossElecMJ | LnPetrolMJ    | LnWaterWithdrawalsKgal | exponent\_sum |
| :---------------- | ------------: | ------: | ---------: | :-------------- | :------------ | :------------- | :------------ | :-------------- | :------------ | :--------------------- | ------------: |
| CO2.Fossil.g.CO2e |         0.921 |       0 |          0 | 4.35(\*\*\*)    | 0.0418        |                | 0.268(\*\*\*) | 0.414(\*\*\*)   | 0.479(\*\*\*) |                        |         1.110 |
| Total.g.CO2e      |         0.918 |       0 |          0 | 4.12(\*\*\*)    | 0.0373        | \-0.0822(\*\*) | 0.32(\*\*\*)  | 0.442(\*\*\*)   | 0.508(\*\*\*) |                        |         1.200 |
| NOx.g             |         0.774 |       0 |          0 | \-0.745(\*\*\*) | 0.164(\*\*\*) |                | 0.0932        | 0.348(\*\*\*)   | 0.541(\*\*\*) |                        |         1.070 |
| SO2.g             |         0.766 |       0 |          0 | \-0.566(\*)     | 0.221(\*\*\*) | 0.245(\*\*\*)  |               | 0.204(\*\*\*)   | 0.526(\*\*\*) |                        |         1.100 |
| CO.g              |         0.656 |       0 |          0 | \-0.234         | 0.145(\*\*\*) |                |               | 0.431(\*\*\*)   | 0.534(\*\*\*) |                        |         0.896 |
| Fugitive.mg       |         0.648 |       0 |          0 | 1.35(\*\*)      | 0.388(\*\*\*) |                |               | 0.622(\*\*\*)   | \-0.116       | 0.175(\*)              |         1.150 |
| PM10.g            |         0.647 |       0 |          0 | \-1.71(\*\*\*)  | 0.225(\*\*\*) |                |               | 0.136           | 0.539(\*\*\*) | 0.147(\*\*)            |         1.020 |
| PM2.5.g           |         0.646 |       0 |          0 | \-1.56(\*\*\*)  | 0.21(\*\*\*)  |                |               | 0.149(\*)       | 0.509(\*\*\*) | 0.108(\*)              |         1.050 |
| Total.Air.mg      |         0.628 |       0 |          0 | 3.82(\*\*\*)    | 0.438(\*\*\*) | 0.142          | 0.256         | 0.441(\*\*)     | \-0.168(\*)   |                        |         0.976 |
| VOC.g             |         0.600 |       0 |          0 | \-0.119         | 0.249(\*\*\*) | \-0.137(\*\*)  |               | 0.471(\*\*\*)   | 0.358(\*\*\*) |                        |         0.788 |
| Stack.mg          |         0.580 |       0 |          0 | 3.12(\*\*\*)    | 0.417(\*\*\*) | 0.151          | 0.31          | 0.451(\*\*)     | \-0.211(\*)   |                        |         1.200 |
| NH3.g             |         0.544 |       0 |          0 | \-3.89(\*\*\*)  | 0.148(\*\*)   | \-0.165(\*\*)  |               | 0.345(\*\*\*)   | 0.389(\*\*\*) | 0.178(\*\*\*)          |         1.120 |
| Surface.water.mg  |         0.541 |       0 |          0 | \-2.76(\*\*)    |               |                | 0.675(\*\*\*) |                 | 0.199         | 0.293(\*\*)            |         1.170 |
| Offiste.mg        |         0.535 |       0 |          0 | 1.55(\*\*\*)    |               | 0.118          | 0.339(\*)     | 0.792(\*\*\*)   | \-0.228(\*\*) |                        |         1.110 |
| Land.mg           |         0.476 |       0 |          0 | \-0.368         |               | 0.229(\*)      | \-0.403       | 0.711(\*\*\*)   | 0.321(\*)     | 0.245(\*)              |         1.230 |
| POTW.Metal.mg     |         0.466 |       0 |          0 | 0.28            | 0.269(\*\*\*) |                |               | 0.519(\*\*\*)   |               |                        |         0.941 |

The following linear model with R^2 \>0.75

``` r
good_lm <- bestglm_list %>% filter(adj.r.squared >0.75)
good_lm %>% arrange(desc(adj.r.squared)) %>% select(target) %>% flatten() %>% unlist
#> [1] "CO2.Fossil.g.CO2e" "Total.g.CO2e"      "NOx.g"            
#> [4] "SO2.g"
```

## Diagnosis for GHG CO2 Equvivalent model:

``` r
par(mfrow=c(2,3))
plot(good_lm$best_model[[2]], which=1:6)
```

<img src="man/figures/README-diagsis-1.png" width="100%" />

## Partial-residual plots

``` r
car::crPlots(bestglm_list$best_model[[2]])
```

<img src="man/figures/README-prplot-1.png" width="100%" />

## Random effect: Impact among sectors

``` r
# CO2.Fossil.t.CO2e C: Emissions of Carbon Dioxide (CO2) into the air from each sector from fossil fuel combustion sources. t CO2e = metric tons of CO2 equivalent.
par(mfrow=c(1,2))
i=2
good_lm$data[[i]][,ncol(good_lm$data[[i]])] %>% boxplot()
plot(good_lm$data[[i]][,1], good_lm$data[[i]][,ncol(good_lm$data[[i]])])
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
# random effect model
data_anv <- good_lm$data[[2]] %>% select(-Description,-name_sub, -Sector_sub)
model.rand_block <- nlme::lme(data = data_anv,
                     LnTotalgCO2e ~ LnBioWasteMJ + LnCoalMJ + LnNatGaseMJ + LnNonFossElecMJ + LnPetrolMJ,
                     random = ~1|Sector)
# anova(model.rand_block)
# fixed effect model
model.fixed = nlme::gls(data = data_anv,
                  LnTotalgCO2e ~ LnBioWasteMJ + LnCoalMJ + LnNatGaseMJ + LnNonFossElecMJ + LnPetrolMJ,
                  method="REML")
# anova(model.fixed)
# Test the random effects in the model
anova(model.rand_block, model.fixed)
#>                  Model df      AIC      BIC    logLik   Test  L.Ratio
#> model.rand_block     1  8 1061.724 1093.329 -522.8620                
#> model.fixed          2  7 1144.165 1171.819 -565.0823 1 vs 2 84.44057
#>                  p-value
#> model.rand_block        
#> model.fixed       <.0001
```

The extremely small p-value in testing random effect suggest that there
are significant difference in green house gases CO2 equivalence among
sectors.

# Clustering for CO2 Equvivalent

For instance, we are interested in what the outlier industries in
producing CO2 Equvivalent. In general, we need to be extremely careful
to rescaling data for distance based modeling that is sensitive to the
distance between datapoints. From the above analysis, we understand this
data is suitable in log scale. With considerations, we maintain the log
transformation in the following distance based modeling, for reason of
consistency and explanatory power. At this section, we are going to
cluster the the relative size (or magnitude) of Greenhouse gases CO2
Equvivalent among all industries, instead of the scalar quantity. We
used a DBSCANS algorithm to cluster the industries with magnitude of
green house gases CO2 equivalence and the magnitude of the six input
sources variable. The clearn log-transformed data of fitting CO2
Equvivalent linear regression is obtained from the `bestglm_list` object
above. Then, we feed this log-transformed data to the cluster algorithm
in python.

``` r
# input data to clustering using python
dat_ghg_total_co2e <-bestglm_list %>% filter(target =="Total.g.CO2e") %>% select(data)
dat_ghg_total_co2e <- dat_ghg_total_co2e[[1]][[1]]
dat_ghg <- dat_ghg_total_co2e[,-c(1:4)]
dat_ghg %>% head %>% knitr::kable(format = "markdown") 
```

| LnCoalMJ | LnNatGaseMJ | LnPetrolMJ | LnBioWasteMJ | LnNonFossElecMJ | LnWaterWithdrawalsKgal | LnTotalgCO2e |
| -------: | ----------: | ---------: | -----------: | --------------: | ---------------------: | -----------: |
| 4.521789 |    9.835637 |  10.462703 |            0 |        0.000000 |               16.91921 |     15.02216 |
| 2.890372 |    5.961005 |   6.350886 |            0 |        7.360740 |               12.90453 |     11.00342 |
| 3.806662 |    8.771525 |   8.921191 |            0 |        6.911747 |               14.96923 |     13.59814 |
| 3.218876 |    6.823286 |   7.527256 |            0 |        5.921578 |               13.63874 |     12.06601 |
| 0.000000 |    8.566174 |  10.936387 |            0 |        9.818420 |               19.14402 |     17.26302 |
| 0.000000 |    3.784190 |   6.577861 |            0 |        3.850148 |               11.94516 |     10.94366 |

``` r
# use python3 engine
library(reticulate)
use_python("/usr/local/bin/python3")
```

``` python
# data type
import pandas as pd
import numpy as np
# visualization
import seaborn as sns
import matplotlib.pyplot as plt
import collections
from scipy.cluster.hierarchy import linkage, fcluster, dendrogram
# fit model
from sklearn.cluster import DBSCAN
```

``` python
# input
dat_ghg = r.dat_ghg
dat_ghg1 = dat_ghg.iloc[:,1:]
```

## Dendrogram

``` python
# dendrogram
Z = linkage(dat_ghg1, method='ward')
plt.figure(figsize=(15,15))
dendrogram(Z) 
#> {'icoord': [[5.0, 5.0, 15.0, 15.0], [25.0, 25.0, 35.0, 35.0], [45.0, 45.0, 55.0, 55.0], [30.0, 30.0, 50.0, 50.0], [10.0, 10.0, 40.0, 40.0], [65.0, 65.0, 75.0, 75.0], [95.0, 95.0, 105.0, 105.0], [85.0, 85.0, 100.0, 100.0], [70.0, 70.0, 92.5, 92.5], [135.0, 135.0, 145.0, 145.0], [125.0, 125.0, 140.0, 140.0], [165.0, 165.0, 175.0, 175.0], [155.0, 155.0, 170.0, 170.0], [132.5, 132.5, 162.5, 162.5], [115.0, 115.0, 147.5, 147.5], [215.0, 215.0, 225.0, 225.0], [205.0, 205.0, 220.0, 220.0], [195.0, 195.0, 212.5, 212.5], [255.0, 255.0, 265.0, 265.0], [245.0, 245.0, 260.0, 260.0], [295.0, 295.0, 305.0, 305.0], [285.0, 285.0, 300.0, 300.0], [275.0, 275.0, 292.5, 292.5], [252.5, 252.5, 283.75, 283.75], [235.0, 235.0, 268.125, 268.125], [203.75, 203.75, 251.5625, 251.5625], [315.0, 315.0, 325.0, 325.0], [335.0, 335.0, 345.0, 345.0], [355.0, 355.0, 365.0, 365.0], [340.0, 340.0, 360.0, 360.0], [385.0, 385.0, 395.0, 395.0], [375.0, 375.0, 390.0, 390.0], [350.0, 350.0, 382.5, 382.5], [405.0, 405.0, 415.0, 415.0], [435.0, 435.0, 445.0, 445.0], [425.0, 425.0, 440.0, 440.0], [410.0, 410.0, 432.5, 432.5], [366.25, 366.25, 421.25, 421.25], [320.0, 320.0, 393.75, 393.75], [227.65625, 227.65625, 356.875, 356.875], [185.0, 185.0, 292.265625, 292.265625], [131.25, 131.25, 238.6328125, 238.6328125], [81.25, 81.25, 184.94140625, 184.94140625], [25.0, 25.0, 133.095703125, 133.095703125], [475.0, 475.0, 485.0, 485.0], [465.0, 465.0, 480.0, 480.0], [455.0, 455.0, 472.5, 472.5], [495.0, 495.0, 505.0, 505.0], [525.0, 525.0, 535.0, 535.0], [545.0, 545.0, 555.0, 555.0], [530.0, 530.0, 550.0, 550.0], [515.0, 515.0, 540.0, 540.0], [500.0, 500.0, 527.5, 527.5], [575.0, 575.0, 585.0, 585.0], [565.0, 565.0, 580.0, 580.0], [595.0, 595.0, 605.0, 605.0], [572.5, 572.5, 600.0, 600.0], [513.75, 513.75, 586.25, 586.25], [463.75, 463.75, 550.0, 550.0], [635.0, 635.0, 645.0, 645.0], [625.0, 625.0, 640.0, 640.0], [615.0, 615.0, 632.5, 632.5], [655.0, 655.0, 665.0, 665.0], [685.0, 685.0, 695.0, 695.0], [675.0, 675.0, 690.0, 690.0], [725.0, 725.0, 735.0, 735.0], [715.0, 715.0, 730.0, 730.0], [705.0, 705.0, 722.5, 722.5], [682.5, 682.5, 713.75, 713.75], [660.0, 660.0, 698.125, 698.125], [745.0, 745.0, 755.0, 755.0], [775.0, 775.0, 785.0, 785.0], [765.0, 765.0, 780.0, 780.0], [750.0, 750.0, 772.5, 772.5], [795.0, 795.0, 805.0, 805.0], [815.0, 815.0, 825.0, 825.0], [835.0, 835.0, 845.0, 845.0], [820.0, 820.0, 840.0, 840.0], [800.0, 800.0, 830.0, 830.0], [761.25, 761.25, 815.0, 815.0], [679.0625, 679.0625, 788.125, 788.125], [855.0, 855.0, 865.0, 865.0], [875.0, 875.0, 885.0, 885.0], [905.0, 905.0, 915.0, 915.0], [895.0, 895.0, 910.0, 910.0], [880.0, 880.0, 902.5, 902.5], [860.0, 860.0, 891.25, 891.25], [925.0, 925.0, 935.0, 935.0], [955.0, 955.0, 965.0, 965.0], [945.0, 945.0, 960.0, 960.0], [930.0, 930.0, 952.5, 952.5], [875.625, 875.625, 941.25, 941.25], [733.59375, 733.59375, 908.4375, 908.4375], [623.75, 623.75, 821.015625, 821.015625], [975.0, 975.0, 985.0, 985.0], [995.0, 995.0, 1005.0, 1005.0], [1025.0, 1025.0, 1035.0, 1035.0], [1015.0, 1015.0, 1030.0, 1030.0], [1000.0, 1000.0, 1022.5, 1022.5], [1045.0, 1045.0, 1055.0, 1055.0], [1011.25, 1011.25, 1050.0, 1050.0], [980.0, 980.0, 1030.625, 1030.625], [1065.0, 1065.0, 1075.0, 1075.0], [1085.0, 1085.0, 1095.0, 1095.0], [1070.0, 1070.0, 1090.0, 1090.0], [1105.0, 1105.0, 1115.0, 1115.0], [1135.0, 1135.0, 1145.0, 1145.0], [1125.0, 1125.0, 1140.0, 1140.0], [1165.0, 1165.0, 1175.0, 1175.0], [1155.0, 1155.0, 1170.0, 1170.0], [1132.5, 1132.5, 1162.5, 1162.5], [1110.0, 1110.0, 1147.5, 1147.5], [1080.0, 1080.0, 1128.75, 1128.75], [1005.3125, 1005.3125, 1104.375, 1104.375], [722.3828125, 722.3828125, 1054.84375, 1054.84375], [506.875, 506.875, 888.61328125, 888.61328125], [79.0478515625, 79.0478515625, 697.744140625, 697.744140625], [1185.0, 1185.0, 1195.0, 1195.0], [1215.0, 1215.0, 1225.0, 1225.0], [1205.0, 1205.0, 1220.0, 1220.0], [1190.0, 1190.0, 1212.5, 1212.5], [1245.0, 1245.0, 1255.0, 1255.0], [1235.0, 1235.0, 1250.0, 1250.0], [1265.0, 1265.0, 1275.0, 1275.0], [1305.0, 1305.0, 1315.0, 1315.0], [1295.0, 1295.0, 1310.0, 1310.0], [1285.0, 1285.0, 1302.5, 1302.5], [1270.0, 1270.0, 1293.75, 1293.75], [1345.0, 1345.0, 1355.0, 1355.0], [1335.0, 1335.0, 1350.0, 1350.0], [1325.0, 1325.0, 1342.5, 1342.5], [1281.875, 1281.875, 1333.75, 1333.75], [1365.0, 1365.0, 1375.0, 1375.0], [1307.8125, 1307.8125, 1370.0, 1370.0], [1242.5, 1242.5, 1338.90625, 1338.90625], [1385.0, 1385.0, 1395.0, 1395.0], [1415.0, 1415.0, 1425.0, 1425.0], [1405.0, 1405.0, 1420.0, 1420.0], [1435.0, 1435.0, 1445.0, 1445.0], [1485.0, 1485.0, 1495.0, 1495.0], [1475.0, 1475.0, 1490.0, 1490.0], [1465.0, 1465.0, 1482.5, 1482.5], [1455.0, 1455.0, 1473.75, 1473.75], [1440.0, 1440.0, 1464.375, 1464.375], [1412.5, 1412.5, 1452.1875, 1452.1875], [1390.0, 1390.0, 1432.34375, 1432.34375], [1505.0, 1505.0, 1515.0, 1515.0], [1545.0, 1545.0, 1555.0, 1555.0], [1535.0, 1535.0, 1550.0, 1550.0], [1565.0, 1565.0, 1575.0, 1575.0], [1542.5, 1542.5, 1570.0, 1570.0], [1525.0, 1525.0, 1556.25, 1556.25], [1510.0, 1510.0, 1540.625, 1540.625], [1595.0, 1595.0, 1605.0, 1605.0], [1615.0, 1615.0, 1625.0, 1625.0], [1600.0, 1600.0, 1620.0, 1620.0], [1585.0, 1585.0, 1610.0, 1610.0], [1525.3125, 1525.3125, 1597.5, 1597.5], [1411.171875, 1411.171875, 1561.40625, 1561.40625], [1645.0, 1645.0, 1655.0, 1655.0], [1675.0, 1675.0, 1685.0, 1685.0], [1665.0, 1665.0, 1680.0, 1680.0], [1650.0, 1650.0, 1672.5, 1672.5], [1635.0, 1635.0, 1661.25, 1661.25], [1486.2890625, 1486.2890625, 1648.125, 1648.125], [1290.703125, 1290.703125, 1567.20703125, 1567.20703125], [1201.25, 1201.25, 1428.955078125, 1428.955078125], [1715.0, 1715.0, 1725.0, 1725.0], [1705.0, 1705.0, 1720.0, 1720.0], [1695.0, 1695.0, 1712.5, 1712.5], [1735.0, 1735.0, 1745.0, 1745.0], [1755.0, 1755.0, 1765.0, 1765.0], [1740.0, 1740.0, 1760.0, 1760.0], [1795.0, 1795.0, 1805.0, 1805.0], [1785.0, 1785.0, 1800.0, 1800.0], [1775.0, 1775.0, 1792.5, 1792.5], [1825.0, 1825.0, 1835.0, 1835.0], [1815.0, 1815.0, 1830.0, 1830.0], [1783.75, 1783.75, 1822.5, 1822.5], [1855.0, 1855.0, 1865.0, 1865.0], [1845.0, 1845.0, 1860.0, 1860.0], [1885.0, 1885.0, 1895.0, 1895.0], [1875.0, 1875.0, 1890.0, 1890.0], [1852.5, 1852.5, 1882.5, 1882.5], [1935.0, 1935.0, 1945.0, 1945.0], [1925.0, 1925.0, 1940.0, 1940.0], [1915.0, 1915.0, 1932.5, 1932.5], [1905.0, 1905.0, 1923.75, 1923.75], [1867.5, 1867.5, 1914.375, 1914.375], [1803.125, 1803.125, 1890.9375, 1890.9375], [1750.0, 1750.0, 1847.03125, 1847.03125], [1955.0, 1955.0, 1965.0, 1965.0], [1975.0, 1975.0, 1985.0, 1985.0], [1995.0, 1995.0, 2005.0, 2005.0], [2025.0, 2025.0, 2035.0, 2035.0], [2015.0, 2015.0, 2030.0, 2030.0], [2000.0, 2000.0, 2022.5, 2022.5], [1980.0, 1980.0, 2011.25, 2011.25], [1960.0, 1960.0, 1995.625, 1995.625], [2075.0, 2075.0, 2085.0, 2085.0], [2065.0, 2065.0, 2080.0, 2080.0], [2055.0, 2055.0, 2072.5, 2072.5], [2045.0, 2045.0, 2063.75, 2063.75], [2105.0, 2105.0, 2115.0, 2115.0], [2135.0, 2135.0, 2145.0, 2145.0], [2125.0, 2125.0, 2140.0, 2140.0], [2155.0, 2155.0, 2165.0, 2165.0], [2132.5, 2132.5, 2160.0, 2160.0], [2110.0, 2110.0, 2146.25, 2146.25], [2095.0, 2095.0, 2128.125, 2128.125], [2054.375, 2054.375, 2111.5625, 2111.5625], [1977.8125, 1977.8125, 2082.96875, 2082.96875], [1798.515625, 1798.515625, 2030.390625, 2030.390625], [2185.0, 2185.0, 2195.0, 2195.0], [2205.0, 2205.0, 2215.0, 2215.0], [2235.0, 2235.0, 2245.0, 2245.0], [2225.0, 2225.0, 2240.0, 2240.0], [2210.0, 2210.0, 2232.5, 2232.5], [2190.0, 2190.0, 2221.25, 2221.25], [2265.0, 2265.0, 2275.0, 2275.0], [2255.0, 2255.0, 2270.0, 2270.0], [2295.0, 2295.0, 2305.0, 2305.0], [2285.0, 2285.0, 2300.0, 2300.0], [2262.5, 2262.5, 2292.5, 2292.5], [2205.625, 2205.625, 2277.5, 2277.5], [2175.0, 2175.0, 2241.5625, 2241.5625], [2315.0, 2315.0, 2325.0, 2325.0], [2335.0, 2335.0, 2345.0, 2345.0], [2320.0, 2320.0, 2340.0, 2340.0], [2385.0, 2385.0, 2395.0, 2395.0], [2375.0, 2375.0, 2390.0, 2390.0], [2365.0, 2365.0, 2382.5, 2382.5], [2355.0, 2355.0, 2373.75, 2373.75], [2405.0, 2405.0, 2415.0, 2415.0], [2425.0, 2425.0, 2435.0, 2435.0], [2410.0, 2410.0, 2430.0, 2430.0], [2455.0, 2455.0, 2465.0, 2465.0], [2445.0, 2445.0, 2460.0, 2460.0], [2475.0, 2475.0, 2485.0, 2485.0], [2452.5, 2452.5, 2480.0, 2480.0], [2505.0, 2505.0, 2515.0, 2515.0], [2495.0, 2495.0, 2510.0, 2510.0], [2535.0, 2535.0, 2545.0, 2545.0], [2525.0, 2525.0, 2540.0, 2540.0], [2502.5, 2502.5, 2532.5, 2532.5], [2466.25, 2466.25, 2517.5, 2517.5], [2420.0, 2420.0, 2491.875, 2491.875], [2364.375, 2364.375, 2455.9375, 2455.9375], [2330.0, 2330.0, 2410.15625, 2410.15625], [2208.28125, 2208.28125, 2370.078125, 2370.078125], [1914.453125, 1914.453125, 2289.1796875, 2289.1796875], [1703.75, 1703.75, 2101.81640625, 2101.81640625], [1315.1025390625, 1315.1025390625, 1902.783203125, 1902.783203125], [2565.0, 2565.0, 2575.0, 2575.0], [2555.0, 2555.0, 2570.0, 2570.0], [2595.0, 2595.0, 2605.0, 2605.0], [2585.0, 2585.0, 2600.0, 2600.0], [2625.0, 2625.0, 2635.0, 2635.0], [2615.0, 2615.0, 2630.0, 2630.0], [2665.0, 2665.0, 2675.0, 2675.0], [2655.0, 2655.0, 2670.0, 2670.0], [2645.0, 2645.0, 2662.5, 2662.5], [2622.5, 2622.5, 2653.75, 2653.75], [2592.5, 2592.5, 2638.125, 2638.125], [2562.5, 2562.5, 2615.3125, 2615.3125], [2685.0, 2685.0, 2695.0, 2695.0], [2725.0, 2725.0, 2735.0, 2735.0], [2715.0, 2715.0, 2730.0, 2730.0], [2705.0, 2705.0, 2722.5, 2722.5], [2690.0, 2690.0, 2713.75, 2713.75], [2755.0, 2755.0, 2765.0, 2765.0], [2745.0, 2745.0, 2760.0, 2760.0], [2775.0, 2775.0, 2785.0, 2785.0], [2815.0, 2815.0, 2825.0, 2825.0], [2805.0, 2805.0, 2820.0, 2820.0], [2795.0, 2795.0, 2812.5, 2812.5], [2780.0, 2780.0, 2803.75, 2803.75], [2752.5, 2752.5, 2791.875, 2791.875], [2701.875, 2701.875, 2772.1875, 2772.1875], [2855.0, 2855.0, 2865.0, 2865.0], [2845.0, 2845.0, 2860.0, 2860.0], [2835.0, 2835.0, 2852.5, 2852.5], [2885.0, 2885.0, 2895.0, 2895.0], [2875.0, 2875.0, 2890.0, 2890.0], [2915.0, 2915.0, 2925.0, 2925.0], [2905.0, 2905.0, 2920.0, 2920.0], [2882.5, 2882.5, 2912.5, 2912.5], [2935.0, 2935.0, 2945.0, 2945.0], [2965.0, 2965.0, 2975.0, 2975.0], [2955.0, 2955.0, 2970.0, 2970.0], [2995.0, 2995.0, 3005.0, 3005.0], [2985.0, 2985.0, 3000.0, 3000.0], [2962.5, 2962.5, 2992.5, 2992.5], [2940.0, 2940.0, 2977.5, 2977.5], [3025.0, 3025.0, 3035.0, 3035.0], [3015.0, 3015.0, 3030.0, 3030.0], [2958.75, 2958.75, 3022.5, 3022.5], [2897.5, 2897.5, 2990.625, 2990.625], [2843.75, 2843.75, 2944.0625, 2944.0625], [2737.03125, 2737.03125, 2893.90625, 2893.90625], [3045.0, 3045.0, 3055.0, 3055.0], [3075.0, 3075.0, 3085.0, 3085.0], [3065.0, 3065.0, 3080.0, 3080.0], [3095.0, 3095.0, 3105.0, 3105.0], [3115.0, 3115.0, 3125.0, 3125.0], [3145.0, 3145.0, 3155.0, 3155.0], [3135.0, 3135.0, 3150.0, 3150.0], [3120.0, 3120.0, 3142.5, 3142.5], [3100.0, 3100.0, 3131.25, 3131.25], [3185.0, 3185.0, 3195.0, 3195.0], [3175.0, 3175.0, 3190.0, 3190.0], [3165.0, 3165.0, 3182.5, 3182.5], [3205.0, 3205.0, 3215.0, 3215.0], [3225.0, 3225.0, 3235.0, 3235.0], [3245.0, 3245.0, 3255.0, 3255.0], [3230.0, 3230.0, 3250.0, 3250.0], [3265.0, 3265.0, 3275.0, 3275.0], [3240.0, 3240.0, 3270.0, 3270.0], [3210.0, 3210.0, 3255.0, 3255.0], [3173.75, 3173.75, 3232.5, 3232.5], [3115.625, 3115.625, 3203.125, 3203.125], [3072.5, 3072.5, 3159.375, 3159.375], [3050.0, 3050.0, 3115.9375, 3115.9375], [2815.46875, 2815.46875, 3082.96875, 3082.96875], [2588.90625, 2588.90625, 2949.21875, 2949.21875], [3285.0, 3285.0, 3295.0, 3295.0], [3325.0, 3325.0, 3335.0, 3335.0], [3315.0, 3315.0, 3330.0, 3330.0], [3305.0, 3305.0, 3322.5, 3322.5], [3355.0, 3355.0, 3365.0, 3365.0], [3345.0, 3345.0, 3360.0, 3360.0], [3313.75, 3313.75, 3352.5, 3352.5], [3290.0, 3290.0, 3333.125, 3333.125], [3395.0, 3395.0, 3405.0, 3405.0], [3385.0, 3385.0, 3400.0, 3400.0], [3375.0, 3375.0, 3392.5, 3392.5], [3311.5625, 3311.5625, 3383.75, 3383.75], [3435.0, 3435.0, 3445.0, 3445.0], [3425.0, 3425.0, 3440.0, 3440.0], [3455.0, 3455.0, 3465.0, 3465.0], [3475.0, 3475.0, 3485.0, 3485.0], [3460.0, 3460.0, 3480.0, 3480.0], [3432.5, 3432.5, 3470.0, 3470.0], [3495.0, 3495.0, 3505.0, 3505.0], [3525.0, 3525.0, 3535.0, 3535.0], [3515.0, 3515.0, 3530.0, 3530.0], [3545.0, 3545.0, 3555.0, 3555.0], [3522.5, 3522.5, 3550.0, 3550.0], [3500.0, 3500.0, 3536.25, 3536.25], [3451.25, 3451.25, 3518.125, 3518.125], [3415.0, 3415.0, 3484.6875, 3484.6875], [3575.0, 3575.0, 3585.0, 3585.0], [3565.0, 3565.0, 3580.0, 3580.0], [3449.84375, 3449.84375, 3572.5, 3572.5], [3347.65625, 3347.65625, 3511.171875, 3511.171875], [3595.0, 3595.0, 3605.0, 3605.0], [3635.0, 3635.0, 3645.0, 3645.0], [3625.0, 3625.0, 3640.0, 3640.0], [3615.0, 3615.0, 3632.5, 3632.5], [3600.0, 3600.0, 3623.75, 3623.75], [3675.0, 3675.0, 3685.0, 3685.0], [3665.0, 3665.0, 3680.0, 3680.0], [3655.0, 3655.0, 3672.5, 3672.5], [3695.0, 3695.0, 3705.0, 3705.0], [3663.75, 3663.75, 3700.0, 3700.0], [3735.0, 3735.0, 3745.0, 3745.0], [3725.0, 3725.0, 3740.0, 3740.0], [3775.0, 3775.0, 3785.0, 3785.0], [3805.0, 3805.0, 3815.0, 3815.0], [3795.0, 3795.0, 3810.0, 3810.0], [3780.0, 3780.0, 3802.5, 3802.5], [3765.0, 3765.0, 3791.25, 3791.25], [3825.0, 3825.0, 3835.0, 3835.0], [3845.0, 3845.0, 3855.0, 3855.0], [3865.0, 3865.0, 3875.0, 3875.0], [3885.0, 3885.0, 3895.0, 3895.0], [3870.0, 3870.0, 3890.0, 3890.0], [3850.0, 3850.0, 3880.0, 3880.0], [3830.0, 3830.0, 3865.0, 3865.0], [3778.125, 3778.125, 3847.5, 3847.5], [3755.0, 3755.0, 3812.8125, 3812.8125], [3732.5, 3732.5, 3783.90625, 3783.90625], [3715.0, 3715.0, 3758.203125, 3758.203125], [3681.875, 3681.875, 3736.6015625, 3736.6015625], [3611.875, 3611.875, 3709.23828125, 3709.23828125], [3429.4140625, 3429.4140625, 3660.556640625, 3660.556640625], [2769.0625, 2769.0625, 3544.9853515625, 3544.9853515625], [1608.94287109375, 1608.94287109375, 3157.02392578125, 3157.02392578125], [388.39599609375, 388.39599609375, 2382.9833984375, 2382.9833984375]], 'dcoord': [[0.0, 4.288467622063064, 4.288467622063064, 0.0], [0.0, 2.1444173091433503, 2.1444173091433503, 0.0], [0.0, 3.209445854461416, 3.209445854461416, 0.0], [2.1444173091433503, 4.541220543492209, 4.541220543492209, 3.209445854461416], [4.288467622063064, 10.770424321953634, 10.770424321953634, 4.541220543492209], [0.0, 1.0077756450474236, 1.0077756450474236, 0.0], [0.0, 1.6623854092731738, 1.6623854092731738, 0.0], [0.0, 4.133970156485949, 4.133970156485949, 1.6623854092731738], [1.0077756450474236, 6.150567657112124, 6.150567657112124, 4.133970156485949], [0.0, 2.5415923066169754, 2.5415923066169754, 0.0], [0.0, 3.1254343009623087, 3.1254343009623087, 2.5415923066169754], [0.0, 1.5578568910137298, 1.5578568910137298, 0.0], [0.0, 3.207110030269233, 3.207110030269233, 1.5578568910137298], [3.1254343009623087, 4.60247561841998, 4.60247561841998, 3.207110030269233], [0.0, 5.487109312861481, 5.487109312861481, 4.60247561841998], [0.0, 0.5522766660791142, 0.5522766660791142, 0.0], [0.0, 1.6949394160299507, 1.6949394160299507, 0.5522766660791142], [0.0, 2.9794146272789703, 2.9794146272789703, 1.6949394160299507], [0.0, 1.317177387882779, 1.317177387882779, 0.0], [0.0, 1.5757943932714247, 1.5757943932714247, 1.317177387882779], [0.0, 0.4326568226317671, 0.4326568226317671, 0.0], [0.0, 0.8872332946761838, 0.8872332946761838, 0.4326568226317671], [0.0, 1.89109303004293, 1.89109303004293, 0.8872332946761838], [1.5757943932714247, 3.426531585327519, 3.426531585327519, 1.89109303004293], [0.0, 4.686602443038697, 4.686602443038697, 3.426531585327519], [2.9794146272789703, 5.9432515543668885, 5.9432515543668885, 4.686602443038697], [0.0, 2.7573186995956873, 2.7573186995956873, 0.0], [0.0, 0.6773618666706023, 0.6773618666706023, 0.0], [0.0, 0.7170450075701397, 0.7170450075701397, 0.0], [0.6773618666706023, 1.0445926164434403, 1.0445926164434403, 0.7170450075701397], [0.0, 1.0775809755347774, 1.0775809755347774, 0.0], [0.0, 1.1546530777308524, 1.1546530777308524, 1.0775809755347774], [1.0445926164434403, 2.475094391339738, 2.475094391339738, 1.1546530777308524], [0.0, 1.1268682226678588, 1.1268682226678588, 0.0], [0.0, 1.004276965605074, 1.004276965605074, 0.0], [0.0, 1.251580343374809, 1.251580343374809, 1.004276965605074], [1.1268682226678588, 2.4945014397147403, 2.4945014397147403, 1.251580343374809], [2.475094391339738, 4.755414174074465, 4.755414174074465, 2.4945014397147403], [2.7573186995956873, 6.646271342220689, 6.646271342220689, 4.755414174074465], [5.9432515543668885, 8.903517840063106, 8.903517840063106, 6.646271342220689], [0.0, 12.201951284802112, 12.201951284802112, 8.903517840063106], [5.487109312861481, 14.84599780304138, 14.84599780304138, 12.201951284802112], [6.150567657112124, 27.66908022308275, 27.66908022308275, 14.84599780304138], [10.770424321953634, 32.26225974053063, 32.26225974053063, 27.66908022308275], [0.0, 1.3255114391480807, 1.3255114391480807, 0.0], [0.0, 1.8307524669539068, 1.8307524669539068, 1.3255114391480807], [0.0, 7.524476965574057, 7.524476965574057, 1.8307524669539068], [0.0, 2.3128972181235503, 2.3128972181235503, 0.0], [0.0, 1.425389996658467, 1.425389996658467, 0.0], [0.0, 2.311019599679912, 2.311019599679912, 0.0], [1.425389996658467, 2.7201323242939703, 2.7201323242939703, 2.311019599679912], [0.0, 3.39554589717662, 3.39554589717662, 2.7201323242939703], [2.3128972181235503, 4.596703780745637, 4.596703780745637, 3.39554589717662], [0.0, 2.7510222392481882, 2.7510222392481882, 0.0], [0.0, 3.536591326463855, 3.536591326463855, 2.7510222392481882], [0.0, 4.044478109601785, 4.044478109601785, 0.0], [3.536591326463855, 6.055401267546618, 6.055401267546618, 4.044478109601785], [4.596703780745637, 8.644508792091678, 8.644508792091678, 6.055401267546618], [7.524476965574057, 11.145230902295658, 11.145230902295658, 8.644508792091678], [0.0, 2.032305459384274, 2.032305459384274, 0.0], [0.0, 2.926244937805482, 2.926244937805482, 2.032305459384274], [0.0, 7.029514145693679, 7.029514145693679, 2.926244937805482], [0.0, 1.9163299514734065, 1.9163299514734065, 0.0], [0.0, 0.8459300297679179, 0.8459300297679179, 0.0], [0.0, 1.666911145524231, 1.666911145524231, 0.8459300297679179], [0.0, 1.0318177213611763, 1.0318177213611763, 0.0], [0.0, 1.9205730854503105, 1.9205730854503105, 1.0318177213611763], [0.0, 2.47955938147674, 2.47955938147674, 1.9205730854503105], [1.666911145524231, 2.8327632390390005, 2.8327632390390005, 2.47955938147674], [1.9163299514734065, 4.238652281346471, 4.238652281346471, 2.8327632390390005], [0.0, 1.5859641551209258, 1.5859641551209258, 0.0], [0.0, 1.7418137302711907, 1.7418137302711907, 0.0], [0.0, 3.157321894835179, 3.157321894835179, 1.7418137302711907], [1.5859641551209258, 3.539079427778799, 3.539079427778799, 3.157321894835179], [0.0, 2.216959303144719, 2.216959303144719, 0.0], [0.0, 1.5685901085582594, 1.5685901085582594, 0.0], [0.0, 1.7585155381762803, 1.7585155381762803, 0.0], [1.5685901085582594, 2.439036904606122, 2.439036904606122, 1.7585155381762803], [2.216959303144719, 3.6045926601659914, 3.6045926601659914, 2.439036904606122], [3.539079427778799, 6.426550980576329, 6.426550980576329, 3.6045926601659914], [4.238652281346471, 6.8791735606756435, 6.8791735606756435, 6.426550980576329], [0.0, 2.3598632844730565, 2.3598632844730565, 0.0], [0.0, 1.6366405063250027, 1.6366405063250027, 0.0], [0.0, 0.8760803812888682, 0.8760803812888682, 0.0], [0.0, 2.463159254000934, 2.463159254000934, 0.8760803812888682], [1.6366405063250027, 2.611482717510167, 2.611482717510167, 2.463159254000934], [2.3598632844730565, 4.325008467577852, 4.325008467577852, 2.611482717510167], [0.0, 3.421715021071728, 3.421715021071728, 0.0], [0.0, 2.4990613591715416, 2.4990613591715416, 0.0], [0.0, 3.7022787384957296, 3.7022787384957296, 2.4990613591715416], [3.421715021071728, 5.652852847327917, 5.652852847327917, 3.7022787384957296], [4.325008467577852, 9.126989501003157, 9.126989501003157, 5.652852847327917], [6.8791735606756435, 11.130189483298423, 11.130189483298423, 9.126989501003157], [7.029514145693679, 15.83367253390864, 15.83367253390864, 11.130189483298423], [0.0, 2.0077140610272997, 2.0077140610272997, 0.0], [0.0, 1.5281566696635402, 1.5281566696635402, 0.0], [0.0, 1.2369260630212335, 1.2369260630212335, 0.0], [0.0, 1.7736751729639368, 1.7736751729639368, 1.2369260630212335], [1.5281566696635402, 2.068439874557458, 2.068439874557458, 1.7736751729639368], [0.0, 2.132102259708784, 2.132102259708784, 0.0], [2.068439874557458, 3.9152291727663013, 3.9152291727663013, 2.132102259708784], [2.0077140610272997, 4.7480704628910715, 4.7480704628910715, 3.9152291727663013], [0.0, 1.3058996424618508, 1.3058996424618508, 0.0], [0.0, 2.016068693885154, 2.016068693885154, 0.0], [1.3058996424618508, 3.7726217687011134, 3.7726217687011134, 2.016068693885154], [0.0, 2.305834023243653, 2.305834023243653, 0.0], [0.0, 1.1262441616182233, 1.1262441616182233, 0.0], [0.0, 1.749763058597228, 1.749763058597228, 1.1262441616182233], [0.0, 1.3020227890929241, 1.3020227890929241, 0.0], [0.0, 2.0538235375109295, 2.0538235375109295, 1.3020227890929241], [1.749763058597228, 2.9011224440923296, 2.9011224440923296, 2.0538235375109295], [2.305834023243653, 3.8912012142642576, 3.8912012142642576, 2.9011224440923296], [3.7726217687011134, 7.784085626770371, 7.784085626770371, 3.8912012142642576], [4.7480704628910715, 15.979076413628007, 15.979076413628007, 7.784085626770371], [15.83367253390864, 25.530158447908295, 25.530158447908295, 15.979076413628007], [11.145230902295658, 44.21882282221781, 44.21882282221781, 25.530158447908295], [32.26225974053063, 55.28841091000626, 55.28841091000626, 44.21882282221781], [0.0, 2.7672120866240633, 2.7672120866240633, 0.0], [0.0, 1.1853073940884835, 1.1853073940884835, 0.0], [0.0, 3.4620217005208964, 3.4620217005208964, 1.1853073940884835], [2.7672120866240633, 8.506941849079102, 8.506941849079102, 3.4620217005208964], [0.0, 1.939565048136895, 1.939565048136895, 0.0], [0.0, 3.611017245927708, 3.611017245927708, 1.939565048136895], [0.0, 0.6647702773978306, 0.6647702773978306, 0.0], [0.0, 0.2799007436731644, 0.2799007436731644, 0.0], [0.0, 0.5253909809165592, 0.5253909809165592, 0.2799007436731644], [0.0, 0.8695866136510783, 0.8695866136510783, 0.5253909809165592], [0.6647702773978306, 1.0548072826282986, 1.0548072826282986, 0.8695866136510783], [0.0, 0.6893812204855896, 0.6893812204855896, 0.0], [0.0, 0.9752106529920564, 0.9752106529920564, 0.6893812204855896], [0.0, 1.6853569249365943, 1.6853569249365943, 0.9752106529920564], [1.0548072826282986, 2.082513694621207, 2.082513694621207, 1.6853569249365943], [0.0, 2.3457262873142133, 2.3457262873142133, 0.0], [2.082513694621207, 6.491628213227621, 6.491628213227621, 2.3457262873142133], [3.611017245927708, 7.923619167140435, 7.923619167140435, 6.491628213227621], [0.0, 1.08954084421352, 1.08954084421352, 0.0], [0.0, 0.7038056240918243, 0.7038056240918243, 0.0], [0.0, 0.996611171603652, 0.996611171603652, 0.7038056240918243], [0.0, 0.6790072733675424, 0.6790072733675424, 0.0], [0.0, 0.34810639427833173, 0.34810639427833173, 0.0], [0.0, 0.592495566204974, 0.592495566204974, 0.34810639427833173], [0.0, 0.7255560975598297, 0.7255560975598297, 0.592495566204974], [0.0, 1.1459143956723823, 1.1459143956723823, 0.7255560975598297], [0.6790072733675424, 1.4798284897514593, 1.4798284897514593, 1.1459143956723823], [0.996611171603652, 2.1097083497042233, 2.1097083497042233, 1.4798284897514593], [1.08954084421352, 2.5697122392933243, 2.5697122392933243, 2.1097083497042233], [0.0, 1.2762045976244307, 1.2762045976244307, 0.0], [0.0, 0.552894185450155, 0.552894185450155, 0.0], [0.0, 0.8809147030869958, 0.8809147030869958, 0.552894185450155], [0.0, 1.0878042985390393, 1.0878042985390393, 0.0], [0.8809147030869958, 1.3271700108239612, 1.3271700108239612, 1.0878042985390393], [0.0, 2.16941868001748, 2.16941868001748, 1.3271700108239612], [1.2762045976244307, 2.49609980774248, 2.49609980774248, 2.16941868001748], [0.0, 0.5115182128573974, 0.5115182128573974, 0.0], [0.0, 0.6517468696296287, 0.6517468696296287, 0.0], [0.5115182128573974, 1.1927519182876605, 1.1927519182876605, 0.6517468696296287], [0.0, 2.649369966828889, 2.649369966828889, 1.1927519182876605], [2.49609980774248, 4.183813973757155, 4.183813973757155, 2.649369966828889], [2.5697122392933243, 6.032808839645179, 6.032808839645179, 4.183813973757155], [0.0, 1.4196494368908734, 1.4196494368908734, 0.0], [0.0, 1.3385893263037354, 1.3385893263037354, 0.0], [0.0, 1.5164295451980891, 1.5164295451980891, 1.3385893263037354], [1.4196494368908734, 3.489403700455003, 3.489403700455003, 1.5164295451980891], [0.0, 6.497736156093032, 6.497736156093032, 3.489403700455003], [6.032808839645179, 10.820157473308331, 10.820157473308331, 6.497736156093032], [7.923619167140435, 12.807783961712078, 12.807783961712078, 10.820157473308331], [8.506941849079102, 17.322800911420327, 17.322800911420327, 12.807783961712078], [0.0, 7.405961570981256, 7.405961570981256, 0.0], [0.0, 11.48553816413072, 11.48553816413072, 7.405961570981256], [0.0, 16.733596960880607, 16.733596960880607, 11.48553816413072], [0.0, 1.362159933523286, 1.362159933523286, 0.0], [0.0, 1.5286013218399412, 1.5286013218399412, 0.0], [1.362159933523286, 2.7452292767396913, 2.7452292767396913, 1.5286013218399412], [0.0, 0.8701386058374766, 0.8701386058374766, 0.0], [0.0, 1.001210158093924, 1.001210158093924, 0.8701386058374766], [0.0, 1.2195068762819667, 1.2195068762819667, 1.001210158093924], [0.0, 1.1102347449234433, 1.1102347449234433, 0.0], [0.0, 1.2918307891622574, 1.2918307891622574, 1.1102347449234433], [1.2195068762819667, 2.334171301180848, 2.334171301180848, 1.2918307891622574], [0.0, 0.6470908567417303, 0.6470908567417303, 0.0], [0.0, 1.0797839389900814, 1.0797839389900814, 0.6470908567417303], [0.0, 0.6614780958428782, 0.6614780958428782, 0.0], [0.0, 1.2708292559541716, 1.2708292559541716, 0.6614780958428782], [1.0797839389900814, 1.6527558483661065, 1.6527558483661065, 1.2708292559541716], [0.0, 0.5448968396930801, 0.5448968396930801, 0.0], [0.0, 0.8005873155792426, 0.8005873155792426, 0.5448968396930801], [0.0, 1.0024940680908114, 1.0024940680908114, 0.8005873155792426], [0.0, 1.9288427044636047, 1.9288427044636047, 1.0024940680908114], [1.6527558483661065, 3.4537736913519983, 3.4537736913519983, 1.9288427044636047], [2.334171301180848, 4.128060109656243, 4.128060109656243, 3.4537736913519983], [2.7452292767396913, 4.709248910552457, 4.709248910552457, 4.128060109656243], [0.0, 1.2313356058332727, 1.2313356058332727, 0.0], [0.0, 0.935730926184198, 0.935730926184198, 0.0], [0.0, 0.466135920054514, 0.466135920054514, 0.0], [0.0, 0.58755719908933, 0.58755719908933, 0.0], [0.0, 0.7126065130098831, 0.7126065130098831, 0.58755719908933], [0.466135920054514, 1.201319695394624, 1.201319695394624, 0.7126065130098831], [0.935730926184198, 1.5274054555568863, 1.5274054555568863, 1.201319695394624], [1.2313356058332727, 1.9227518897704776, 1.9227518897704776, 1.5274054555568863], [0.0, 1.2992949422103919, 1.2992949422103919, 0.0], [0.0, 1.4924580072637506, 1.4924580072637506, 1.2992949422103919], [0.0, 2.2064149715602035, 2.2064149715602035, 1.4924580072637506], [0.0, 2.9081409854136537, 2.9081409854136537, 2.2064149715602035], [0.0, 1.1256536727986564, 1.1256536727986564, 0.0], [0.0, 0.37332919173869467, 0.37332919173869467, 0.0], [0.0, 0.5610546916782294, 0.5610546916782294, 0.37332919173869467], [0.0, 0.8752409902834086, 0.8752409902834086, 0.0], [0.5610546916782294, 1.9790169468849332, 1.9790169468849332, 0.8752409902834086], [1.1256536727986564, 2.100258003478658, 2.100258003478658, 1.9790169468849332], [0.0, 3.2400985251501226, 3.2400985251501226, 2.100258003478658], [2.9081409854136537, 4.262818563494461, 4.262818563494461, 3.2400985251501226], [1.9227518897704776, 5.220339198178567, 5.220339198178567, 4.262818563494461], [4.709248910552457, 9.789059215149718, 9.789059215149718, 5.220339198178567], [0.0, 0.9804599092566584, 0.9804599092566584, 0.0], [0.0, 0.5952130667044138, 0.5952130667044138, 0.0], [0.0, 0.5559577015081576, 0.5559577015081576, 0.0], [0.0, 0.694618135355322, 0.694618135355322, 0.5559577015081576], [0.5952130667044138, 1.1069705014088185, 1.1069705014088185, 0.694618135355322], [0.9804599092566584, 1.5060513150108896, 1.5060513150108896, 1.1069705014088185], [0.0, 0.8968785796116732, 0.8968785796116732, 0.0], [0.0, 1.569870473891478, 1.569870473891478, 0.8968785796116732], [0.0, 1.2293022865961363, 1.2293022865961363, 0.0], [0.0, 1.8035448445547804, 1.8035448445547804, 1.2293022865961363], [1.569870473891478, 2.9730396329647397, 2.9730396329647397, 1.8035448445547804], [1.5060513150108896, 3.707984793416065, 3.707984793416065, 2.9730396329647397], [0.0, 5.171047914987279, 5.171047914987279, 3.707984793416065], [0.0, 1.534351017034192, 1.534351017034192, 0.0], [0.0, 1.6209630279725786, 1.6209630279725786, 0.0], [1.534351017034192, 3.013563593614301, 3.013563593614301, 1.6209630279725786], [0.0, 1.02684830762653, 1.02684830762653, 0.0], [0.0, 1.38437529728383, 1.38437529728383, 1.02684830762653], [0.0, 2.4223425008558634, 2.4223425008558634, 1.38437529728383], [0.0, 3.2629925426918454, 3.2629925426918454, 2.4223425008558634], [0.0, 1.115271875167296, 1.115271875167296, 0.0], [0.0, 1.3887841398087413, 1.3887841398087413, 0.0], [1.115271875167296, 2.6440366537615954, 2.6440366537615954, 1.3887841398087413], [0.0, 0.5427808835199546, 0.5427808835199546, 0.0], [0.0, 0.8079580883639083, 0.8079580883639083, 0.5427808835199546], [0.0, 0.9829104041367441, 0.9829104041367441, 0.0], [0.8079580883639083, 1.580650095028237, 1.580650095028237, 0.9829104041367441], [0.0, 0.6765526774349127, 0.6765526774349127, 0.0], [0.0, 0.9039073223988655, 0.9039073223988655, 0.6765526774349127], [0.0, 1.1701613685120391, 1.1701613685120391, 0.0], [0.0, 1.458118984867185, 1.458118984867185, 1.1701613685120391], [0.9039073223988655, 1.8261770077785715, 1.8261770077785715, 1.458118984867185], [1.580650095028237, 2.7528332927429138, 2.7528332927429138, 1.8261770077785715], [2.6440366537615954, 3.6497074309259663, 3.6497074309259663, 2.7528332927429138], [3.2629925426918454, 5.764107276088408, 5.764107276088408, 3.6497074309259663], [3.013563593614301, 8.369054344909904, 8.369054344909904, 5.764107276088408], [5.171047914987279, 10.931537857063079, 10.931537857063079, 8.369054344909904], [9.789059215149718, 18.663216847355255, 18.663216847355255, 10.931537857063079], [16.733596960880607, 35.64369428555948, 35.64369428555948, 18.663216847355255], [17.322800911420327, 36.66686131938344, 36.66686131938344, 35.64369428555948], [0.0, 1.9170088265861767, 1.9170088265861767, 0.0], [0.0, 2.2568400536091517, 2.2568400536091517, 1.9170088265861767], [0.0, 0.712643635576153, 0.712643635576153, 0.0], [0.0, 1.3412333909245366, 1.3412333909245366, 0.712643635576153], [0.0, 0.4205340766856073, 0.4205340766856073, 0.0], [0.0, 0.7942677256038867, 0.7942677256038867, 0.4205340766856073], [0.0, 0.597682709344215, 0.597682709344215, 0.0], [0.0, 0.9307642803480662, 0.9307642803480662, 0.597682709344215], [0.0, 1.8978585549011122, 1.8978585549011122, 0.9307642803480662], [0.7942677256038867, 1.9864033128356142, 1.9864033128356142, 1.8978585549011122], [1.3412333909245366, 2.987843111961701, 2.987843111961701, 1.9864033128356142], [2.2568400536091517, 3.82859087807365, 3.82859087807365, 2.987843111961701], [0.0, 1.2793943495945492, 1.2793943495945492, 0.0], [0.0, 0.8246284763106778, 0.8246284763106778, 0.0], [0.0, 1.1441972628477322, 1.1441972628477322, 0.8246284763106778], [0.0, 1.4233521319322362, 1.4233521319322362, 1.1441972628477322], [1.2793943495945492, 2.3208499190448393, 2.3208499190448393, 1.4233521319322362], [0.0, 0.5589335283782179, 0.5589335283782179, 0.0], [0.0, 1.2921132492354739, 1.2921132492354739, 0.5589335283782179], [0.0, 1.0105926722961378, 1.0105926722961378, 0.0], [0.0, 0.6901425953088979, 0.6901425953088979, 0.0], [0.0, 1.0054464901559315, 1.0054464901559315, 0.6901425953088979], [0.0, 1.1818659555198423, 1.1818659555198423, 1.0054464901559315], [1.0105926722961378, 2.1710935420896473, 2.1710935420896473, 1.1818659555198423], [1.2921132492354739, 2.872203836118319, 2.872203836118319, 2.1710935420896473], [2.3208499190448393, 4.9939694443081315, 4.9939694443081315, 2.872203836118319], [0.0, 1.0512557049114866, 1.0512557049114866, 0.0], [0.0, 1.3447157530103286, 1.3447157530103286, 1.0512557049114866], [0.0, 2.6295474276166524, 2.6295474276166524, 1.3447157530103286], [0.0, 0.24473944054582364, 0.24473944054582364, 0.0], [0.0, 0.45554258726901803, 0.45554258726901803, 0.24473944054582364], [0.0, 0.5798573497684443, 0.5798573497684443, 0.0], [0.0, 1.1282707867569717, 1.1282707867569717, 0.5798573497684443], [0.45554258726901803, 1.8264497864136389, 1.8264497864136389, 1.1282707867569717], [0.0, 0.5583595237187302, 0.5583595237187302, 0.0], [0.0, 0.3467208811370047, 0.3467208811370047, 0.0], [0.0, 0.4827706106374387, 0.4827706106374387, 0.3467208811370047], [0.0, 0.47128603646613315, 0.47128603646613315, 0.0], [0.0, 0.7591348063230404, 0.7591348063230404, 0.47128603646613315], [0.4827706106374387, 1.115566124781893, 1.115566124781893, 0.7591348063230404], [0.5583595237187302, 1.2517441611769085, 1.2517441611769085, 1.115566124781893], [0.0, 0.43323222987071197, 0.43323222987071197, 0.0], [0.0, 1.3368087486707347, 1.3368087486707347, 0.43323222987071197], [1.2517441611769085, 2.2157531185333736, 2.2157531185333736, 1.3368087486707347], [1.8264497864136389, 2.7427220097663345, 2.7427220097663345, 2.2157531185333736], [2.6295474276166524, 5.482190711475707, 5.482190711475707, 2.7427220097663345], [4.9939694443081315, 6.681638938775081, 6.681638938775081, 5.482190711475707], [0.0, 2.875833227187629, 2.875833227187629, 0.0], [0.0, 1.07048948438643, 1.07048948438643, 0.0], [0.0, 2.064677600286575, 2.064677600286575, 1.07048948438643], [0.0, 0.48447672538710124, 0.48447672538710124, 0.0], [0.0, 0.24575771617655506, 0.24575771617655506, 0.0], [0.0, 0.3905499619612974, 0.3905499619612974, 0.0], [0.0, 0.6520400269103447, 0.6520400269103447, 0.3905499619612974], [0.24575771617655506, 0.9038776139418266, 0.9038776139418266, 0.6520400269103447], [0.48447672538710124, 1.82113873882037, 1.82113873882037, 0.9038776139418266], [0.0, 0.5105206040443556, 0.5105206040443556, 0.0], [0.0, 0.8007221530208753, 0.8007221530208753, 0.5105206040443556], [0.0, 1.1770452546803292, 1.1770452546803292, 0.8007221530208753], [0.0, 0.5466058066103135, 0.5466058066103135, 0.0], [0.0, 0.2721851778363883, 0.2721851778363883, 0.0], [0.0, 0.6174382585091027, 0.6174382585091027, 0.0], [0.2721851778363883, 0.7310285560539611, 0.7310285560539611, 0.6174382585091027], [0.0, 0.8374387970499939, 0.8374387970499939, 0.0], [0.7310285560539611, 1.222637672037537, 1.222637672037537, 0.8374387970499939], [0.5466058066103135, 1.5165393561407683, 1.5165393561407683, 1.222637672037537], [1.1770452546803292, 2.3949054313535467, 2.3949054313535467, 1.5165393561407683], [1.82113873882037, 3.075843970748059, 3.075843970748059, 2.3949054313535467], [2.064677600286575, 4.7384803340990915, 4.7384803340990915, 3.075843970748059], [2.875833227187629, 8.698185649570199, 8.698185649570199, 4.7384803340990915], [6.681638938775081, 10.798062359816774, 10.798062359816774, 8.698185649570199], [3.82859087807365, 13.653021760408992, 13.653021760408992, 10.798062359816774], [0.0, 0.9113564084340553, 0.9113564084340553, 0.0], [0.0, 0.3182215639198989, 0.3182215639198989, 0.0], [0.0, 0.5007910863753144, 0.5007910863753144, 0.3182215639198989], [0.0, 0.61679351126469, 0.61679351126469, 0.5007910863753144], [0.0, 0.36691530169304465, 0.36691530169304465, 0.0], [0.0, 0.7972869181033582, 0.7972869181033582, 0.36691530169304465], [0.61679351126469, 0.9988908624467764, 0.9988908624467764, 0.7972869181033582], [0.9113564084340553, 1.208591146012348, 1.208591146012348, 0.9988908624467764], [0.0, 0.5781193448392449, 0.5781193448392449, 0.0], [0.0, 0.7350484755226702, 0.7350484755226702, 0.5781193448392449], [0.0, 1.8238001675018087, 1.8238001675018087, 0.7350484755226702], [1.208591146012348, 2.8283738512136978, 2.8283738512136978, 1.8238001675018087], [0.0, 0.33897991056722704, 0.33897991056722704, 0.0], [0.0, 0.7778988197091821, 0.7778988197091821, 0.33897991056722704], [0.0, 0.5451544440722683, 0.5451544440722683, 0.0], [0.0, 0.9025768150971892, 0.9025768150971892, 0.0], [0.5451544440722683, 1.2393808879428234, 1.2393808879428234, 0.9025768150971892], [0.7778988197091821, 1.8698145792229517, 1.8698145792229517, 1.2393808879428234], [0.0, 0.6859643729230748, 0.6859643729230748, 0.0], [0.0, 0.30256563461126823, 0.30256563461126823, 0.0], [0.0, 0.8273619782269298, 0.8273619782269298, 0.30256563461126823], [0.0, 0.8344523334075006, 0.8344523334075006, 0.0], [0.8273619782269298, 1.3415766860451621, 1.3415766860451621, 0.8344523334075006], [0.6859643729230748, 2.2610180707155982, 2.2610180707155982, 1.3415766860451621], [1.8698145792229517, 2.577465487185614, 2.577465487185614, 2.2610180707155982], [0.0, 3.812449924838435, 3.812449924838435, 2.577465487185614], [0.0, 2.3051923874055733, 2.3051923874055733, 0.0], [0.0, 4.076624414122383, 4.076624414122383, 2.3051923874055733], [3.812449924838435, 5.695876298198831, 5.695876298198831, 4.076624414122383], [2.8283738512136978, 6.686032080886121, 6.686032080886121, 5.695876298198831], [0.0, 1.0986122886681098, 1.0986122886681098, 0.0], [0.0, 0.7982932116736386, 0.7982932116736386, 0.0], [0.0, 0.9939685690932899, 0.9939685690932899, 0.7982932116736386], [0.0, 2.025367679413338, 2.025367679413338, 0.9939685690932899], [1.0986122886681098, 3.015645269806098, 3.015645269806098, 2.025367679413338], [0.0, 0.49396124352532744, 0.49396124352532744, 0.0], [0.0, 0.6893011625297887, 0.6893011625297887, 0.49396124352532744], [0.0, 1.2096932893272965, 1.2096932893272965, 0.6893011625297887], [0.0, 1.286280203158211, 1.286280203158211, 0.0], [1.2096932893272965, 3.0063088525320905, 3.0063088525320905, 1.286280203158211], [0.0, 0.5981177848706493, 0.5981177848706493, 0.0], [0.0, 1.2321629120595303, 1.2321629120595303, 0.5981177848706493], [0.0, 0.7829211051931371, 0.7829211051931371, 0.0], [0.0, 0.6702939480742441, 0.6702939480742441, 0.0], [0.0, 1.3065574113130805, 1.3065574113130805, 0.6702939480742441], [0.7829211051931371, 1.32299108454364, 1.32299108454364, 1.3065574113130805], [0.0, 1.753879052009748, 1.753879052009748, 1.32299108454364], [0.0, 1.0288310359210902, 1.0288310359210902, 0.0], [0.0, 0.6811588893824703, 0.6811588893824703, 0.0], [0.0, 0.4776626830587823, 0.4776626830587823, 0.0], [0.0, 0.8000707998434515, 0.8000707998434515, 0.0], [0.4776626830587823, 0.9288968215345484, 0.9288968215345484, 0.8000707998434515], [0.6811588893824703, 1.2743784361809398, 1.2743784361809398, 0.9288968215345484], [1.0288310359210902, 1.8441400077263868, 1.8441400077263868, 1.2743784361809398], [1.753879052009748, 2.317637053218076, 2.317637053218076, 1.8441400077263868], [0.0, 2.536471076095505, 2.536471076095505, 2.317637053218076], [1.2321629120595303, 3.9837827382641158, 3.9837827382641158, 2.536471076095505], [0.0, 4.954226720458987, 4.954226720458987, 3.9837827382641158], [3.0063088525320905, 5.5298908773040125, 5.5298908773040125, 4.954226720458987], [3.015645269806098, 13.272007970417917, 13.272007970417917, 5.5298908773040125], [6.686032080886121, 20.833365285352787, 20.833365285352787, 13.272007970417917], [13.653021760408992, 41.24952151606013, 41.24952151606013, 20.833365285352787], [36.66686131938344, 85.16389480425576, 85.16389480425576, 41.24952151606013], [55.28841091000626, 145.34449705834737, 145.34449705834737, 85.16389480425576]], 'ivl': ['15', '76', '17', '61', '4', '18', '323', '325', '322', '324', '329', '67', '77', '68', '80', '321', '73', '364', '0', '71', '19', '59', '72', '348', '2', '328', '340', '69', '352', '26', '383', '62', '373', '6', '14', '27', '386', '3', '337', '351', '330', '360', '371', '63', '355', '82', '140', '139', '158', '162', '196', '157', '29', '32', '38', '85', '147', '152', '159', '88', '138', '153', '185', '197', '251', '111', '137', '105', '95', '165', '143', '202', '114', '201', '145', '180', '215', '42', '213', '21', '35', '144', '288', '160', '167', '22', '200', '193', '199', '150', '37', '184', '30', '131', '172', '151', '187', '110', '168', '161', '170', '97', '93', '108', '154', '169', '155', '198', '83', '163', '173', '210', '98', '84', '141', '34', '86', '164', '70', '326', '58', '16', '57', '8', '203', '204', '25', '372', '1', '361', '353', '354', '367', '368', '344', '359', '156', '369', '331', '387', '338', '7', '384', '381', '388', '342', '365', '349', '12', '385', '56', '358', '48', '356', '346', '370', '24', '332', '366', '11', '23', '335', '379', '64', '5', '81', '60', '28', '75', '327', '65', '66', '78', '317', '318', '294', '299', '102', '189', '120', '178', '221', '195', '291', '113', '100', '123', '104', '107', '304', '99', '55', '265', '45', '278', '205', '217', '272', '277', '176', '239', '181', '43', '211', '142', '179', '253', '175', '274', '194', '250', '254', '229', '47', '232', '146', '236', '94', '174', '219', '41', '182', '206', '40', '214', '207', '39', '212', '279', '177', '275', '33', '133', '132', '134', '148', '171', '149', '36', '188', '121', '191', '116', '183', '115', '91', '293', '186', '190', '31', '92', '106', '112', '53', '117', '130', '79', '363', '341', '334', '339', '378', '380', '389', '13', '343', '357', '362', '127', '283', '267', '52', '119', '125', '333', '44', '227', '290', '309', '103', '192', '87', '118', '136', '166', '96', '306', '259', '256', '257', '246', '244', '248', '231', '243', '320', '46', '223', '228', '230', '280', '89', '220', '238', '109', '287', '128', '49', '252', '249', '263', '218', '271', '209', '216', '247', '245', '264', '208', '234', '233', '316', '222', '237', '235', '273', '51', '258', '285', '314', '295', '315', '241', '319', '226', '270', '313', '303', '289', '90', '298', '54', '296', '284', '302', '224', '240', '124', '308', '260', '276', '50', '242', '266', '122', '261', '74', '336', '345', '312', '376', '20', '310', '129', '297', '281', '282', '101', '268', '269', '292', '286', '255', '350', '374', '135', '347', '305', '307', '9', '126', '225', '10', '375', '377', '382', '301', '311', '262', '300'], 'leaves': [15, 76, 17, 61, 4, 18, 323, 325, 322, 324, 329, 67, 77, 68, 80, 321, 73, 364, 0, 71, 19, 59, 72, 348, 2, 328, 340, 69, 352, 26, 383, 62, 373, 6, 14, 27, 386, 3, 337, 351, 330, 360, 371, 63, 355, 82, 140, 139, 158, 162, 196, 157, 29, 32, 38, 85, 147, 152, 159, 88, 138, 153, 185, 197, 251, 111, 137, 105, 95, 165, 143, 202, 114, 201, 145, 180, 215, 42, 213, 21, 35, 144, 288, 160, 167, 22, 200, 193, 199, 150, 37, 184, 30, 131, 172, 151, 187, 110, 168, 161, 170, 97, 93, 108, 154, 169, 155, 198, 83, 163, 173, 210, 98, 84, 141, 34, 86, 164, 70, 326, 58, 16, 57, 8, 203, 204, 25, 372, 1, 361, 353, 354, 367, 368, 344, 359, 156, 369, 331, 387, 338, 7, 384, 381, 388, 342, 365, 349, 12, 385, 56, 358, 48, 356, 346, 370, 24, 332, 366, 11, 23, 335, 379, 64, 5, 81, 60, 28, 75, 327, 65, 66, 78, 317, 318, 294, 299, 102, 189, 120, 178, 221, 195, 291, 113, 100, 123, 104, 107, 304, 99, 55, 265, 45, 278, 205, 217, 272, 277, 176, 239, 181, 43, 211, 142, 179, 253, 175, 274, 194, 250, 254, 229, 47, 232, 146, 236, 94, 174, 219, 41, 182, 206, 40, 214, 207, 39, 212, 279, 177, 275, 33, 133, 132, 134, 148, 171, 149, 36, 188, 121, 191, 116, 183, 115, 91, 293, 186, 190, 31, 92, 106, 112, 53, 117, 130, 79, 363, 341, 334, 339, 378, 380, 389, 13, 343, 357, 362, 127, 283, 267, 52, 119, 125, 333, 44, 227, 290, 309, 103, 192, 87, 118, 136, 166, 96, 306, 259, 256, 257, 246, 244, 248, 231, 243, 320, 46, 223, 228, 230, 280, 89, 220, 238, 109, 287, 128, 49, 252, 249, 263, 218, 271, 209, 216, 247, 245, 264, 208, 234, 233, 316, 222, 237, 235, 273, 51, 258, 285, 314, 295, 315, 241, 319, 226, 270, 313, 303, 289, 90, 298, 54, 296, 284, 302, 224, 240, 124, 308, 260, 276, 50, 242, 266, 122, 261, 74, 336, 345, 312, 376, 20, 310, 129, 297, 281, 282, 101, 268, 269, 292, 286, 255, 350, 374, 135, 347, 305, 307, 9, 126, 225, 10, 375, 377, 382, 301, 311, 262, 300], 'color_list': ['g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'g', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'r', 'b']}
plt.yscale('symlog')
plt.show()
```

<img src="man/figures/README-dendrogram-1.png" width="100%" />

## Elbow plot

``` python
# Elbow plot: number of instance in cluster by hyperparameter eps
count = []
for i in np.arange(0.01, 5, 0.01): 
    clustering = DBSCAN(eps= i).fit(dat_ghg1)
    a = clustering.labels_
    b = collections.Counter(a).get(-1)
    count.append(b)
plt.plot(pd.Series(np.arange(0.01, 5, 0.01)),count)
```

<img src="man/figures/README-elbow-1.png" width="100%" />

## DBSCAN at eps = 4

``` python
Cluster_ghg = DBSCAN(eps=4).fit(dat_ghg1)
cluster_labels = Cluster_ghg.labels_
```

## Visualize clusters

``` r
dat_ghg_total_co2e$Sector <- dat_ghg_total_co2e$Description
dat_ghg_total_co2e$labels <- py$cluster_labels %>% as.factor()
p <- ggplot(data = dat_ghg_total_co2e, 
            aes(text = paste("sub_Sector:", name_sub))) + 
  geom_point(aes(x = Sector, 
                 y = LnTotalgCO2e, 
                 color = labels)) +
  coord_flip() + 
  theme(text = element_text(size=8)); p
```

<img src="man/figures/README-cluster_point_plot-1.png" width="100%" />

(click
[HERE](https://weiquanluo.github.io/img/ghg_total_co2e_cluster.html) for
interactive plot)

# Discussion

# Final Note

The challenges of this project:

  - Manage data multicollinearity, heteroskedastic, highly right skewed.
  - Piping data processing, functional programming, statistic extraction
  - Communication between data processing in R and machine learning in
    python

# Exercise

Please, click [HERE]() to Exercise.
