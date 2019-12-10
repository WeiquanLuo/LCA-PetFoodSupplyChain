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

Let’s take a look at the visulization of plot with 5-dimemtions: Total
CO2 Equvivalent as the target variable (size) by Sectors (color), three
log10 scaled input resources as input variables (x,y,z) (
[HERE](https://weiquanluo.github.io/img/plotly_GHG_TotalCO2.html)).

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
# test: target_nm = "Total.g.CO2e",  X = resource
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

Here is the explanation of the `Total.t.CO2e`:

Global Warming Potential (GWP) is a weighting of greenhouse gas
emissions into the air from the production of each sector. Weighting
factors are 100-year GWP values from the IPCC Second Assessment Report
(IPCC 2001). t CO2e = metric tons of CO2 equivalent emissions.

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
dat_ghg_total_co2e$sector <- dat_ghg_total_co2e$Description
dat_ghg_total_co2e$labels <- py$cluster_labels %>% as.factor()
p <- ggplot(data = dat_ghg_total_co2e, 
            aes(text = paste("sub_Sector:", name_sub))) + 
  geom_point(aes(x = sector, 
                 y = LnTotalgCO2e, 
                 color = labels)) +
  coord_flip() + 
  theme(text = element_text(size=8)); p
```

<img src="man/figures/README-cluster_point_plot-1.png" width="100%" />

([HERE](https://weiquanluo.github.io/img/ghg_total_co2e_cluster.html)
for interactive plot)

Let’s take a look at the visulization of plot with 6-dimemtions: Total
CO2 Equvivalent as the target variable (size) by Sectors (color), three
log10 scaled input resources as input variables (x,y,z) by clusters
(shape) (
[HERE](https://weiquanluo.github.io/img/plotly_GHG_TotalCO2_wcluster.html)).

# Discussion

``` r
data %>% knitr::kable(format = "markdown") 
```

|     | Sector | Description                                                              | name\_sub                                                                                                              | Sector\_sub | Total.g.CO2e | Coal.MJ | Petrol.MJ | NatGase.MJ | labels |
| :-- | :----- | :----------------------------------------------------------------------- | :--------------------------------------------------------------------------------------------------------------------- | :---------- | -----------: | ------: | --------: | ---------: | :----- |
| 1   | 0      | Government and special                                                   | Government and special                                                                                                 | 0           |      3342280 |      92 |     34986 |      18688 | \-1    |
| 2   | 4      | Retail trade                                                             | Retail trade                                                                                                           | 4e+05       |        60079 |      18 |       573 |        388 | 0      |
| 3   | 48     | Transportation and Warehousing                                           | Transportation and Warehousing                                                                                         | 480000      |       804630 |      45 |      7489 |       6448 | 0      |
| 4   | 52     | Finance and Insurance                                                    | Finance and Insurance                                                                                                  | 520000      |       173860 |      25 |      1858 |        919 | 0      |
| 5   | 11     | Agriculture, Forestry, Fishing and Hunting                               | Animal Production                                                                                                      | 112000      |     31421963 |       1 |     56184 |       5251 | \-1    |
| 6   | 11     | Agriculture, Forestry, Fishing and Hunting                               | Forestry and Logging                                                                                                   | 113000      |        56594 |       1 |       719 |         44 | 0      |
| 7   | 52     | Finance and Insurance                                                    | Credit Intermediation and Related Activities                                                                           | 522000      |       115176 |       1 |      1126 |        797 | 0      |
| 8   | 53     | Real Estate and Rental and Leasing                                       | Rental and Leasing Services                                                                                            | 532000      |        23786 |       1 |       177 |        240 | 0      |
| 9   | 61     | Educational Services                                                     | Educational Services                                                                                                   | 611000      |       224185 |       3 |        78 |       4344 | 0      |
| 10  | 62     | Health Care and Social Assistance                                        | Ambulatory Health Care Services                                                                                        | 621000      |           94 |       1 |         2 |          2 | 0      |
| 11  | 62     | Health Care and Social Assistance                                        | Social Assistance                                                                                                      | 624000      |           47 |       1 |         1 |          2 | 0      |
| 12  | 71     | Arts, Entertainment, and Recreation                                      | Performing Arts, Spectator Sports, and Related Industries                                                              | 711000      |         6219 |       1 |        48 |         64 | 0      |
| 13  | 71     | Arts, Entertainment, and Recreation                                      | Amusement, Gambling, and Recreation Industries                                                                         | 713000      |        19916 |       7 |       154 |        175 | 0      |
| 14  | 72     | Accommodation and Food Services                                          | Accommodation                                                                                                          | 721000      |         2087 |       2 |        13 |         25 | 0      |
| 15  | 81     | Other Services (except Public Administration)                            | Religious, Grantmaking, Civic, Professional, and Similar Organizations                                                 | 813000      |       122861 |      34 |       878 |       1193 | 0      |
| 16  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Oilseed and Grain Farming                                                                                              | 111100      |    512173950 |       1 |   1088901 |     142243 | \-1    |
| 17  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Fruit and Tree Nut Farming                                                                                             | 111300      |       139016 |       1 |       733 |         74 | 0      |
| 18  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Other Crop Farming                                                                                                     | 111900      |     17849730 |       1 |     50999 |       4470 | \-1    |
| 19  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Cattle Ranching and Farming                                                                                            | 112100      |    190802162 |       1 |    172867 |      12380 | \-1    |
| 20  | 21     | Mining                                                                   | Metal Ore Mining                                                                                                       | 212200      |       218462 |       1 |      1516 |       2257 | 0      |
| 21  | 31     | Manufacturing                                                            | Tobacco Manufacturing                                                                                                  | 312200      |           12 |       1 |         1 |          1 | 0      |
| 22  | 32     | Manufacturing                                                            | Other Chemical Product and Preparation Manufacturing                                                                   | 325900      |       341855 |    1441 |       892 |       3023 | 0      |
| 23  | 32     | Manufacturing                                                            | Lime and Gypsum Product Manufacturing                                                                                  | 327400      |      1038503 |    1171 |       849 |       4213 | 0      |
| 24  | 51     | Information                                                              | Newspaper, Periodical, Book, and Directory Publishers                                                                  | 511100      |         5757 |       1 |        60 |         41 | 0      |
| 25  | 54     | Professional, Scientific, and Technical Services                         | Management, Scientific, and Technical Consulting Services                                                              | 541600      |        12090 |       1 |       114 |         89 | 0      |
| 26  | 54     | Professional, Scientific, and Technical Services                         | Other Professional, Scientific, and Technical Services                                                                 | 541900      |        55396 |      19 |       409 |        550 | 0      |
| 27  | 72     | Accommodation and Food Services                                          | Traveler Accommodation                                                                                                 | 721100      |       255423 |      65 |      1711 |       2639 | 0      |
| 28  | 81     | Other Services (except Public Administration)                            | Automotive Repair and Maintenance                                                                                      | 811100      |        87940 |      16 |       593 |        918 | 0      |
| 29  | 21     | Mining                                                                   | Support Activities for Mining                                                                                          | 213110      |        52900 |       1 |       712 |         51 | 0      |
| 30  | 31     | Manufacturing                                                            | Starch and Vegetable Fats and Oils Manufacturing                                                                       | 311220      |     10785331 |    9555 |     11456 |     181371 | 0      |
| 31  | 31     | Manufacturing                                                            | Sugar Manufacturing                                                                                                    | 311310      |       601093 |    4808 |       526 |       2732 | 0      |
| 32  | 31     | Manufacturing                                                            | Dairy Product (except Frozen) Manufacturing                                                                            | 311510      |        22010 |      21 |        27 |        366 | 0      |
| 33  | 31     | Manufacturing                                                            | Animal Slaughtering and Processing                                                                                     | 311610      |      4925270 |    4361 |      5955 |      81802 | 0      |
| 34  | 32     | Manufacturing                                                            | Veneer, Plywood, and Engineered Wood Product Manufacturing                                                             | 321210      |        16938 |      12 |        22 |        286 | 0      |
| 35  | 32     | Manufacturing                                                            | Paper Bag and Coated and Treated Paper Manufacturing                                                                   | 322220      |      1663592 |    1269 |       758 |      29899 | 0      |
| 36  | 32     | Manufacturing                                                            | Other Plastics Product Manufacturing                                                                                   | 326190      |       181888 |     376 |       574 |       2158 | 0      |
| 37  | 32     | Manufacturing                                                            | Pottery, Ceramics, and Plumbing Fixture Manufacturing                                                                  | 327110      |        29327 |      89 |        69 |        317 | 0      |
| 38  | 32     | Manufacturing                                                            | Clay Building Material and Refractories Manufacturing                                                                  | 327120      |       114505 |     350 |       260 |       1236 | 0      |
| 39  | 33     | Manufacturing                                                            | Alumina and Aluminum Production and Processing                                                                         | 331310      |     12405935 |       1 |      2754 |      89282 | 0      |
| 40  | 33     | Manufacturing                                                            | Forging and Stamping                                                                                                   | 332110      |        76607 |      22 |        24 |       1457 | 0      |
| 41  | 33     | Manufacturing                                                            | Cutlery and Handtool Manufacturing                                                                                     | 332210      |        31113 |      17 |        10 |        579 | 0      |
| 42  | 33     | Manufacturing                                                            | Metal Valve Manufacturing                                                                                              | 332910      |        22824 |      18 |         7 |        417 | 0      |
| 43  | 33     | Manufacturing                                                            | All Other Fabricated Metal Product Manufacturing                                                                       | 332990      |       120735 |      29 |       177 |       2114 | 0      |
| 44  | 33     | Manufacturing                                                            | Other Industrial Machinery Manufacturing                                                                               | 333290      |        11461 |       5 |         7 |        214 | 0      |
| 45  | 33     | Manufacturing                                                            | Commercial and Service Industry Machinery Manufacturing                                                                | 333310      |         1198 |       1 |         7 |         17 | 0      |
| 46  | 33     | Manufacturing                                                            | Ventilation, Heating, Air-Conditioning, and Commercial Refrigeration Equipment Manufacturing                           | 333410      |         7820 |       5 |         6 |        142 | 0      |
| 47  | 33     | Manufacturing                                                            | Metalworking Machinery Manufacturing                                                                                   | 333510      |         1494 |       2 |         2 |         28 | 0      |
| 48  | 33     | Manufacturing                                                            | All Other General Purpose Machinery Manufacturing                                                                      | 333990      |         8762 |       3 |         4 |        168 | 0      |
| 49  | 33     | Manufacturing                                                            | Computer and Peripheral Equipment Manufacturing                                                                        | 334110      |        12755 |       1 |        68 |        158 | 0      |
| 50  | 33     | Manufacturing                                                            | Semiconductor and Other Electronic Component Manufacturing                                                             | 334410      |         2508 |       1 |         1 |         50 | 0      |
| 51  | 33     | Manufacturing                                                            | Electromedical and Electrotherapeutic Apparatus Manufacturing                                                          | 334510      |          319 |       1 |         1 |          7 | 0      |
| 52  | 33     | Manufacturing                                                            | Manufacturing and Reproducing Magnetic and Optical Media                                                               | 334610      |         2579 |       1 |         2 |         51 | 0      |
| 53  | 33     | Manufacturing                                                            | Aerospace Product and Parts Manufacturing                                                                              | 336410      |          756 |       2 |         2 |         14 | 0      |
| 54  | 33     | Manufacturing                                                            | Household and Institutional Furniture Manufacturing                                                                    | 337120      |        19732 |      22 |        21 |        304 | 0      |
| 55  | 33     | Manufacturing                                                            | Office Furniture (including Fixtures) Manufacturing                                                                    | 337210      |           71 |       1 |         1 |          2 | 0      |
| 56  | 33     | Manufacturing                                                            | All Other Miscellaneous Manufacturing                                                                                  | 339990      |         6643 |       1 |         6 |        123 | 0      |
| 57  | 54     | Professional, Scientific, and Technical Services                         | Computer Systems Design and Related Services                                                                           | 541510      |        19573 |       1 |       263 |         55 | 0      |
| 58  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Vegetable and Melon Farming                                                                                            | 111200      |        99077 |       1 |       538 |         57 | 0      |
| 59  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Tree Nut Farming                                                                                                       | 111335      |        26847 |       1 |       154 |         16 | 0      |
| 60  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Greenhouse, Nursery, and Floriculture Production                                                                       | 111400      |       159087 |       1 |       953 |        841 | 0      |
| 61  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Tobacco Farming                                                                                                        | 111910      |       108513 |       1 |       534 |         36 | 0      |
| 62  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Cotton Farming                                                                                                         | 111920      |      4631796 |       1 |     15130 |       2308 | \-1    |
| 63  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Dairy Cattle and Milk Production                                                                                       | 112120      |      2130599 |       1 |      2382 |        109 | 0      |
| 64  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Poultry and Egg Production                                                                                             | 112300      |       390080 |       1 |      2392 |        675 | 0      |
| 65  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Logging                                                                                                                | 113300      |       189952 |      43 |      2647 |          1 | \-1    |
| 66  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Fishing                                                                                                                | 114100      |      3567130 |       1 |     51551 |          1 | \-1    |
| 68  | 11     | Agriculture, Forestry, Fishing and Hunting                               | Support Activities for Agriculture and Forestry                                                                        | 115000      |       597536 |       1 |      8819 |          1 | \-1    |
| 69  | 21     | Mining                                                                   | Oil and Gas Extraction                                                                                                 | 211000      |     42519311 |       1 |     21652 |     207909 | 0      |
| 70  | 21     | Mining                                                                   | Coal Mining                                                                                                            | 212100      |     11918238 |     840 |     10549 |      10580 | 0      |
| 71  | 21     | Mining                                                                   | Iron Ore Mining                                                                                                        | 212210      |       430338 |     521 |      1507 |       5565 | 0      |
| 72  | 21     | Mining                                                                   | Copper, Nickel, Lead, and Zinc Mining                                                                                  | 212230      |       133581 |       1 |      1932 |          1 | \-1    |
| 73  | 21     | Mining                                                                   | Stone Mining and Quarrying                                                                                             | 212310      |       445262 |       1 |      4995 |       1935 | 0      |
| 74  | 21     | Mining                                                                   | Sand, Gravel, Clay, and Ceramic and Refractory Minerals Mining and Quarrying                                           | 212320      |       140626 |     213 |      1102 |        884 | 0      |
| 75  | 21     | Mining                                                                   | Other Nonmetallic Mineral Mining and Quarrying                                                                         | 212390      |      1262986 |     130 |      8053 |      13781 | 0      |
| 76  | 21     | Mining                                                                   | Drilling Oil and Gas Wells                                                                                             | 213111      |          618 |       1 |        10 |          1 | 0      |
| 77  | 21     | Mining                                                                   | Support Activities for Oil and Gas Operations                                                                          | 213112      |        61075 |       1 |       858 |         23 | 0      |
| 78  | 22     | Utilities                                                                | Electric Power Generation, Transmission and Distribution                                                               | 221100      |    261525355 | 2321468 |    112772 |     678892 | \-1    |
| 79  | 22     | Utilities                                                                | Natural Gas Distribution                                                                                               | 221200      |      6361745 |       1 |      1780 |       9028 | 0      |
| 80  | 22     | Utilities                                                                | Water, Sewage and Other Systems                                                                                        | 221300      |      1499264 |       1 |       247 |        589 | \-1    |
| 85  | 23     | Construction                                                             | Other residential structures                                                                                           | 230202      |         6846 |       1 |        94 |         12 | 0      |
| 86  | 23     | Construction                                                             | Nonresidential maintenance and repair                                                                                  | 230301      |      3041421 |       1 |     41878 |       4107 | 0      |
| 87  | 23     | Construction                                                             | Residential maintenance and repair                                                                                     | 230302      |        84181 |       1 |      1140 |        136 | 0      |
| 88  | 31     | Manufacturing                                                            | Dog and Cat Food Manufacturing                                                                                         | 311111      |     81049437 |   71325 |     84058 |    1366674 | \-1    |
| 89  | 31     | Manufacturing                                                            | Other Animal Food Manufacturing                                                                                        | 311119      |      1283551 |    1127 |      1452 |      21479 | 0      |
| 90  | 31     | Manufacturing                                                            | Flour Milling and Malt Manufacturing                                                                                   | 311210      |      1301006 |    1148 |      1468 |      21767 | 0      |
| 91  | 31     | Manufacturing                                                            | Wet Corn Milling                                                                                                       | 311221      |     29971740 |  259954 |      4298 |     131052 | 0      |
| 92  | 31     | Manufacturing                                                            | Fats and Oils Refining and Blending                                                                                    | 311225      |      1997989 |    1760 |      2112 |      33635 | 0      |
| 93  | 31     | Manufacturing                                                            | Breakfast Cereal Manufacturing                                                                                         | 311230      |         2002 |       3 |         4 |         34 | 0      |
| 94  | 31     | Manufacturing                                                            | Beet Sugar Manufacturing                                                                                               | 311313      |      5232239 |   41672 |      4212 |      24598 | 0      |
| 95  | 31     | Manufacturing                                                            | Chocolate and Confectionery Manufacturing from Cacao Beans                                                             | 311320      |          792 |       2 |         2 |         14 | 0      |
| 96  | 31     | Manufacturing                                                            | Confectionery Manufacturing from Purchased Chocolate                                                                   | 311330      |          385 |       1 |         2 |          7 | 0      |
| 97  | 31     | Manufacturing                                                            | Nonchocolate Confectionery Manufacturing                                                                               | 311340      |        19116 |      18 |        22 |        322 | 0      |
| 98  | 31     | Manufacturing                                                            | Frozen Food Manufacturing                                                                                              | 311410      |        28792 |      27 |        31 |        486 | 0      |
| 99  | 31     | Manufacturing                                                            | Fruit and Vegetable Canning, Pickling, and Drying                                                                      | 311420      |       168198 |     151 |       185 |       2820 | 0      |
| 100 | 31     | Manufacturing                                                            | Cheese Manufacturing                                                                                                   | 311513      |        17452 |      17 |        20 |        294 | 0      |
| 101 | 31     | Manufacturing                                                            | Dry, Condensed, and Evaporated Dairy Product Manufacturing                                                             | 311514      |        92085 |      84 |       100 |       1546 | 0      |
| 102 | 31     | Manufacturing                                                            | Ice Cream and Frozen Dessert Manufacturing                                                                             | 311520      |          926 |       2 |         2 |         16 | 0      |
| 103 | 31     | Manufacturing                                                            | Poultry Processing                                                                                                     | 311615      |        83247 |      74 |        99 |       1388 | 0      |
| 104 | 31     | Manufacturing                                                            | Seafood Product Preparation and Packaging                                                                              | 311700      |       934437 |     839 |      1104 |      15539 | 0      |
| 105 | 31     | Manufacturing                                                            | Bread and Bakery Product Manufacturing                                                                                 | 311810      |         5684 |       6 |         8 |         96 | 0      |
| 106 | 31     | Manufacturing                                                            | Cookie, Cracker, and Pasta Manufacturing                                                                               | 311820      |         6015 |       6 |         8 |        100 | 0      |
| 107 | 31     | Manufacturing                                                            | Tortilla Manufacturing                                                                                                 | 311830      |          148 |       1 |         1 |          3 | 0      |
| 108 | 31     | Manufacturing                                                            | Snack Food Manufacturing                                                                                               | 311910      |        12306 |      12 |        14 |        208 | 0      |
| 109 | 31     | Manufacturing                                                            | Coffee and Tea Manufacturing                                                                                           | 311920      |         2225 |       3 |         4 |         38 | 0      |
| 110 | 31     | Manufacturing                                                            | Flavoring Syrup and Concentrate Manufacturing                                                                          | 311930      |         4735 |       6 |         6 |         80 | 0      |
| 111 | 31     | Manufacturing                                                            | Seasoning and Dressing Manufacturing                                                                                   | 311940      |        54445 |      47 |        78 |        893 | 0      |
| 112 | 31     | Manufacturing                                                            | All Other Food Manufacturing                                                                                           | 311990      |        36709 |      34 |        46 |        610 | 0      |
| 113 | 31     | Manufacturing                                                            | Soft Drink and Ice Manufacturing                                                                                       | 312110      |         3331 |       9 |         7 |         46 | 0      |
| 114 | 31     | Manufacturing                                                            | Breweries                                                                                                              | 312120      |       186160 |     422 |       292 |       2572 | 0      |
| 115 | 31     | Manufacturing                                                            | Wineries                                                                                                               | 312130      |         1140 |       4 |         3 |         16 | 0      |
| 116 | 31     | Manufacturing                                                            | Distilleries                                                                                                           | 312140      |        73782 |     177 |       138 |        976 | 0      |
| 117 | 31     | Manufacturing                                                            | Fiber, Yarn, and Thread Mills                                                                                          | 313100      |        23113 |      80 |        34 |        276 | 0      |
| 118 | 31     | Manufacturing                                                            | Broadwoven Fabric Mills                                                                                                | 313210      |        23362 |      85 |        29 |        278 | 0      |
| 119 | 31     | Manufacturing                                                            | Narrow Fabric Mills and Schiffli Machine Embroidery                                                                    | 313220      |         5719 |      23 |         9 |         65 | 0      |
| 120 | 31     | Manufacturing                                                            | Nonwoven Fabric Mills                                                                                                  | 313230      |        68762 |     241 |        96 |        813 | 0      |
| 121 | 31     | Manufacturing                                                            | Knit Fabric Mills                                                                                                      | 313240      |        15950 |      57 |        22 |        191 | 0      |
| 122 | 31     | Manufacturing                                                            | Textile and Fabric Finishing Mills                                                                                     | 313310      |        69956 |     248 |        88 |        836 | 0      |
| 123 | 31     | Manufacturing                                                            | Fabric Coating Mills                                                                                                   | 313320      |        33592 |     121 |        45 |        396 | 0      |
| 124 | 31     | Manufacturing                                                            | Carpet and Rug Mills                                                                                                   | 314110      |         2842 |      10 |         6 |         35 | 0      |
| 125 | 31     | Manufacturing                                                            | Curtain and Linen Mills                                                                                                | 314120      |         1222 |       5 |         3 |         15 | 0      |
| 126 | 31     | Manufacturing                                                            | Textile Bag and Canvas Mills                                                                                           | 314910      |         8499 |      29 |        18 |         97 | 0      |
| 127 | 31     | Manufacturing                                                            | All Other Textile Product Mills                                                                                        | 314990      |        36280 |     126 |        81 |        392 | 0      |
| 128 | 31     | Manufacturing                                                            | Apparel Knitting Mills                                                                                                 | 315100      |          418 |       1 |         1 |          9 | 0      |
| 129 | 31     | Manufacturing                                                            | Cut and Sew Apparel Contractors                                                                                        | 315210      |         4288 |       1 |         6 |         80 | 0      |
| 130 | 31     | Manufacturing                                                            | Men’s and Boys’ Cut and Sew Apparel Manufacturing                                                                      | 315220      |          388 |       1 |         1 |          8 | 0      |
| 131 | 31     | Manufacturing                                                            | Women’s and Girls’ Cut and Sew Apparel Manufacturing                                                                   | 315230      |         1085 |       1 |         2 |         21 | 0      |
| 132 | 31     | Manufacturing                                                            | Other Cut and Sew Apparel Manufacturing                                                                                | 315290      |           51 |       1 |         1 |          2 | 0      |
| 133 | 31     | Manufacturing                                                            | Apparel Accessories and Other Apparel Manufacturing                                                                    | 315900      |         1260 |       1 |         2 |         24 | 0      |
| 134 | 31     | Manufacturing                                                            | Leather and Hide Tanning and Finishing                                                                                 | 316100      |         2468 |       1 |         1 |         50 | 0      |
| 135 | 31     | Manufacturing                                                            | Footwear Manufacturing                                                                                                 | 316200      |            5 |       1 |         1 |          1 | 0      |
| 136 | 31     | Manufacturing                                                            | Other Leather and Allied Product Manufacturing                                                                         | 316900      |         7991 |       1 |        51 |         91 | 0      |
| 137 | 32     | Manufacturing                                                            | Sawmills and Wood Preservation                                                                                         | 321100      |       102837 |       1 |       569 |       1055 | 0      |
| 138 | 32     | Manufacturing                                                            | Reconstituted Wood Product Manufacturing                                                                               | 321219      |        22668 |       9 |        77 |        320 | 0      |
| 139 | 32     | Manufacturing                                                            | Millwork                                                                                                               | 321910      |        11795 |       1 |        19 |        178 | 0      |
| 140 | 32     | Manufacturing                                                            | Wood Container and Pallet Manufacturing                                                                                | 321920      |        33953 |       1 |       160 |        175 | 0      |
| 141 | 32     | Manufacturing                                                            | Manufactured Home (Mobile Home) Manufacturing                                                                          | 321991      |           41 |       1 |         1 |          1 | 0      |
| 142 | 32     | Manufacturing                                                            | Prefabricated Wood Building Manufacturing                                                                              | 321992      |          452 |       1 |         2 |          8 | 0      |
| 143 | 32     | Manufacturing                                                            | All Other Miscellaneous Wood Product Manufacturing                                                                     | 321999      |        26973 |       1 |        40 |        413 | 0      |
| 144 | 32     | Manufacturing                                                            | Pulp Mills                                                                                                             | 322110      |       937363 |    1681 |      5376 |       8063 | 0      |
| 145 | 32     | Manufacturing                                                            | Paper Mills                                                                                                            | 322120      |      9657386 |   47191 |     23114 |      71267 | 0      |
| 146 | 32     | Manufacturing                                                            | Paperboard Mills                                                                                                       | 322130      |      6706011 |   27711 |     13021 |      62766 | 0      |
| 147 | 32     | Manufacturing                                                            | Paperboard Container Manufacturing                                                                                     | 322210      |       993916 |     694 |      1859 |      16223 | 0      |
| 148 | 32     | Manufacturing                                                            | Stationery Product Manufacturing                                                                                       | 322230      |         7608 |       7 |         6 |        136 | 0      |
| 149 | 32     | Manufacturing                                                            | Sanitary Paper Product Manufacturing                                                                                   | 322291      |        72855 |      57 |        30 |       1315 | 0      |
| 150 | 32     | Manufacturing                                                            | All Other Converted Paper Product Manufacturing                                                                        | 322299      |       277789 |     212 |       118 |       5007 | 0      |
| 151 | 32     | Manufacturing                                                            | Printing                                                                                                               | 323110      |       167323 |       1 |        74 |       3242 | 0      |
| 152 | 32     | Manufacturing                                                            | Support Activities for Printing                                                                                        | 323120      |         5474 |       1 |         1 |        110 | 0      |
| 153 | 32     | Manufacturing                                                            | Petroleum Refineries                                                                                                   | 324110      |     23267677 |     128 |    252543 |     104086 | 0      |
| 154 | 32     | Manufacturing                                                            | Asphalt Paving Mixture and Block Manufacturing                                                                         | 324121      |        44473 |      14 |       147 |        662 | 0      |
| 155 | 32     | Manufacturing                                                            | Asphalt Shingle and Coating Materials Manufacturing                                                                    | 324122      |        19386 |       5 |       104 |        236 | 0      |
| 156 | 32     | Manufacturing                                                            | Petroleum Lubricating Oil and Grease Manufacturing                                                                     | 324191      |        96718 |      13 |       907 |        646 | 0      |
| 157 | 32     | Manufacturing                                                            | All Other Petroleum and Coal Products Manufacturing                                                                    | 324199      |       410849 |    2258 |      2108 |        753 | 0      |
| 158 | 32     | Manufacturing                                                            | Petrochemical Manufacturing                                                                                            | 325110      |      4401532 |    1111 |     31073 |      31813 | 0      |
| 159 | 32     | Manufacturing                                                            | Industrial Gas Manufacturing                                                                                           | 325120      |      2552552 |       1 |         1 |       5871 | \-1    |
| 160 | 32     | Manufacturing                                                            | Synthetic Dye and Pigment Manufacturing                                                                                | 325130      |       579380 |    1299 |       774 |       3091 | 0      |
| 161 | 32     | Manufacturing                                                            | Alkalies and Chlorine Manufacturing                                                                                    | 325181      |      1070673 |    3256 |       164 |      15301 | 0      |
| 162 | 32     | Manufacturing                                                            | Carbon Black Manufacturing                                                                                             | 325182      |       308492 |       1 |      3671 |       1224 | 0      |
| 163 | 32     | Manufacturing                                                            | All Other Basic Inorganic Chemical Manufacturing                                                                       | 325188      |      4253385 |    8224 |      5627 |      29431 | 0      |
| 164 | 32     | Manufacturing                                                            | Other Basic Organic Chemical Manufacturing                                                                             | 325190      |     13066921 |   37443 |     41396 |     114420 | 0      |
| 165 | 32     | Manufacturing                                                            | Plastics Material and Resin Manufacturing                                                                              | 325211      |      5728012 |    5537 |     28669 |      68804 | 0      |
| 166 | 32     | Manufacturing                                                            | Synthetic Rubber Manufacturing                                                                                         | 325212      |       153641 |     400 |        81 |       2234 | 0      |
| 167 | 32     | Manufacturing                                                            | Artificial and Synthetic Fibers and Filaments Manufacturing                                                            | 325220      |       122217 |     570 |       155 |       1214 | 0      |
| 168 | 32     | Manufacturing                                                            | Fertilizer Manufacturing                                                                                               | 325310      |     56511587 |    1290 |      6444 |     268019 | 0      |
| 169 | 32     | Manufacturing                                                            | Pesticide and Other Agricultural Chemical Manufacturing                                                                | 325320      |       566846 |    2645 |      1012 |       5207 | 0      |
| 170 | 32     | Manufacturing                                                            | Medicinal and Botanical Manufacturing                                                                                  | 325411      |      1644253 |       1 |      3000 |      28564 | 0      |
| 171 | 32     | Manufacturing                                                            | Pharmaceutical Preparation Manufacturing                                                                               | 325412      |        84019 |     251 |        84 |       1110 | 0      |
| 172 | 32     | Manufacturing                                                            | In-Vitro Diagnostic Substance Manufacturing                                                                            | 325413      |          763 |       1 |         3 |         14 | 0      |
| 173 | 32     | Manufacturing                                                            | Biological Product (except Diagnostic) Manufacturing                                                                   | 325414      |        90802 |       1 |        63 |       1721 | 0      |
| 174 | 32     | Manufacturing                                                            | Paint and Coating Manufacturing                                                                                        | 325510      |        61666 |     276 |       130 |        562 | 0      |
| 175 | 32     | Manufacturing                                                            | Adhesive Manufacturing                                                                                                 | 325520      |       185574 |     476 |      1428 |        875 | 0      |
| 176 | 32     | Manufacturing                                                            | Soap and Cleaning Compound Manufacturing                                                                               | 325610      |       116694 |     529 |       203 |       1110 | 0      |
| 177 | 32     | Manufacturing                                                            | Toilet Preparation Manufacturing                                                                                       | 325620      |        24233 |     112 |        33 |        243 | 0      |
| 178 | 32     | Manufacturing                                                            | Printing Ink Manufacturing                                                                                             | 325910      |       127652 |     160 |      1409 |        306 | 0      |
| 179 | 32     | Manufacturing                                                            | Plastics Packaging Materials and Unlaminated Film and Sheet Manufacturing                                              | 326110      |      1277652 |    3275 |       283 |      19232 | 0      |
| 180 | 32     | Manufacturing                                                            | Unlaminated Plastics Profile Shape Manufacturing                                                                       | 326121      |        34809 |      92 |         8 |        522 | 0      |
| 181 | 32     | Manufacturing                                                            | Plastics Pipe and Pipe Fitting Manufacturing                                                                           | 326122      |         5903 |      17 |         2 |         89 | 0      |
| 182 | 32     | Manufacturing                                                            | Laminated Plastics Plate, Sheet (except Packaging), and Shape Manufacturing                                            | 326130      |        27008 |      71 |         7 |        406 | 0      |
| 183 | 32     | Manufacturing                                                            | Polystyrene Foam Product Manufacturing                                                                                 | 326140      |        58338 |     151 |        20 |        870 | 0      |
| 184 | 32     | Manufacturing                                                            | Urethane and Other Foam Product (except Polystyrene) Manufacturing                                                     | 326150      |        10033 |      22 |        33 |        119 | 0      |
| 185 | 32     | Manufacturing                                                            | Plastics Bottle Manufacturing                                                                                          | 326160      |        12084 |      32 |         4 |        183 | 0      |
| 186 | 32     | Manufacturing                                                            | Tire Manufacturing                                                                                                     | 326210      |       124359 |     316 |        28 |       1881 | 0      |
| 187 | 32     | Manufacturing                                                            | Rubber and Plastics Hoses and Belting Manufacturing                                                                    | 326220      |        14948 |      40 |         4 |        224 | 0      |
| 188 | 32     | Manufacturing                                                            | Other Rubber Product Manufacturing                                                                                     | 326290      |        27963 |      73 |         7 |        421 | 0      |
| 189 | 32     | Manufacturing                                                            | Flat Glass Manufacturing                                                                                               | 327211      |        77227 |       1 |        57 |       1465 | 0      |
| 190 | 32     | Manufacturing                                                            | Other Pressed and Blown Glass and Glassware Manufacturing                                                              | 327212      |        94493 |     287 |       213 |       1027 | 0      |
| 191 | 32     | Manufacturing                                                            | Glass Container Manufacturing                                                                                          | 327213      |       277836 |       1 |         1 |       5526 | 0      |
| 192 | 32     | Manufacturing                                                            | Glass Product Manufacturing Made of Purchased Glass                                                                    | 327215      |        19682 |      60 |        49 |        210 | 0      |
| 193 | 32     | Manufacturing                                                            | Cement Manufacturing                                                                                                   | 327310      |      1452427 |    5075 |      1353 |        438 | 0      |
| 194 | 32     | Manufacturing                                                            | Ready-Mix Concrete Manufacturing                                                                                       | 327320      |        27022 |      81 |        65 |        290 | 0      |
| 195 | 32     | Manufacturing                                                            | Concrete Pipe, Brick, and Block Manufacturing                                                                          | 327330      |        11276 |      36 |        27 |        122 | 0      |
| 196 | 32     | Manufacturing                                                            | Other Concrete Product Manufacturing                                                                                   | 327390      |        25325 |      78 |        62 |        268 | 0      |
| 197 | 32     | Manufacturing                                                            | Abrasive Product Manufacturing                                                                                         | 327910      |        45938 |      99 |        77 |        349 | 0      |
| 198 | 32     | Manufacturing                                                            | Cut Stone and Stone Product Manufacturing                                                                              | 327991      |         2117 |       7 |         8 |         22 | 0      |
| 199 | 32     | Manufacturing                                                            | Ground or Treated Mineral and Earth Manufacturing                                                                      | 327992      |       186278 |     573 |       418 |       2010 | 0      |
| 200 | 32     | Manufacturing                                                            | Mineral Wool Manufacturing                                                                                             | 327993      |        17331 |      27 |         1 |        300 | 0      |
| 201 | 32     | Manufacturing                                                            | All Other Miscellaneous Nonmetallic Mineral Product Manufacturing                                                      | 327999      |         8378 |      26 |        22 |         89 | 0      |
| 202 | 33     | Manufacturing                                                            | Iron and Steel Mills and Ferroalloy Manufacturing                                                                      | 331110      |     25622729 |  176314 |      2881 |      81059 | 0      |
| 203 | 33     | Manufacturing                                                            | Steel Product Manufacturing from Purchased Steel                                                                       | 331200      |       240503 |       1 |         1 |       4784 | 0      |
| 204 | 33     | Manufacturing                                                            | Secondary Smelting and Alloying of Aluminum                                                                            | 331314      |       669371 |       1 |       389 |      12690 | 0      |
| 205 | 33     | Manufacturing                                                            | Primary Smelting and Refining of Copper                                                                                | 331411      |       115212 |     304 |       283 |       1247 | 0      |
| 206 | 33     | Manufacturing                                                            | Primary Smelting and Refining of Nonferrous Metal (except Copper and Aluminum)                                         | 331419      |       800043 |     353 |       325 |       1500 | 0      |
| 207 | 33     | Manufacturing                                                            | Copper Rolling, Drawing, Extruding, and Alloying                                                                       | 331420      |        93199 |     302 |       223 |        920 | 0      |
| 208 | 33     | Manufacturing                                                            | Nonferrous Metal (except Copper and Aluminum) Rolling, Drawing, Extruding, and Alloying                                | 331490      |       173489 |     492 |       411 |       1843 | 0      |
| 209 | 33     | Manufacturing                                                            | Ferrous Metal Foundries                                                                                                | 331510      |       199027 |     766 |        65 |       2522 | 0      |
| 210 | 33     | Manufacturing                                                            | Nonferrous Metal Foundries                                                                                             | 331520      |       100852 |     404 |        35 |       1249 | 0      |
| 211 | 33     | Manufacturing                                                            | Custom Roll Forming                                                                                                    | 332114      |        32452 |       9 |        12 |        618 | 0      |
| 212 | 33     | Manufacturing                                                            | Plate Work and Fabricated Structural Product Manufacturing                                                             | 332310      |        31970 |      19 |        12 |        589 | 0      |
| 213 | 33     | Manufacturing                                                            | Ornamental and Architectural Metal Products Manufacturing                                                              | 332320      |        82707 |      55 |        26 |       1516 | 0      |
| 214 | 33     | Manufacturing                                                            | Power Boiler and Heat Exchanger Manufacturing                                                                          | 332410      |         6210 |       4 |         3 |        117 | 0      |
| 215 | 33     | Manufacturing                                                            | Metal Tank (Heavy Gauge) Manufacturing                                                                                 | 332420      |         3925 |       3 |         2 |         74 | 0      |
| 216 | 33     | Manufacturing                                                            | Metal Can, Box, and Other Metal Container (Light Gauge) Manufacturing                                                  | 332430      |      3465131 |    1075 |       945 |      65737 | 0      |
| 217 | 33     | Manufacturing                                                            | Hardware Manufacturing                                                                                                 | 332500      |        15079 |      11 |         6 |        276 | 0      |
| 218 | 33     | Manufacturing                                                            | Spring and Wire Product Manufacturing                                                                                  | 332600      |        48832 |      19 |        16 |        920 | 0      |
| 219 | 33     | Manufacturing                                                            | Machine Shops                                                                                                          | 332710      |        74374 |      43 |       178 |       1168 | 0      |
| 220 | 33     | Manufacturing                                                            | Turned Product and Screw, Nut, and Bolt Manufacturing                                                                  | 332720      |        33692 |      21 |        12 |        622 | 0      |
| 221 | 33     | Manufacturing                                                            | Coating, Engraving, Heat Treating, and Allied Activities                                                               | 332800      |       388327 |      48 |       721 |       6671 | 0      |
| 222 | 33     | Manufacturing                                                            | Plumbing Fixture Fitting and Trim Manufacturing                                                                        | 332913      |         2355 |       2 |         2 |         45 | 0      |
| 223 | 33     | Manufacturing                                                            | Ball and Roller Bearing Manufacturing                                                                                  | 332991      |        19805 |      10 |         6 |        373 | 0      |
| 224 | 33     | Manufacturing                                                            | Fabricated Pipe and Pipe Fitting Manufacturing                                                                         | 332996      |         2371 |       3 |         2 |         44 | 0      |
| 225 | 33     | Manufacturing                                                            | Farm Machinery and Equipment Manufacturing                                                                             | 333111      |        40982 |       4 |         9 |        800 | 0      |
| 226 | 33     | Manufacturing                                                            | Lawn and Garden Tractor and Home Lawn and Garden Equipment Manufacturing                                               | 333112      |         1132 |       1 |         1 |         22 | 0      |
| 227 | 33     | Manufacturing                                                            | Construction Machinery Manufacturing                                                                                   | 333120      |         8112 |       1 |        28 |        127 | 0      |
| 228 | 33     | Manufacturing                                                            | Mining and Oil and Gas Field Machinery Manufacturing                                                                   | 333130      |         2555 |       2 |         2 |         49 | 0      |
| 229 | 33     | Manufacturing                                                            | Plastics and Rubber Industry Machinery Manufacturing                                                                   | 333220      |         1241 |       2 |         2 |         23 | 0      |
| 230 | 33     | Manufacturing                                                            | Semiconductor Machinery Manufacturing                                                                                  | 333295      |          424 |       1 |         1 |          9 | 0      |
| 231 | 33     | Manufacturing                                                            | Optical Instrument and Lens Manufacturing                                                                              | 333314      |           69 |       1 |         1 |          2 | 0      |
| 232 | 33     | Manufacturing                                                            | Photographic and Photocopying Equipment Manufacturing                                                                  | 333315      |          202 |       1 |         1 |          5 | 0      |
| 233 | 33     | Manufacturing                                                            | Other Commercial and Service Industry Machinery Manufacturing                                                          | 333319      |         1438 |       1 |        11 |         17 | 0      |
| 234 | 33     | Manufacturing                                                            | Heating Equipment (except Warm Air Furnaces) Manufacturing                                                             | 333414      |         1931 |       1 |         2 |         38 | 0      |
| 235 | 33     | Manufacturing                                                            | Air-Conditioning and Warm Air Heating Equipment and Commercial and Industrial Refrigeration Equipment Manufacturing    | 333415      |         8497 |       2 |         3 |        166 | 0      |
| 236 | 33     | Manufacturing                                                            | Industrial Mold Manufacturing                                                                                          | 333511      |         1643 |       1 |         2 |         32 | 0      |
| 237 | 33     | Manufacturing                                                            | Special Die and Tool, Die Set, Jig, and Fixture Manufacturing                                                          | 333514      |         1440 |       1 |         1 |         28 | 0      |
| 238 | 33     | Manufacturing                                                            | Cutting Tool and Machine Tool Accessory Manufacturing                                                                  | 333515      |         7643 |       4 |         4 |        144 | 0      |
| 239 | 33     | Manufacturing                                                            | Turbine and Turbine Generator Set Units Manufacturing                                                                  | 333611      |         2834 |       3 |         3 |         52 | 0      |
| 240 | 33     | Manufacturing                                                            | Speed Changer, Industrial High-Speed Drive, and Gear Manufacturing                                                     | 333612      |         5269 |       1 |         2 |        105 | 0      |
| 241 | 33     | Manufacturing                                                            | Mechanical Power Transmission Equipment Manufacturing                                                                  | 333613      |         2908 |       2 |         2 |         56 | 0      |
| 242 | 33     | Manufacturing                                                            | Other Engine Equipment Manufacturing                                                                                   | 333618      |         5965 |       2 |         2 |        117 | 0      |
| 243 | 33     | Manufacturing                                                            | Pump and Pumping Equipment Manufacturing                                                                               | 333911      |         2185 |       2 |         2 |         43 | 0      |
| 244 | 33     | Manufacturing                                                            | Air and Gas Compressor Manufacturing                                                                                   | 333912      |          852 |       1 |         1 |         17 | 0      |
| 245 | 33     | Manufacturing                                                            | Material Handling Equipment Manufacturing                                                                              | 333920      |        18743 |       5 |         7 |        361 | 0      |
| 246 | 33     | Manufacturing                                                            | Power-Driven Handtool Manufacturing                                                                                    | 333991      |          520 |       1 |         1 |         11 | 0      |
| 247 | 33     | Manufacturing                                                            | Packaging Machinery Manufacturing                                                                                      | 333993      |          172 |       1 |         1 |          4 | 0      |
| 248 | 33     | Manufacturing                                                            | Industrial Process Furnace and Oven Manufacturing                                                                      | 333994      |          469 |       1 |         1 |          9 | 0      |
| 249 | 33     | Manufacturing                                                            | Electronic Computer Manufacturing                                                                                      | 334111      |         1532 |       1 |         1 |         31 | 0      |
| 250 | 33     | Manufacturing                                                            | Computer Storage Device Manufacturing                                                                                  | 334112      |          757 |       1 |         1 |         16 | 0      |
| 251 | 33     | Manufacturing                                                            | Telephone Apparatus Manufacturing                                                                                      | 334210      |         5546 |       1 |         2 |        110 | 0      |
| 252 | 33     | Manufacturing                                                            | Radio and Television Broadcasting and Wireless Communications Equipment Manufacturing                                  | 334220      |          643 |       1 |         1 |         13 | 0      |
| 253 | 33     | Manufacturing                                                            | Other Communications Equipment Manufacturing                                                                           | 334290      |         2896 |       1 |         2 |         58 | 0      |
| 254 | 33     | Manufacturing                                                            | Audio and Video Equipment Manufacturing                                                                                | 334300      |          990 |       1 |         1 |         20 | 0      |
| 255 | 33     | Manufacturing                                                            | Electron Tube Manufacturing                                                                                            | 334411      |         1835 |       1 |         1 |         37 | 0      |
| 256 | 33     | Manufacturing                                                            | Bare Printed Circuit Board Manufacturing                                                                               | 334412      |         8927 |       1 |         3 |        175 | 0      |
| 257 | 33     | Manufacturing                                                            | Semiconductor and Related Device Manufacturing                                                                         | 334413      |       351250 |       1 |         1 |       1412 | 0      |
| 258 | 33     | Manufacturing                                                            | Electronic Connector Manufacturing                                                                                     | 334417      |         1453 |       1 |         1 |         30 | 0      |
| 259 | 33     | Manufacturing                                                            | Printed Circuit Assembly (Electronic Assembly) Manufacturing                                                           | 334418      |         9960 |       1 |         1 |        199 | 0      |
| 260 | 33     | Manufacturing                                                            | Other Electronic Component Manufacturing                                                                               | 334419      |         7531 |       1 |         2 |        150 | 0      |
| 261 | 33     | Manufacturing                                                            | Electromedical and Electrotherapeutic Apparatus Manufacturing                                                          | 334510      |           13 |       1 |         1 |          1 | 0      |
| 262 | 33     | Manufacturing                                                            | Search, Detection, Navigation, Guidance, Aeronautical, and Nautical System and Instrument Manufacturing                | 334511      |          938 |       1 |         1 |         19 | 0      |
| 263 | 33     | Manufacturing                                                            | Automatic Environmental Control Manufacturing for Residential, Commercial, and Appliance Use                           | 334512      |          788 |       1 |         1 |         16 | 0      |
| 264 | 33     | Manufacturing                                                            | Instruments and Related Products Manufacturing for Measuring, Displaying, and Controlling Industrial Process Variables | 334513      |         2459 |       1 |         3 |         47 | 0      |
| 265 | 33     | Manufacturing                                                            | Totalizing Fluid Meter and Counting Device Manufacturing                                                               | 334514      |          914 |       1 |         1 |         19 | 0      |
| 266 | 33     | Manufacturing                                                            | Instrument Manufacturing for Measuring and Testing Electricity and Electrical Signals                                  | 334515      |          219 |       1 |         1 |          5 | 0      |
| 267 | 33     | Manufacturing                                                            | Analytical Laboratory Instrument Manufacturing                                                                         | 334516      |          554 |       1 |         1 |         12 | 0      |
| 268 | 33     | Manufacturing                                                            | Irradiation Apparatus Manufacturing                                                                                    | 334517      |           37 |       1 |         1 |          2 | 0      |
| 269 | 33     | Manufacturing                                                            | Magnetic and Optical Recording Media Manufacturing                                                                     | 334613      |         2501 |       1 |         1 |         50 | 0      |
| 270 | 33     | Manufacturing                                                            | Electric Lamp Bulb and Part Manufacturing                                                                              | 335110      |         4064 |       1 |         4 |         78 | 0      |
| 271 | 33     | Manufacturing                                                            | Lighting Fixture Manufacturing                                                                                         | 335120      |         5965 |       1 |         5 |        114 | 0      |
| 272 | 33     | Manufacturing                                                            | Small Electrical Appliance Manufacturing                                                                               | 335210      |          466 |       1 |         1 |         10 | 0      |
| 273 | 33     | Manufacturing                                                            | Household Cooking Appliance Manufacturing                                                                              | 335221      |          709 |       1 |         2 |         14 | 0      |
| 274 | 33     | Manufacturing                                                            | Household Refrigerator and Home Freezer Manufacturing                                                                  | 335222      |          117 |       1 |         1 |          3 | 0      |
| 275 | 33     | Manufacturing                                                            | Household Laundry Equipment Manufacturing                                                                              | 335224      |           57 |       1 |         1 |          2 | 0      |
| 276 | 33     | Manufacturing                                                            | Other Major Household Appliance Manufacturing                                                                          | 335228      |          268 |       1 |         1 |          6 | 0      |
| 277 | 33     | Manufacturing                                                            | Power, Distribution, and Specialty Transformer Manufacturing                                                           | 335311      |         2471 |       1 |         2 |         48 | 0      |
| 278 | 33     | Manufacturing                                                            | Motor and Generator Manufacturing                                                                                      | 335312      |        13653 |       1 |         8 |        263 | 0      |
| 279 | 33     | Manufacturing                                                            | Switchgear and Switchboard Apparatus Manufacturing                                                                     | 335313      |         3218 |       1 |         3 |         62 | 0      |
| 280 | 33     | Manufacturing                                                            | Relay and Industrial Control Manufacturing                                                                             | 335314      |         9176 |       1 |         1 |        183 | 0      |
| 281 | 33     | Manufacturing                                                            | Storage Battery Manufacturing                                                                                          | 335911      |        30012 |       1 |        28 |        563 | 0      |
| 282 | 33     | Manufacturing                                                            | Primary Battery Manufacturing                                                                                          | 335912      |          196 |       1 |         1 |          5 | 0      |
| 283 | 33     | Manufacturing                                                            | Communication and Energy Wire and Cable Manufacturing                                                                  | 335920      |        14323 |       1 |        13 |        271 | 0      |
| 284 | 33     | Manufacturing                                                            | Wiring Device Manufacturing                                                                                            | 335930      |         5853 |       1 |         5 |        113 | 0      |
| 285 | 33     | Manufacturing                                                            | Carbon and Graphite Product Manufacturing                                                                              | 335991      |        75926 |       1 |        58 |       1437 | 0      |
| 286 | 33     | Manufacturing                                                            | All Other Miscellaneous Electrical Equipment and Component Manufacturing                                               | 335999      |         1468 |       1 |         2 |         28 | 0      |
| 287 | 33     | Manufacturing                                                            | Automobile Manufacturing                                                                                               | 336111      |          202 |       1 |         1 |          5 | 0      |
| 288 | 33     | Manufacturing                                                            | Light Truck and Utility Vehicle Manufacturing                                                                          | 336112      |          190 |       1 |         1 |          4 | 0      |
| 289 | 33     | Manufacturing                                                            | Heavy Duty Truck Manufacturing                                                                                         | 336120      |         2504 |       2 |         4 |         45 | 0      |
| 290 | 33     | Manufacturing                                                            | Motor Vehicle Body Manufacturing                                                                                       | 336211      |          511 |       1 |         2 |         10 | 0      |
| 291 | 33     | Manufacturing                                                            | Truck Trailer Manufacturing                                                                                            | 336212      |          215 |       1 |         1 |          5 | 0      |
| 292 | 33     | Manufacturing                                                            | Motor Home Manufacturing                                                                                               | 336213      |           31 |       1 |         1 |          2 | 0      |
| 293 | 33     | Manufacturing                                                            | Travel Trailer and Camper Manufacturing                                                                                | 336214      |          588 |       1 |         2 |         11 | 0      |
| 294 | 33     | Manufacturing                                                            | Motor Vehicle Parts Manufacturing                                                                                      | 336300      |       116240 |      50 |       147 |       2018 | 0      |
| 295 | 33     | Manufacturing                                                            | Aircraft Manufacturing                                                                                                 | 336411      |          361 |       1 |         2 |          7 | 0      |
| 296 | 33     | Manufacturing                                                            | Aircraft Engine and Engine Parts Manufacturing                                                                         | 336412      |         1935 |       2 |         7 |         30 | 0      |
| 297 | 33     | Manufacturing                                                            | Other Aircraft Parts and Auxiliary Equipment Manufacturing                                                             | 336413      |         4251 |       2 |        18 |         60 | 0      |
| 298 | 33     | Manufacturing                                                            | Guided Missile and Space Vehicle Manufacturing                                                                         | 336414      |          142 |       1 |         1 |          3 | 0      |
| 299 | 33     | Manufacturing                                                            | Railroad Rolling Stock Manufacturing                                                                                   | 336500      |        17302 |       6 |        23 |        305 | 0      |
| 300 | 33     | Manufacturing                                                            | Ship Building and Repairing                                                                                            | 336611      |         3498 |       3 |         9 |         55 | 0      |
| 301 | 33     | Manufacturing                                                            | Boat Building                                                                                                          | 336612      |          166 |       1 |         1 |          4 | 0      |
| 302 | 33     | Manufacturing                                                            | Motorcycle, Bicycle, and Parts Manufacturing                                                                           | 336991      |          592 |       1 |         2 |         11 | 0      |
| 303 | 33     | Manufacturing                                                            | Military Armored Vehicle, Tank, and Tank Component Manufacturing                                                       | 336992      |            7 |       1 |         1 |          1 | 0      |
| 304 | 33     | Manufacturing                                                            | All Other Transportation Equipment Manufacturing                                                                       | 336999      |          397 |       1 |         2 |          8 | 0      |
| 305 | 33     | Manufacturing                                                            | Wood Kitchen Cabinet and Countertop Manufacturing                                                                      | 337110      |         3039 |       2 |         7 |         43 | 0      |
| 306 | 33     | Manufacturing                                                            | Upholstered Household Furniture Manufacturing                                                                          | 337121      |           42 |       1 |         1 |          2 | 0      |
| 307 | 33     | Manufacturing                                                            | Nonupholstered Wood Household Furniture Manufacturing                                                                  | 337122      |           60 |       1 |         1 |          2 | 0      |
| 308 | 33     | Manufacturing                                                            | Institutional Furniture Manufacturing                                                                                  | 337127      |          554 |       1 |         2 |         10 | 0      |
| 309 | 33     | Manufacturing                                                            | Custom Architectural Woodwork and Millwork Manufacturing                                                               | 337212      |          306 |       1 |         1 |          6 | 0      |
| 310 | 33     | Manufacturing                                                            | Showcase, Partition, Shelving, and Locker Manufacturing                                                                | 337215      |         3265 |       3 |         4 |         54 | 0      |
| 311 | 33     | Manufacturing                                                            | Mattress Manufacturing                                                                                                 | 337910      |           79 |       1 |         1 |          2 | 0      |
| 312 | 33     | Manufacturing                                                            | Blind and Shade Manufacturing                                                                                          | 337920      |         1047 |       3 |         2 |         15 | 0      |
| 313 | 33     | Manufacturing                                                            | Laboratory Apparatus and Furniture Manufacturing                                                                       | 339111      |           88 |       1 |         1 |          2 | 0      |
| 314 | 33     | Manufacturing                                                            | Surgical and Medical Instrument Manufacturing                                                                          | 339112      |          262 |       1 |         1 |          6 | 0      |
| 315 | 33     | Manufacturing                                                            | Surgical Appliance and Supplies Manufacturing                                                                          | 339113      |         1743 |       1 |         3 |         32 | 0      |
| 316 | 33     | Manufacturing                                                            | Dental Equipment and Supplies Manufacturing                                                                            | 339114      |            3 |       1 |         1 |          1 | 0      |
| 317 | 33     | Manufacturing                                                            | Ophthalmic Goods Manufacturing                                                                                         | 339115      |           38 |       1 |         1 |          2 | 0      |
| 318 | 33     | Manufacturing                                                            | Dental Laboratories                                                                                                    | 339116      |            1 |       1 |         1 |          1 | 0      |
| 319 | 33     | Manufacturing                                                            | Jewelry and Silverware Manufacturing                                                                                   | 339910      |          234 |       1 |         1 |          5 | 0      |
| 320 | 33     | Manufacturing                                                            | Sporting and Athletic Goods Manufacturing                                                                              | 339920      |          330 |       1 |         1 |          7 | 0      |
| 321 | 33     | Manufacturing                                                            | Doll, Toy, and Game Manufacturing                                                                                      | 339930      |          150 |       1 |         1 |          4 | 0      |
| 322 | 33     | Manufacturing                                                            | Office Supplies (except Paper) Manufacturing                                                                           | 339940      |         2985 |       1 |         3 |         56 | 0      |
| 323 | 33     | Manufacturing                                                            | Sign Manufacturing                                                                                                     | 339950      |         7566 |       1 |         9 |        135 | 0      |
| 324 | 33     | Manufacturing                                                            | Gasket, Packing, and Sealing Device Manufacturing                                                                      | 339991      |        10744 |       1 |        11 |        195 | 0      |
| 325 | 33     | Manufacturing                                                            | Musical Instrument Manufacturing                                                                                       | 339992      |          215 |       1 |         1 |          5 | 0      |
| 326 | 33     | Manufacturing                                                            | Broom, Brush, and Mop Manufacturing                                                                                    | 339994      |         1351 |       1 |         2 |         26 | 0      |
| 327 | 42     | Wholesale Trade                                                          | Wholesale Trade                                                                                                        | 420000      |      3629942 |     446 |     56599 |      18522 | 0      |
| 328 | 48     | Transportation and Warehousing                                           | Air Transportation                                                                                                     | 481000      |      5021621 |       1 |     74754 |          1 | 1      |
| 329 | 48     | Transportation and Warehousing                                           | Rail Transportation                                                                                                    | 482000      |     21082197 |       1 |    307632 |          1 | 1      |
| 330 | 48     | Transportation and Warehousing                                           | Water Transportation                                                                                                   | 483000      |      5884803 |       1 |     93900 |          1 | 1      |
| 331 | 48     | Transportation and Warehousing                                           | Truck Transportation                                                                                                   | 484000      |     41884693 |       1 |    562340 |          1 | 1      |
| 332 | 48     | Transportation and Warehousing                                           | Transit and Ground Passenger Transportation                                                                            | 485000      |        64217 |       1 |      5464 |          1 | \-1    |
| 333 | 48     | Transportation and Warehousing                                           | Pipeline Transportation                                                                                                | 486000      |     11461174 |       1 |         1 |     104230 | \-1    |
| 334 | 49     | Transportation and Warehousing                                           | Postal Service                                                                                                         | 491000      |       362534 |       1 |      3935 |       2242 | 0      |
| 335 | 49     | Transportation and Warehousing                                           | Couriers and Messengers                                                                                                | 492000      |      4359948 |       1 |     63286 |          1 | 1      |
| 336 | 49     | Transportation and Warehousing                                           | Warehousing and Storage                                                                                                | 493000      |       327335 |     253 |      3461 |       1695 | 0      |
| 337 | 51     | Information                                                              | Newspaper Publishers                                                                                                   | 511110      |        20101 |       6 |       132 |        225 | 0      |
| 338 | 51     | Information                                                              | Periodical Publishers                                                                                                  | 511120      |        12750 |       1 |       170 |         41 | 0      |
| 339 | 51     | Information                                                              | Book Publishers                                                                                                        | 511130      |          793 |       1 |         7 |          9 | 0      |
| 340 | 51     | Information                                                              | Software Publishers                                                                                                    | 511200      |         3410 |       1 |        46 |         11 | 0      |
| 341 | 51     | Information                                                              | Motion Picture and Video Industries                                                                                    | 512100      |         8052 |       3 |        54 |         91 | 0      |
| 342 | 51     | Information                                                              | Sound Recording Industries                                                                                             | 512200      |          687 |       1 |        10 |          3 | 0      |
| 343 | 51     | Information                                                              | Radio and Television Broadcasting                                                                                      | 515100      |       109384 |      23 |       664 |       1300 | 0      |
| 344 | 51     | Information                                                              | Cable and Other Subscription Programming                                                                               | 515200      |        34773 |       7 |       228 |        391 | 0      |
| 345 | 51     | Information                                                              | Internet Publishing and Broadcasting                                                                                   | 516110      |         3369 |       1 |        46 |         11 | 0      |
| 346 | 51     | Information                                                              | Telecommunications                                                                                                     | 517000      |       433508 |     101 |      2468 |       5265 | 0      |
| 347 | 51     | Information                                                              | Internet Service Providers and Web Search Portals                                                                      | 518100      |         3072 |       1 |        44 |          7 | 0      |
| 348 | 51     | Information                                                              | Data Processing, Hosting, and Related Services                                                                         | 518200      |        20382 |       1 |       239 |        103 | 0      |
| 349 | 51     | Information                                                              | Other Information Services                                                                                             | 519100      |         1771 |       1 |        13 |         21 | 0      |
| 350 | 52     | Finance and Insurance                                                    | Securities, Commodity Contracts, and Other Financial Investments and Related Activities                                | 523000      |        42848 |       6 |       528 |        143 | 0      |
| 351 | 52     | Finance and Insurance                                                    | Insurance Carriers                                                                                                     | 524100      |          671 |       1 |         8 |          6 | 0      |
| 352 | 52     | Finance and Insurance                                                    | Agencies, Brokerages, and Other Insurance Related Activities                                                           | 524200      |        19538 |       6 |       158 |        178 | 0      |
| 353 | 52     | Finance and Insurance                                                    | Funds, Trusts, and Other Financial Vehicles                                                                            | 525000      |           95 |       1 |         2 |          1 | 0      |
| 354 | 53     | Real Estate and Rental and Leasing                                       | Real Estate                                                                                                            | 531000      |       760099 |    3028 |      1091 |       8334 | 0      |
| 355 | 53     | Real Estate and Rental and Leasing                                       | Automotive Equipment Rental and Leasing                                                                                | 532100      |        21160 |       8 |       191 |        150 | 0      |
| 356 | 53     | Real Estate and Rental and Leasing                                       | Video Tape and Disc Rental                                                                                             | 532230      |           18 |       1 |         1 |          1 | 0      |
| 357 | 53     | Real Estate and Rental and Leasing                                       | Commercial and Industrial Machinery and Equipment Rental and Leasing                                                   | 532400      |       152346 |      33 |      1098 |       1349 | 0      |
| 358 | 53     | Real Estate and Rental and Leasing                                       | Lessors of Nonfinancial Intangible Assets (except Copyrighted Works)                                                   | 533000      |       264731 |       1 |      1082 |       3900 | 0      |
| 359 | 54     | Professional, Scientific, and Technical Services                         | Legal Services                                                                                                         | 541100      |        61829 |      13 |       624 |        406 | 0      |
| 360 | 54     | Professional, Scientific, and Technical Services                         | Accounting, Tax Preparation, Bookkeeping, and Payroll Services                                                         | 541200      |        61686 |      19 |       572 |        462 | 0      |
| 361 | 54     | Professional, Scientific, and Technical Services                         | Architectural, Engineering, and Related Services                                                                       | 541300      |       306096 |     277 |      3420 |        827 | 0      |
| 362 | 54     | Professional, Scientific, and Technical Services                         | Specialized Design Services                                                                                            | 541400      |        12179 |       1 |       121 |         91 | 0      |
| 363 | 54     | Professional, Scientific, and Technical Services                         | Custom Computer Programming Services                                                                                   | 541511      |         2593 |       2 |        28 |         17 | 0      |
| 364 | 54     | Professional, Scientific, and Technical Services                         | Computer Systems Design Services                                                                                       | 541512      |        25845 |       1 |       382 |         27 | 0      |
| 365 | 54     | Professional, Scientific, and Technical Services                         | Management Consulting Services                                                                                         | 541610      |        56329 |       1 |       669 |        220 | 0      |
| 366 | 54     | Professional, Scientific, and Technical Services                         | Scientific Research and Development Services                                                                           | 541700      |       524147 |    2382 |      3222 |       2043 | 0      |
| 367 | 54     | Professional, Scientific, and Technical Services                         | Advertising and Related Services                                                                                       | 541800      |        51524 |      16 |       483 |        350 | 0      |
| 368 | 54     | Professional, Scientific, and Technical Services                         | Photographic Services                                                                                                  | 541920      |         2742 |       3 |        23 |         21 | 0      |
| 369 | 54     | Professional, Scientific, and Technical Services                         | Veterinary Services                                                                                                    | 541940      |         3058 |       2 |        25 |         31 | 0      |
| 370 | 55     | Management of Companies and Enterprises                                  | Management of Companies and Enterprises                                                                                | 550000      |      1644584 |     513 |     10536 |      17665 | 0      |
| 371 | 56     | Administrative and Support and Waste Management and Remediation Services | Office Administrative Services                                                                                         | 561100      |        27211 |       6 |       233 |        236 | 0      |
| 372 | 56     | Administrative and Support and Waste Management and Remediation Services | Facilities Support Services                                                                                            | 561200      |         7234 |       4 |        55 |         68 | 0      |
| 373 | 56     | Administrative and Support and Waste Management and Remediation Services | Employment Services                                                                                                    | 561300      |        82576 |      17 |       951 |        376 | 0      |
| 374 | 56     | Administrative and Support and Waste Management and Remediation Services | Business Support Services                                                                                              | 561400      |        48665 |       8 |       494 |        325 | 0      |
| 375 | 56     | Administrative and Support and Waste Management and Remediation Services | Travel Arrangement and Reservation Services                                                                            | 561500      |        90352 |      23 |       642 |        890 | 0      |
| 376 | 56     | Administrative and Support and Waste Management and Remediation Services | Investigation and Security Services                                                                                    | 561600      |        16987 |       1 |       182 |        107 | 0      |
| 377 | 56     | Administrative and Support and Waste Management and Remediation Services | Services to Buildings and Dwellings                                                                                    | 561700      |       501550 |      28 |      6674 |        950 | 0      |
| 378 | 56     | Administrative and Support and Waste Management and Remediation Services | Other Support Services                                                                                                 | 561900      |        44845 |      11 |       354 |        420 | 0      |
| 379 | 56     | Administrative and Support and Waste Management and Remediation Services | Waste Management and Remediation Services                                                                              | 562000      |      7887688 |      27 |      3529 |        890 | 0      |
| 382 | 62     | Health Care and Social Assistance                                        | Hospitals                                                                                                              | 622000      |           11 |       1 |         1 |          1 | 0      |
| 383 | 62     | Health Care and Social Assistance                                        | Nursing and Residential Care Facilities                                                                                | 623000      |           40 |       1 |         1 |          1 | 0      |
| 384 | 62     | Health Care and Social Assistance                                        | Community Food and Housing, and Emergency and Other Relief Services                                                    | 624200      |            1 |       1 |         1 |          1 | 0      |
| 385 | 62     | Health Care and Social Assistance                                        | Child Day Care Services                                                                                                | 624400      |           46 |       1 |         1 |          1 | 0      |
| 386 | 71     | Arts, Entertainment, and Recreation                                      | Performing Arts Companies                                                                                              | 711100      |         2729 |       1 |        20 |         27 | 0      |
| 387 | 71     | Arts, Entertainment, and Recreation                                      | Spectator Sports                                                                                                       | 711200      |         7261 |       4 |        64 |         61 | 0      |
| 388 | 71     | Arts, Entertainment, and Recreation                                      | Independent Artists, Writers, and Performers                                                                           | 711500      |         3722 |       1 |        34 |         31 | 0      |
| 390 | 71     | Arts, Entertainment, and Recreation                                      | Fitness and Recreational Sports Centers                                                                                | 713940      |        15313 |       5 |       118 |        141 | 0      |
| 391 | 71     | Arts, Entertainment, and Recreation                                      | Bowling Centers                                                                                                        | 713950      |           38 |       1 |         1 |          1 | 0      |
| 392 | 72     | Accommodation and Food Services                                          | Food Services and Drinking Places                                                                                      | 722000      |       323449 |     209 |      2212 |       3056 | 0      |
| 393 | 81     | Other Services (except Public Administration)                            | Car Washes                                                                                                             | 811192      |        23537 |       5 |       172 |        229 | 0      |
| 394 | 81     | Other Services (except Public Administration)                            | Electronic and Precision Equipment Repair and Maintenance                                                              | 811200      |        19761 |      10 |       136 |        194 | 0      |
| 395 | 81     | Other Services (except Public Administration)                            | Commercial and Industrial Machinery and Equipment (except Automotive and Electronic) Repair and Maintenance            | 811300      |        72885 |      11 |       632 |        574 | 0      |
| 396 | 81     | Other Services (except Public Administration)                            | Personal and Household Goods Repair and Maintenance                                                                    | 811400      |        25938 |       5 |       202 |        237 | 0      |
| 399 | 81     | Other Services (except Public Administration)                            | Drycleaning and Laundry Services                                                                                       | 812300      |        13292 |       4 |        87 |        142 | 0      |
| 400 | 81     | Other Services (except Public Administration)                            | Other Personal Services                                                                                                | 812900      |         3988 |       2 |        29 |         41 | 0      |

# Final Note

The challenges of this project:

  - Manage data multicollinearity, heteroskedastic, highly right skewed.
  - Piping data processing, functional programming, statistic extraction
  - Communication between data processing in R and machine learning in
    python

# Exercise

Please, click [HERE]() to Exercise.
