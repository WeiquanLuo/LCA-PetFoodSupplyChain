
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
# Input columns 
X <- dat %>% 
  select(Coal.TJ, NatGase.TJ, Petrol.TJ, Bio.Waste.TJ, NonFossElec.TJ, Water.Withdrawals.Kgal)
psych::describe(X) %>% knitr::kable(format = "markdown") 
```

|                        | vars |   n |        mean |           sd |    median |   trimmed |       mad | min |          max |        range |     skew | kurtosis |          se |
| :--------------------- | ---: | --: | ----------: | -----------: | --------: | --------: | --------: | --: | -----------: | -----------: | -------: | -------: | ----------: |
| Coal.TJ                |    1 | 402 |   0.0076337 |    0.1168583 | 0.0000010 | 0.0000344 | 0.0000015 |   0 | 2.321467e+00 | 2.321467e+00 | 19.34331 | 379.4182 |   0.0058284 |
| NatGase.TJ             |    2 | 402 |   0.0110593 |    0.0797249 | 0.0001375 | 0.0006286 | 0.0002039 |   0 | 1.366673e+00 | 1.366673e+00 | 13.87669 | 217.6585 |   0.0039763 |
| Petrol.TJ              |    3 | 402 |   0.0086899 |    0.0654656 | 0.0000235 | 0.0003121 | 0.0000348 |   0 | 1.088900e+00 | 1.088900e+00 | 13.11028 | 195.8157 |   0.0032651 |
| Bio.Waste.TJ           |    4 | 402 |   0.0019597 |    0.0143116 | 0.0000010 | 0.0000250 | 0.0000015 |   0 | 1.806160e-01 | 1.806160e-01 | 10.26709 | 114.4378 |   0.0007138 |
| NonFossElec.TJ         |    5 | 402 |   0.0042177 |    0.0251197 | 0.0001570 | 0.0005038 | 0.0002298 |   0 | 4.231030e-01 | 4.231030e-01 | 13.11334 | 200.3802 |   0.0012529 |
| Water.Withdrawals.Kgal |    6 | 402 | 446.1435342 | 7894.3608174 | 0.0428940 | 0.5317598 | 0.0630550 |   0 | 1.580305e+05 | 1.580305e+05 | 19.78841 | 391.9673 | 393.7349309 |

``` r
M <- cor(X)
corrplot::corrplot(M, method="color")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
# target columns 
CPA <- dat %>% select(CO.t, NH3.t, NOx.t, PM10.t, PM2.5.t, SO2.t, VOC.t)
GHG <- dat %>% select(Total.t.CO2e, CO2.Fossil.t.CO2e, CO2.Process.t.CO2e, CH4.t.CO2e, HFC.PFCs.t.CO2e)
TOX <- dat %>% select(Fugitive.kg, Stack.kg, Total.Air.kg, Surface.water.kg, U_ground.Water.kg, Land.kg, Offiste.kg, POTW.Metal.kg)
ys <- cbind(CPA, GHG, TOX)
psych::describe(ys) %>% knitr::kable(format = "markdown") 
```

|                    | vars |   n |      mean |         sd |    median |   trimmed |       mad | min |        max |      range |      skew |  kurtosis |        se |
| :----------------- | ---: | --: | --------: | ---------: | --------: | --------: | --------: | --: | ---------: | ---------: | --------: | --------: | --------: |
| CO.t               |    1 | 402 | 0.0222147 |  0.2952078 | 0.0000385 | 0.0006645 | 0.0000571 |   0 |   5.888105 |   5.888105 | 19.523839 | 384.79885 | 0.0147236 |
| NH3.t              |    2 | 402 | 0.0098300 |  0.1389211 | 0.0000010 | 0.0000086 | 0.0000015 |   0 |   2.687352 |   2.687352 | 18.084851 | 341.44049 | 0.0069288 |
| NOx.t              |    3 | 402 | 0.0080359 |  0.0540448 | 0.0000280 | 0.0003363 | 0.0000415 |   0 |   0.707607 |   0.707607 |  9.977676 | 106.60356 | 0.0026955 |
| PM10.t             |    4 | 402 | 0.0177165 |  0.2850504 | 0.0000055 | 0.0001408 | 0.0000082 |   0 |   5.695606 |   5.695606 | 19.666983 | 388.60310 | 0.0142170 |
| PM2.5.t            |    5 | 402 | 0.0047236 |  0.0694547 | 0.0000040 | 0.0000720 | 0.0000059 |   0 |   1.382822 |   1.382822 | 19.453251 | 382.59517 | 0.0034641 |
| SO2.t              |    6 | 402 | 0.0057112 |  0.0559066 | 0.0000140 | 0.0002277 | 0.0000208 |   0 |   1.082028 |   1.082028 | 17.908593 | 339.19226 | 0.0027884 |
| VOC.t              |    7 | 402 | 0.0047379 |  0.0406288 | 0.0000225 | 0.0002173 | 0.0000334 |   0 |   0.723802 |   0.723802 | 15.184308 | 251.89230 | 0.0020264 |
| Total.t.CO2e       |    8 | 402 | 3.8162115 | 30.7723669 | 0.0171445 | 0.1191106 | 0.0251975 |   0 | 512.173950 | 512.173950 | 13.299869 | 198.03556 | 1.5347862 |
| CO2.Fossil.t.CO2e  |    9 | 402 | 1.8345859 | 14.4120956 | 0.0169625 | 0.0901948 | 0.0249277 |   0 | 257.559803 | 257.559803 | 14.832228 | 248.47182 | 0.7188100 |
| CO2.Process.t.CO2e |   10 | 402 | 0.1249104 |  1.3012894 | 0.0000000 | 0.0000000 | 0.0000000 |   0 |  18.929557 |  18.929557 | 12.407185 | 160.89258 | 0.0649024 |
| CH4.t.CO2e         |   11 | 402 | 0.5452572 |  5.9006979 | 0.0000000 | 0.0000000 | 0.0000000 |   0 | 108.460987 | 108.460987 | 15.916464 | 278.74807 | 0.2943001 |
| HFC.PFCs.t.CO2e    |   12 | 402 | 0.0219541 |  0.2509943 | 0.0000000 | 0.0000000 | 0.0000000 |   0 |   4.161163 |   4.161163 | 13.617934 | 201.14719 | 0.0125185 |
| Fugitive.kg        |   13 | 402 | 0.1221874 |  0.9156211 | 0.0001560 | 0.0036797 | 0.0002313 |   0 |  12.189404 |  12.189404 | 11.026986 | 130.43964 | 0.0456670 |
| Stack.kg           |   14 | 402 | 0.4206834 |  2.7331204 | 0.0003090 | 0.0090083 | 0.0004581 |   0 |  33.321666 |  33.321666 |  8.712720 |  82.72182 | 0.1363157 |
| Total.Air.kg       |   15 | 402 | 0.5428712 |  3.4459035 | 0.0005675 | 0.0135676 | 0.0008414 |   0 |  37.092916 |  37.092916 |  8.377980 |  73.86411 | 0.1718660 |
| Surface.water.kg   |   16 | 402 | 0.1281632 |  1.0648699 | 0.0000000 | 0.0003837 | 0.0000000 |   0 |  16.404607 |  16.404607 | 12.738129 | 175.01346 | 0.0531109 |
| U\_ground.Water.kg |   17 | 402 | 0.0643912 |  0.5634316 | 0.0000000 | 0.0000000 | 0.0000000 |   0 |   8.839104 |   8.839104 | 12.131672 | 164.85908 | 0.0281014 |
| Land.kg            |   18 | 402 | 0.3012675 |  3.2804508 | 0.0000000 | 0.0003429 | 0.0000000 |   0 |  59.730055 |  59.730055 | 15.784280 | 271.04069 | 0.1636140 |
| Offiste.kg         |   19 | 402 | 0.1176076 |  0.9913416 | 0.0000885 | 0.0029975 | 0.0001312 |   0 |  18.394299 |  18.394299 | 16.006124 | 285.75539 | 0.0494436 |
| POTW.Metal.kg      |   20 | 402 | 0.0007064 |  0.0036614 | 0.0000000 | 0.0000332 | 0.0000000 |   0 |   0.040218 |   0.040218 |  7.481724 |  61.88361 | 0.0001826 |

# Clustering for the data

# Fit Models

## user-define function

``` r
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
  Xy <- cbind(dat %>% select(Sector), log(Xy)) 
  Xy <- Xy[!is.infinite(rowSums(Xy)),] %>% mutate(Sector= Sector %>% as.factor())
  colnames(Xy) <- colnames(Xy) %>% stringr::str_replace_all("\\.","") 
  return(Xy)
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
```

## Linear Regression Modeling with log10 Transformation on Input and Output

``` r
# create a dataframe with a column with impact variable names 
target_list <- tibble(target = c(colnames(CPA),colnames(GHG),colnames(TOX))); target_list %>% flatten() %>% unlist()
#>  [1] "CO.t"               "NH3.t"              "NOx.t"             
#>  [4] "PM10.t"             "PM2.5.t"            "SO2.t"             
#>  [7] "VOC.t"              "Total.t.CO2e"       "CO2.Fossil.t.CO2e" 
#> [10] "CO2.Process.t.CO2e" "CH4.t.CO2e"         "HFC.PFCs.t.CO2e"   
#> [13] "Fugitive.kg"        "Stack.kg"           "Total.Air.kg"      
#> [16] "Surface.water.kg"   "U_ground.Water.kg"  "Land.kg"           
#> [19] "Offiste.kg"         "POTW.Metal.kg"

# model selection for each impact variable
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
# extract coefficient from the best model of each impact variable
coef_list <- bestglm_list %>% 
  mutate(coefs = best_model %>% purrr::map(.f=broom::tidy)) %>% 
  select(target, coefs) %>% 
  tidyr::unnest(coefs) %>% 
  select(target, term, estimate) %>% 
  tidyr::spread(key= term, value = estimate)
# extract p-value for each paramters of the best model of each impact variable
signif_list <- bestglm_list %>% 
  mutate(coefs = best_model %>% purrr::map(.f=broom::tidy)) %>% 
  select(target, coefs) %>% 
  tidyr::unnest(coefs) %>% 
  select(target, term, p.value) %>% 
  tidyr::spread(key= term, value = p.value)
# combind coefficient and p-value to result in a tidy table of the result
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
coef_signif_list$sum_elastic <- coef_list[,3:8] %>% rowSums(na.rm = TRUE) %>% signif(digits = 3)
coef_signif_list %>% 
  select(target, sum_elastic, everything()) %>% 
  arrange(desc(adj.r.squared)) %>%
  knitr::kable(format = "markdown") 
```

| target            | sum\_elastic | r.squared | adj.r.squared | p.value | (Intercept)    | BioWasteTJ    | CoalTJ        | NatGaseTJ     | NonFossElecTJ | PetrolTJ      | WaterWithdrawalsKgal |
| :---------------- | -----------: | --------: | ------------: | ------: | :------------- | :------------ | :------------ | :------------ | :------------ | :------------ | :------------------- |
| CO2.Fossil.t.CO2e |        0.906 | 0.9910351 |     0.9908457 |       0 | \-2.48(\*\*\*) | 0.262(\*\*\*) | 0.237(\*\*\*) | 0.658(\*\*\*) | \-0.252(\*)   |               |                      |
| Total.t.CO2e      |        0.923 | 0.9828135 |     0.9823259 |       0 | \-1.6(\*\*\*)  | 0.244(\*\*\*) | 0.334(\*\*\*) | 0.573(\*\*\*) | \-0.229(\*)   |               |                      |
| SO2.t             |        0.988 | 0.9283235 |     0.9267985 |       0 | \-0.76(\*\*)   | 0.18(\*\*\*)  | 0.172(\*\*)   | 0.636(\*\*\*) |               |               |                      |
| NOx.t             |        1.100 | 0.9117863 |     0.9099226 |       0 | 3.39(\*\*\*)   |               |               |               | 1.1(\*\*\*)   |               |                      |
| PM10.t            |        0.847 | 0.8891458 |     0.8860010 |       0 | 0.842          |               | 0.847(\*\*\*) |               |               |               |                      |
| PM2.5.t           |        0.962 | 0.8781678 |     0.8745845 |       0 | \-4.38(\*\*\*) | 0.237(\*\*)   |               | 0.725(\*\*\*) |               |               |                      |
| NH3.t             |        1.020 | 0.7683303 |     0.7642299 |       0 | 5.29(\*\*\*)   |               | 0.141(\*\*\*) | 0.68(\*\*\*)  |               | 0.195(\*\*\*) |                      |
| CO.t              |        0.951 | 0.7511409 |     0.7476604 |       0 | \-0.948(\*)    | 0.275(\*\*\*) |               | 0.676(\*\*\*) |               |               |                      |
| VOC.t             |        1.070 | 0.6905785 |     0.6862205 |       0 | 2.74(\*\*\*)   |               |               |               | 1.07(\*\*\*)  |               |                      |
| Total.Air.kg      |        1.230 | 0.6697148 |     0.6649625 |       0 | 5.2(\*\*\*)    |               |               |               | 1.23(\*\*\*)  |               |                      |
| Fugitive.kg       |        0.890 | 0.6404521 |     0.6378467 |       0 | \-2.31(\*\*)   |               |               |               | 0.89(\*\*\*)  |               |                      |
| Surface.water.kg  |        1.500 | 0.6229475 |     0.6194563 |       0 | 2.97(\*\*\*)   |               |               | 1.5(\*\*\*)   |               |               |                      |
| Stack.kg          |        1.050 | 0.5949768 |     0.5920418 |       0 | 0.945(\*\*\*)  |               | 0.533(\*\*\*) | 0.166(\*)     |               | 0.35(\*\*\*)  |                      |
| POTW.Metal.kg     |        0.969 | 0.4867847 |     0.4817033 |       0 | \-1.03(\*)     | 0.215(\*\*)   |               |               | 0.755(\*\*\*) |               |                      |
| Offiste.kg        |        1.040 | 0.4845963 |     0.4806316 |       0 | 5.8(\*\*\*)    |               | 0.165(\*\*\*) | 0.707(\*\*\*) |               | 0.204(\*\*\*) | \-0.0377(\*)         |
| Land.kg           |        1.150 | 0.4658109 |     0.4611250 |       0 | 5.65(\*\*\*)   |               |               |               | 0.9(\*\*\*)   | 0.249(\*\*)   |                      |

# Communciate and visualize the results

## The linear model with R^2 \>0.75

``` r
good_lm <- bestglm_list %>% filter(adj.r.squared >0.75)
good_lm %>% arrange(desc(adj.r.squared)) %>% select(target) %>% flatten() %>% unlist
#> [1] "CO2.Fossil.t.CO2e" "Total.t.CO2e"      "SO2.t"            
#> [4] "NOx.t"             "PM10.t"            "PM2.5.t"          
#> [7] "NH3.t"
```

## The diagnosis of linear model

``` r
par(mfrow=c(2,3))
plot(good_lm$best_model[[1]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
plot(good_lm$best_model[[2]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-2.png" width="100%" />

``` r
plot(good_lm$best_model[[3]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-3.png" width="100%" />

``` r
plot(good_lm$best_model[[4]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-4.png" width="100%" />

``` r
plot(good_lm$best_model[[5]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-5.png" width="100%" />

``` r
plot(good_lm$best_model[[6]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-6.png" width="100%" />

``` r
plot(good_lm$best_model[[7]], which=1:6)
```

<img src="man/figures/README-unnamed-chunk-9-7.png" width="100%" />

# Result

## Impact between Sector

``` r
# descriptive analysis
good_lm$target
#> [1] "NH3.t"             "NOx.t"             "PM10.t"           
#> [4] "PM2.5.t"           "SO2.t"             "Total.t.CO2e"     
#> [7] "CO2.Fossil.t.CO2e"
# NOx.t: Emissions of Nitrogen Oxides to Air from each sector. t = meric tons
# SO2.t: Emissions of Sulfur Dioxide to Air from each sector. t = meric tons 
# Total.t.CO2e: Global Warming Potential (GWP) is a weighting of greenhouse gas emissions into the air from the production of each sector. Weighting factors are 100-year GWP values from the IPCC Second Assessment Report (IPCC 2001). t CO2e = metric tons of CO2 equivalent emissions. 
# CO2.Fossil.t.CO2e C: Emissions of Carbon Dioxide (CO2) into the air from each sector from fossil fuel combustion sources. t CO2e = metric tons of CO2 equivalent.
par(mfrow=c(1,2))
plot(good_lm$data[[1]][,ncol(good_lm$data[[1]])])
plot(good_lm$data[[1]][,1], good_lm$data[[1]][,ncol(good_lm$data[[1]])])
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r
plot(good_lm$data[[2]][,ncol(good_lm$data[[2]])])
plot(good_lm$data[[2]][,1], good_lm$data[[2]][,ncol(good_lm$data[[2]])])
```

<img src="man/figures/README-unnamed-chunk-10-2.png" width="100%" />

``` r
plot(good_lm$data[[3]][,ncol(good_lm$data[[3]])])
plot(good_lm$data[[3]][,1], good_lm$data[[3]][,ncol(good_lm$data[[3]])])
```

<img src="man/figures/README-unnamed-chunk-10-3.png" width="100%" />

``` r
plot(good_lm$data[[4]][,ncol(good_lm$data[[4]])])
plot(good_lm$data[[4]][,1], good_lm$data[[4]][,ncol(good_lm$data[[4]])])
```

<img src="man/figures/README-unnamed-chunk-10-4.png" width="100%" />
