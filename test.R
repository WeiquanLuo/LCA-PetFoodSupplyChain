# set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()
library(dplyr)
library(purrr)
library(stringr)
library(plyr)


#multiple csv input
temp = list.files(path = "data", pattern="*xls$")
tempname = sapply(strsplit(temp, split='.', fixed=TRUE), function(x) (x[1] %>% str_replace("311111_1M_","") %>% str_replace("_all","")))
for (i in 1:length(temp)){
  assign(tempname[i], gdata::read.xls(file.path("data", temp[i])) %>% 
           select(-"X.."))
}

# obtain colname as individual vector
cols <- readxl::read_excel("data/Columns name.xlsx", col_names=FALSE)
for (i in 1: nrow(cols)){
  col <-cols[i,]
  col <- col %>% flatten_chr() %>% na.omit()
  col1 <- str_replace_all(col[1], "[[:space:]]",""); print(col1)
  assign(col1, col[-1])
}

remove2ndcol <- function(df){
  df<- df[,-2]
  return(df)
}

# assign col name to df
eeno %>% remove2ndcol %>% setNames(c("Sector", EconomicActivity)) %>% as_tibble()
conv %>% remove2ndcol %>% setNames(c("Sector", ConventionalAirPollutants)) %>% as_tibble()
fuels %>% remove2ndcol %>% setNames(c("Sector", Energy)) %>% as_tibble()
gwp %>% remove2ndcol %>% setNames(c("Sector", GreenhoseGases)) %>% as_tibble()
trisect %>% remove2ndcol %>% setNames(c("Sector", ToxicReleases)) %>% as_tibble()
water %>% remove2ndcol %>% setNames(c("Sector", WaterWithdrawals)) %>% as_tibble()