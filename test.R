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
  df <- df[,-2]
  return(df)
}


sumSector_sub <- function(df){
  df <- df %>% group_by(Sector) %>% summarise_all(funs(sum))
  return(df)
}


# assign col name to df
eeno <- eeno %>% remove2ndcol %>% setNames(c("Sector", EconomicActivity)) %>% as_tibble() %>% sumSector_sub
conv <- conv %>% remove2ndcol %>% setNames(c("Sector", ConventionalAirPollutants)) %>% as_tibble() %>% sumSector_sub
fuels <- fuels %>% remove2ndcol %>% setNames(c("Sector", Energy)) %>% as_tibble() %>% sumSector_sub
gwp <- gwp %>% remove2ndcol %>% setNames(c("Sector", GreenhoseGases)) %>% as_tibble() %>% sumSector_sub
trisect <- trisect %>% remove2ndcol %>% setNames(c("Sector", ToxicReleases)) %>% as_tibble() %>% sumSector_sub
water <- water %>% remove2ndcol %>% setNames(c("Sector", WaterWithdrawals)) %>% as_tibble() %>% sumSector_sub

# cbind
dat <- eeno %>% 
  left_join(conv, by = "Sector") %>% 
  full_join(fuels, by = "Sector") %>% 
  full_join(gwp, by = "Sector") %>% 
  full_join(trisect, by = "Sector") %>% 
  full_join(water, by = "Sector")

write.csv(dat, "data/dat_311111_1M.csv")


