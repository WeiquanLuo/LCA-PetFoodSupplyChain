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
  full_join(conv, by = "Sector") %>%
  full_join(fuels, by = "Sector") %>% 
  full_join(gwp, by = "Sector") %>% 
  full_join(trisect, by = "Sector") %>% 
  full_join(water, by = "Sector")

# fill the sector_sub to 6 digit
round26Sector <- function(sector){
  sector <- sector %>% as.character()
  myfun <- function(s){
    if (6 - nchar(s) == 1L) s <- paste0(s,"0")
    if (6 - nchar(s) == 2L) s <- paste0(s,"00")
    if (6 - nchar(s) == 3L) s <- paste0(s,"000")
    if (6 - nchar(s) == 4L) s <- paste0(s,"0000")
    if (6 - nchar(s) == 5L) s <- paste0(s,"00000")
    return(s)
    }
  sector <- map(sector, myfun) %>% unlist
  return(sector)
}
dat <- dat %>% 
  mutate(Sector_sub = round26Sector(Sector)) %>% 
  select(Sector_sub, everything(), -Sector) %>% 
  mutate(Sector_sub = as.numeric(Sector_sub))

# bind sector and sector_sub
cols <- read.csv("data/NAICS_2002_completed.csv")
dat2 <- left_join(dat, cols, by= c("Sector_sub" = "Sector_sub")) %>% select(Sector, Description, name_sub, everything())

write.csv(dat2, "data/dat_311111_1M.csv")


