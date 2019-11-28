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








