# set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()
library(dplyr)

#multiple csv input
temp = list.files(path = "data", pattern="*xls$")
tempname = sapply(strsplit(temp, split='.', fixed=TRUE), function(x) (x[1]))
for (i in 1:length(temp)){
  assign(tempname[i], gdata::read.xls(file.path("data", temp[i])) %>% 
           select(-"X.."))
}

#heatmap(as.matrix.data.frame(data))
