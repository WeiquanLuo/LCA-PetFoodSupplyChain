# obtain the section name in a table
# https://www.census.gov/cgi-bin/sssd/naics/naicsrch?chart=2002

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())
dev.off()

library(rvest)
library(tidyverse)
library(furrr)
library(purrr)
library(tictoc)
# plan(multiprocess) # switch to parallel computing

url <- "https://www.census.gov/cgi-bin/sssd/naics/naicsrch?chart=2002"
s <- html_session(url)

# tb: sector name
NAICS_2002 <- read_html(s) %>% html_table(); NAICS_2002 <- NAICS_2002[[1]]
names(NAICS_2002) <- NAICS_2002[1,]; NAICS_2002 <- NAICS_2002[-1,]; NAICS_2002
NAICS_2002 <- NAICS_2002 %>% mutate(Sector = Sector %>% as.character())
# write_csv(NAICS_2002, "data/NAICS_2002.csv")
# after modification
NAICS_2002 <- read_csv("data/NAICS_2002.csv")
sector <- NAICS_2002[1:20,] %>% select(Sector) %>% flatten() %>% unlist
links <- map(sector, function(sector) paste0("https://www.census.gov/cgi-bin/sssd/naics/naicsrch?chart_code=",
                                    sector, 
                                    "&search=2002%20NAICS%20Search")) %>% unlist; links
get_tb <- function(link){
  NAICS_2002_sub <- read_html(link) %>% html_table() %>% as.data.frame() %>% flatten() %>% unlist; NAICS_2002_sub
  Sector_sub <- sapply(strsplit(NAICS_2002_sub, split='\n', fixed=TRUE), function(x) (x[1])) %>% 
    str_extract("[[:digit:]]*"); Sector_sub # keep digit
  name_sub <- sapply(strsplit(NAICS_2002_sub, split='\n', fixed=TRUE), function(x) (x[2])) %>%
    str_replace("[[:space:]]{4}",""); name_sub # remove space at the beginning
  output_tb <- tibble(Sector_sub, name_sub)
}

tb <- map_dfr(links, get_tb)
tb <- tb %>% mutate(Sector = str_sub(Sector_sub, start= 1L, end= 2L))
NAICS_2002 <- NAICS_2002 %>% mutate(Sector = Sector %>% as.character())
NAICS_2002 <- NAICS_2002 %>% left_join(tb, by= "Sector" ); NAICS_2002

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
NAICS_2002 <- NAICS_2002 %>% mutate(Sector_sub = round26Sector(Sector_sub)) %>% distinct()

write_csv(NAICS_2002, "data/NAICS_2002_completed.csv")

NAICS_2002 <- read.csv("data/NAICS_2002_completed.csv")
