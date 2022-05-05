#===============================================================================
# 2019-12-17 -- sex gap
# Functions to read local HMD
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================



library(tidyverse)
library(vroom)
library(fs)

# path to local HMD
hmdpath <- fs::as_fs_path("~/data/hmd/")

# customize fread
fread_hmd <- function(x) x %>% data.table::fread(skip = 2, na.strings = ".")

# wrap
fread_hmd_dir <- function(thedir) {
    thedir %>% 
        dir_ls() %>% 
        map_df(fread_hmd, .id = "country") %>% 
        janitor::clean_names() %>% 
        mutate(
            country = country %>% 
                str_remove(thedir %>% path_real()) %>% 
                str_remove("\\..*") %>% 
                str_remove("\\/"),
            age = age %>% str_remove("\\+") %>% as.integer()
        )
}