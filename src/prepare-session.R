#===============================================================================
# 2020-09-15 -- indexation
# prepare session
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================    
library(tidyverse)
library(magrittr)
library(patchwork)
library(paletteer)
library(hrbrthemes)
library(sf)

library(showtext)
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()
library(ggdark)
library(cowplot)
library(ggforce)

library(DemoTools)

source("R/fun-inset-cph-box.R")

source("R/fun-topals-fit.R")
source("R/fun-show-topals.R")