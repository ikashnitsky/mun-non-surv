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

# library(showtext)
# font_add_google("Roboto Condensed", "Roboto Condensed")
# showtext_auto()
library(ggdark)
library(cowplot)
library(ggforce)
library(prismatic)

# remotes::install_github("jimjam-slam/ggflags")
library(ggflags)

# library(DemoTools)

source("src/fun-inset-cph-box.R")

source("src/fun-topals-fit.R")
source("src/fun-show-topals.R")
