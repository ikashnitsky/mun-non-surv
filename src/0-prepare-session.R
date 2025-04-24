#===============================================================================
# 2025-04-24 -- mun-non-surv
# Calculate Danish municipal life tables using TOPALS
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================



# pacman ------------------------------------------------------------------


# First, install pacman to deal easier with other packages
# solution by Sacha Epskamp from: http://stackoverflow.com/a/9341833/4638884
if (!require('pacman',character.only = TRUE)){
    install.packages('pacman', dep = TRUE)
    if (!require('pacman', character.only = TRUE))
        stop("Package not found")
}

# from github -------------------------------------------------------------



# ggflags, if not installed -- install
if (!require('ggflags',character.only = TRUE)){
    remotes::install_github("jimjam-slam/ggflags", upgrade = "never")
    if (!require('pacman', character.only = TRUE))
        stop("Package not found")
}

# libraries ---------------------------------------------------------------

pacman::p_load(
    install = T, update = F,

    tidyverse,
    magrittr,
    fs,
    readxl,
    devtools,
    remotes,
    patchwork,
    vroom,
    sf,
    rmapshaper,
    rgeoda,
    matrixStats,
    janitor,
    showtext,
    hrbrthemes,
    ggflags,
    ggforce,
    sysfonts,
    cowplot,
    prismatic,
    pdftools
)


# fonts -------------------------------------------------------------------

sysfonts::font_add_google("Roboto Condensed", "rc")
sysfonts::font_add_google("Outfit", "ou")
sysfonts::font_add_google("Atkinson Hyperlegible", "ah")
showtext_auto()


# custom ggplot theme -----------------------------------------------------

devtools::source_gist("653e1040a07364ae82b1bb312501a184")
theme_set(theme_ik(base_family = "ah"))

options(scipen = 999)


# chosen colors -----------------------------------------------------------

# Denmark and Sweden
pal_dkse <- c("#B5223BFF", "#64B6EEFF")

# females and males
pal <- c("#b085f5", "#bb4d00")

# load own functions ------------------------------------------------------

source("src/1-own-functions.R")


# session info ------------------------------------------------------------

# > sessionInfo()
# R version 4.4.3 (2025-02-28 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 10 x64 (build 19045)
#
# Matrix products: default
#
#
# locale:
#     [1] LC_COLLATE=English_United States.utf8
# [2] LC_CTYPE=English_United States.utf8
# [3] LC_MONETARY=English_United States.utf8
# [4] LC_NUMERIC=C
# [5] LC_TIME=English_United States.utf8
#
# time zone: Europe/Copenhagen
# tzcode source: internal
#
# attached base packages:
#     [1] stats     graphics  grDevices utils     datasets
# [6] methods   base
#
# other attached packages:
#     [1] ungroup_1.4.4    prismatic_1.1.2
# [3] cowplot_1.1.3    ggforce_0.4.2
# [5] hrbrthemes_0.8.7 showtext_0.9-7
# [7] showtextdb_3.0   sysfonts_0.8.9
# [9] janitor_2.2.1    rmapshaper_0.5.0
# [11] sf_1.0-19        vroom_1.6.5
# [13] patchwork_1.3.0  remotes_2.5.0
# [15] devtools_2.4.5   usethis_3.1.0
# [17] fs_1.6.5         magrittr_2.0.3
# [19] lubridate_1.9.4  forcats_1.0.0
# [21] stringr_1.5.1    dplyr_1.1.4
# [23] purrr_1.0.4      readr_2.1.5
# [25] tidyr_1.3.1      tibble_3.2.1
# [27] ggplot2_3.5.1    tidyverse_2.0.0
# [29] ggflags_0.0.4    pacman_0.5.1
#
# loaded via a namespace (and not attached):
#     [1] Rdpack_2.6.2            DBI_1.2.3
# [3] pbapply_1.7-2           httr2_1.1.0
# [5] readxl_1.4.3            rlang_1.1.5
# [7] snakecase_0.11.1        e1071_1.7-16
# [9] compiler_4.4.3          systemfonts_1.2.1
# [11] vctrs_0.6.5             profvis_0.4.0
# [13] pkgconfig_2.0.3         crayon_1.5.3
# [15] fastmap_1.2.0           ellipsis_0.3.2
# [17] promises_1.3.2          sessioninfo_1.2.3
# [19] tzdb_0.4.0              bit_4.5.0.1
# [21] cachem_1.1.0            jsonlite_1.8.9
# [23] later_1.4.1             tweenr_2.0.3
# [25] parallel_4.4.3          R6_2.6.1
# [27] stringi_1.8.4           extrafontdb_1.0
# [29] pkgload_1.4.0           cellranger_1.1.0
# [31] Rcpp_1.0.14             extrafont_0.19
# [33] gitcreds_0.1.2          Matrix_1.7-2
# [35] httpuv_1.6.15           timechange_0.3.0
# [37] tidyselect_1.2.1        rstudioapi_0.17.1
# [39] miniUI_0.1.1.1          curl_6.2.0
# [41] pkgbuild_1.4.6          lattice_0.22-6
# [43] shiny_1.10.0            withr_3.0.2
# [45] units_0.8-5             proxy_0.4-27
# [47] urlchecker_1.0.1        polyclip_1.10-7
# [49] pillar_1.10.1           KernSmooth_2.23-26
# [51] generics_0.1.3          sp_2.2-0
# [53] hms_1.1.3               munsell_0.5.1
# [55] scales_1.3.0            xtable_1.8-4
# [57] class_7.3-23            glue_1.8.0
# [59] gdtools_0.4.1           tools_4.4.3
# [61] grid_4.4.3              Rttf2pt1_1.3.12
# [63] rbibutils_2.3           gh_1.4.1
# [65] colorspace_2.1-1        cli_3.6.4
# [67] rappdirs_0.3.3          fontBitstreamVera_0.1.1
# [69] V8_6.0.1                gtable_0.3.6
# [71] digest_0.6.37           fontquiver_0.2.1
# [73] classInt_0.4-11         htmlwidgets_1.6.4
# [75] farver_2.1.2            memoise_2.0.1
# [77] htmltools_0.5.8.1       lifecycle_1.0.4
# [79] httr_1.4.7              mime_0.12
# [81] fontLiberation_0.1.0    bit64_4.6.0-1
# [83] MASS_7.3-64
