#===============================================================================
# 2021-05-19 -- non-surv
# Check 15q50 in Danish cohorts -- HMD
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================


library(tidyverse)
library(magrittr)
library(patchwork)
library(cowplot)
library(hrbrthemes)
library(MortalityLaws)

# functions to read local HMD directories
devtools::source_gist("0f93062f2b67eeac69949554027fa84f")


dk_coh_m <- fread_hmd("~/data/hmd/c_lt_male/mltcoh_1x1/DNK.mltcoh_1x1.txt")

dk_coh_m$Year %>% range()


dk_dr <- fread_hmd("~/data/hmd/c_death_rates/cMx_1x1/DNK.cMx_1x1.txt") %>% 
    janitor::clean_names()

q5065 <- dk_dr %>% 
    pivot_longer(female:total, names_to = "sex") %>% 
    group_by(year, sex) %>% 
    mutate(
        px = exp(-value)
    ) %>% 
    filter(age %in% 50:65) %>% 
    drop_na() %>% 
    group_by(year, sex) %>% 
    summarise(p5065 = prod(px)) %>% 
    ungroup() %>% 
    mutate(q5065 = 1 - p5065)


q5065 %>% 
    filter(year > 1899, !sex=="total") %>% 
    ggplot(aes(year, q5065, color = sex, group = sex))+
    geom_rect(
        aes(xmin = 1953, xmax = 1968, ymin = -Inf, ymax = Inf),
        color = NA, fill = "#eaeaea"
    )+
    geom_point()+
    stat_smooth(se = F, color = "#ffffff", size = 1.5)+
    stat_smooth(se = F, size = 1)+
    theme_minimal()+
    theme(legend.position = "bottom")

ggsave("~/Downloads/cohort-q5065.png", width = 6, height = 5)
