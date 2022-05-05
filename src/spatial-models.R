#===============================================================================
# 2020-09-15 -- indexation
# Spatial models for non survival
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

source("src/prepare-session.R")


# load data
load("dat/topals-pooled-5y.rda")
load("dat/geodat-dk.rda")

load("dat/df_le_comapre.rda")
load("dat/non-surv-5065.rda")


# correlate q5065 and e0

df <- left_join(df_le_comapre, df_q5065)

df %>%
    ggplot(aes(life_exp, q5065))+
    geom_point()+
    facet_grid(period~sex)

df %>%
    filter(!sex=="b") %>%
    group_by(period, id) %>%
    summarise(
        e = e,
        r_e0 = life_exp[1] / life_exp[2],
        r_q5065 = q5065[1] / q5065[2]
    ) %>%
    ggplot(aes(r_e0, r_q5065, size = e))+
    stat_smooth(geom = "path", method = "lm", se = F, show.legend = FALSE)+
    geom_point(shape = 1)+
    scale_x_continuous(trans = "log10")+
    scale_y_continuous(trans = "log10")+
    scale_size_area(max_size = 8, breaks = c(100, 250, 500))+
    facet_grid(~period)+
    dark_theme_minimal(base_family = font_rc)+
    theme(
        legend.position = c(.6,.02),
        legend.justification = c(1,0)
    )+
    labs(
        x = "Sex ratio of life expectancy at birth",
        y = "Sex ratio of non-survival 50-65",
        size = "Populaion,\nthous."
    )



ggsave(filename = "fig/life-exp-non-surv-sex-ratios.pdf",
       width = 7, height = 4)
