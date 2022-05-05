#===============================================================================
# 2020-10-29 -- indexation
# compare DK and SE  -- municipal life expectancies
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


source("R/prepare-session.R")

load("data/df_le_comapre.rda")
load("data/sw-df_le_comapre.rda")

# merge for comparison
df <- bind_rows(
    df_le_comapre %>% mutate(country = "Denmark"),
    sw_df_le_comapre  %>% mutate(country = "Sweden")
) %>% 
    # add proportion of population in a county
    group_by(country, period, sex) %>% 
    arrange(life_exp) %>% 
    mutate(
        prop_e = e %>% prop.table() %>% cumsum()
    ) %>% 
    ungroup()


# distribution of pop sizes ------------------------------------------------

df %>% 
    filter(sex=="b", period == "2015-2019") %>% 
    ggplot(aes(e, color = country))+
    geom_hline(yintercept = .5, size = .5, color = "#fafafa")+
    stat_ecdf(size = 1)+
    scale_x_continuous(trans = "log", breaks = c(5, 10, 25, 50, 100, 250, 500))+
    scale_y_percent()+
    scale_color_manual(values = c(2, 7), guide = F)+
    dark_theme_minimal(base_family = font_rc)+
    labs(
        x = "Municipality population size, thous.",
        y = "Cumulative density"
    )

ggsave(filename = "figures/dksw-mun-pop-size.pdf", 
       width = 7, height = 4)


# compare cumulative e0 distributions -------------------------------------

df %>% 
    filter(!sex=="b", period == "2015-2019") %>% 
    ungroup() %>% 
    mutate(sex = sex %>% lvls_revalue(c("FEMALE", "MALE"))) %>%
    ggplot(aes(prop_e, life_exp, color = country))+
    geom_vline(xintercept = .5, size = .5, color = "#fafafa")+
    geom_step(size = 1)+
    scale_color_manual(values = c(2, 7), guide = F)+
    facet_row(~sex)+
    dark_theme_minimal(base_family = font_rc)+
    labs(
        x = "Life expectancy",
        y = "Cumulative density population proportions"
    )+
    geom_text(
        data = . %>% 
            select(country, sex) %>% 
            distinct() %>% 
            filter(sex == "FEMALE"),
        x = c(.25, .75), y = c(85, 82),
        label = c("Sweden", "Denmark"), 
        size = 5, family = font_rc, color = c(7, 2)
    )

ggsave(filename = "figures/dksw-life-exp-densities.pdf", 
       width = 7, height = 4)

# diagonal plot M vs F ----------------------------------------------------
  
df %>% 
    filter(!sex=="b") %>% 
    pivot_wider(names_from = sex, values_from = life_exp) %>% 
    ggplot(aes(f, m-f))+
    geom_abline(slope = 1)+
    geom_point(shape = 1, aes(size = e, color = country))+
    stat_smooth(geom = "path", method = "lm", se = F)+
    scale_color_manual(values = c(2, 7), guide = F)+
    scale_size_area(breaks = c(25, 50, 100, 250, 500))+
    facet_grid(country~period)+
    dark_theme_minimal(base_family = font_rc)+   
    theme(
        panel.spacing = unit(2, "lines"),
        legend.position = c(.6, .3), 
        legend.justification = c(1,0)
    )+
    labs(
        y = "Sex gap in life expectancy",
        x = "Female life expectancy",
        size = "Populaion,\nthous."
    )

gg_diag_le_sex <- last_plot()

ggsave(filename = "figures/dksw-life-exp-sex-gap.pdf", gg_diag_le_sex, 
       width = 7, height = 7)
