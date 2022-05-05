#===============================================================================
# 2020-06-10 -- non-surv
# map DK mun e0
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


source("src/prepare-session.R")

# load dat
load("dat/topals-pooled-5y.rda")
load("dat/geodat-dk.rda")

load("dat/df_le_comapre.rda")


# diagonal plot M vs F ----------------------------------------------------
#
df_le_comapre %>%
    filter(!sex=="b") %>%
    pivot_wider(names_from = sex, values_from = life_exp) %>%
    ggplot(aes(f, m-f))+
    geom_abline(slope = 1)+
    geom_point(shape = 1, aes(size = e))+
    stat_smooth(geom = "path", method = "lm", se = F)+
    scale_size_area(breaks = c(25, 50, 100, 250, 500))+
    facet_row(~period)+
    dark_theme_minimal(base_family = font_rc)+
    theme(legend.position = c(.55,.02),
          legend.justification = c(1,0))+
    labs(y = "Sex gap in life expectancy",
         x = "Female life expectancy",
         size = "Populaion,\nthous.")

gg_diag_le_sex <- last_plot()

ggsave(filename = "fig/life-exp-sex-gap.pdf", gg_diag_le_sex,
       width = 7, height = 4)



# map ---------------------------------------------------------------------

df_map <- df_le_comapre %>%
    left_join(gd_dk_mun_s %>% select(-name), by = "id") %>%
    st_as_sf() %>%
    st_transform(crs = 3044)

# 2015-19, by sex
df_map %>%
    filter(!sex=="b", period == "2015-2019") %>%
    ungroup() %>%
    mutate(sex = sex %>% lvls_revalue(c("FEMALE", "MALE"))) %>%
    ggplot()+
    geom_sf(aes(fill = life_exp, geometry = geometry), color = NA)+
    geom_sf(dat = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(palette = "YlGnBu", direction = 1)+
    facet_grid(~sex)+
    cowplot::theme_map(font_family = font_rc, font_size = 16)+
    theme(legend.position = c(1, .9),
          legend.justification = c(1, 1),
          legend.background = element_blank(),
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Life expectancy at birth",
         subtitle = "TOPALS model, pooled dat 2015-19",
         caption = "dat: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

map_le_sex <- last_plot()

ggsave("fig/maps-life-expectancy-sex.pdf", map_le_sex,
       width = 12, height = 6)



# proportion of non-survival 50-65 ----------------------------------------

load("dat/non-surv-5065.rda")

# map prob of dying between 50 and 65
dm_q5065 %>%
    filter(!sex=="b") %>%
    ungroup() %>%
    mutate(sex = sex %>% lvls_revalue(c("FEMALE", "MALE"))) %>%
    ggplot()+
    geom_sf(aes(fill = q5065, geometry = geometry), color = NA)+
    geom_sf(dat = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(palette = "BrBG",
                         breaks = seq(.06, .16, .02))+
    facet_grid(sex~period)+
    cowplot::theme_map(font_family = font_rc, font_size = 16)+
    theme(legend.position = c(.43, .5),
          legend.justification = c(.5, .5),
          legend.background = element_blank(),
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Probability of dying between 50 and 65",
         caption = "dat: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

map_q5065_period_sex <- last_plot()

ggsave("fig/maps-q5065-sex.pdf", map_q5065_period_sex,
       width = 10, height = 8.5)



# step cumulative ----------------------------------------------------

pal <- c("#80deea", "#ffff6e")

df_le_comapre %>%
    filter(!sex=="b") %>%
    ungroup() %>%
    mutate(sex = sex %>% lvls_revalue(c("FEMALE", "MALE"))) %>%
    group_by(period, sex) %>%
    arrange(life_exp) %>%
    mutate(
        prop_e = e %>% prop.table() %>% cumsum()
    ) %>%
    ungroup() %>%
    ggplot(aes(prop_e, life_exp, color = period))+
    geom_vline(xintercept = .5, size = .5, color = "#fafafa")+
    geom_step(size = 1)+
    geom_text(
      dat = . %>%
          select(period, sex) %>%
          distinct() %>%
          filter(sex == "FEMALE"),
      x = c(.25, .75), y = c(83.5, 81.5),
      label = c("2015-19", "2010-14"),
      size = 5, family = font_rc, color = pal %>% rev
    )+
    scale_x_percent()+
    scale_color_manual(values = pal)+
    facet_row(~sex)+
    dark_theme_minimal(base_family = font_rc)+
    theme(
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")
    )+
    labs(y = "Life expectancy",
         x = "Cumulative proprotion of the population",
         size = "Populaion,\nthous.")

gg_step_cum <- last_plot()

ggsave(filename = "fig/life-exp-step-cum.pdf", gg_step_cum,
       width = 7, height = 4)


