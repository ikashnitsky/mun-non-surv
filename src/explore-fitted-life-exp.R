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




# compare TOPALS vs DST ---------------------------------------------------

load("dat/comapre-topals-dst-b.rda")

# diagonal plot
both_comapre %>%
    ggplot(aes(dst_le, life_exp, size = e))+
    geom_abline(slope = 1)+
    geom_point(shape = 1, color = "#4a4a4a")+
    scale_size_area(breaks = c(25, 50, 100, 250, 500))+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = c(.95,.05),
        legend.justification = c(1,0),
        plot.background = element_rect(fill = "#dadada", color = NA)
    )+
    labs(y = "TOPALS fitted life expectancy",
         x = "DST estimated life expectancy",
         size = "Populaion,\nthous.")

gg_compare_diag <- last_plot()

# DST vs difference
both_comapre %>%
    ggplot(aes(dst_le, (life_exp - dst_le), size = e))+
    geom_hline(yintercept = 0)+
    geom_point(shape = 1, color = "#4a4a4a")+
    scale_size_area(breaks = c(25, 50, 100, 250, 500), guide = "none")+
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_rect(fill = "#dadada", color = NA)
    )+
    labs(y = "TOPALS minus DST",
         x = "DST estimated life expectancy",
         size = "Populaion,\nthous.")

gg_compare_diff <- last_plot()

gg_compare <- (gg_compare_diag + gg_compare_diff)

ggsave(filename = "fig/compare-life-exp-fit.pdf", gg_compare,
       width = 7, height = 4, device = cairo_pdf)

# map the results
load("dat/geodata-dk.rda")

both_map <- both_comapre %>%
    left_join(gd_dk_mun_s %>% select(-name), by = "id") %>%
    st_as_sf() %>%
    st_transform(crs = 3044)

# DK mun -- life exp via TOPALS
both_map %>%
    ggplot()+
    geom_sf(aes(fill = life_exp, geometry = geometry), color = NA)+
    # geom_sf(data = bord, color = "#ffffff", size = .5)+
    geom_sf(data = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(palette = "YlGnBu", direction = 1)+
    # scale_fill_manual(values = pal.25, guide = guide_legend(ncol = 1))+
    cowplot::theme_map(font_family = font_rc, font_size = 16)+
    theme(legend.position = c(1,  .9),
          legend.justification = c(1, 1),
          legend.background = element_blank(),
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Life expectancy at birth",
         subtitle = "TOPALS model, pooled data 2015-19",
         caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

map_le <- last_plot()

map_le_zoom <- map_le %>% inset_cph_box()


# difference from DST
both_map %>%
    ggplot()+
    geom_sf(aes(fill = life_exp - dst_le, geometry = geometry), color = NA)+
    # geom_sf(data = bord, color = "#ffffff", size = .5)+
    geom_sf(data = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(palette = "BrBG", breaks = c( -.25, -.1, .1, .25))+
    # scale_fill_manual(values = pal.25, guide = guide_legend(ncol = 1))+
    cowplot::theme_map(font_family = font_rc, font_size = 16)+
    theme(legend.position = c(1,  .9),
          legend.justification = c(1, 1),
          legend.background = element_blank(),
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Gap in life expectancy estimates",
         subtitle = "TOPALS minus DST",
         caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

map_le_diff <- last_plot()

map_le_diff_zoom <- map_le_diff %>% inset_cph_box()

maps_le <- map_le_zoom + map_le_diff_zoom

ggsave("fig/maps-life-expectancy-check.pdf", maps_le,
       width = 12, height = 6)



# DK mun -- life exp via TOPALS
both_map %>%
    # mutate(rel_e0 = life_exp %>% relative_categories(dk_e0)) %>% view
    ggplot()+
    geom_sf(aes(fill = life_exp, geometry = geometry), color = NA)+
    geom_sf(data = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(palette = "RdBu", direction = 1,
                         breaks = seq(79.5, 82.5, 1))+
    cowplot::theme_map(font_family = font_rc, font_size = 14)+
    theme(
        legend.position = c(1,  .9),
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "#dadada", color = NA),
        plot.title = element_text(family = "Roboto Slab",
                                  face = 2, size = 20),
        panel.spacing = unit(0, "lines")
    )+
    labs(title = "Life expectancy at birth",
         subtitle = "TOPALS model, pooled data 2015-19\nNational life expectancy at birth is 81 years",
         # caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

div_le <- last_plot()

div_le_zoom <- div_le %>% inset_cph_box(y = .44)

# map prob of dying between 50 and 65
both_map %>%
    ggplot()+
    geom_sf(aes(fill = q5065 * 100, geometry = geometry), color = NA)+
    geom_sf(data = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(palette = "BrBG",
                         breaks = seq(6, 11, 1))+
    cowplot::theme_map(font_family = font_rc, font_size = 14)+
    theme(legend.position = c(1,  .9),
          legend.justification = c(1, 1),
          legend.background = element_blank(),
          plot.background = element_rect(fill = "#dadada", color = NA),
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Probability of dying between 50 and 65",
         subtitle = "TOPALS model, pooled data 2015-19\nNational probability of dying between 50 and 65 is 8.5%",
         # caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

div_q5065 <- last_plot()

div_q5065_zoom <- div_q5065 %>% inset_cph_box(y = .44)

maps_topals_div <- div_le_zoom + div_q5065_zoom

ggsave("fig/maps-le-q5065-div.pdf", maps_topals_div,
       width = 12, height = 6, device = cairo_pdf)




# diagonal plot M vs F ----------------------------------------------------
#
df_le_comapre %>%
    filter(!sex=="b") %>%
    pivot_wider(names_from = sex, values_from = life_exp) %>%
    ggplot(aes(f, m-f))+
    geom_point(shape = 1, aes(size = e), color = "#4a4a4a")+
    geom_smooth(method = "lm", se = F, mapping = aes(weight = e), color = 1, size = 3/4)+
    scale_size_area(breaks = c(25, 50, 100, 250, 500))+
    facet_row(~period)+
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_rect(fill = "#dadada", color = NA),
        legend.position = c(.55,.02),
        legend.justification = c(1,0)
    )+
    labs(y = "Sex gap in life expectancy",
         x = "Female life expectancy",
         size = "Populaion,\nthous.")

gg_diag_le_sex <- last_plot()

ggsave(filename = "fig/life-exp-sex-gap.pdf", gg_diag_le_sex,
       width = 7, height = 4, device = cairo_pdf)



# map ---------------------------------------------------------------------

df_map <- df_le_comapre %>%
    left_join(gd_dk_mun_s %>% select(-name), by = "id") %>%
    st_as_sf() %>%
    st_transform(crs = 3044)

map_le_m <- df_map %>%
    filter(sex=="m", period == "2015-2019") %>%
    ggplot()+
    geom_sf(aes(fill = life_exp, geometry = geometry), color = NA)+
    geom_sf(data = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(palette = "RdBu", direction = 1,
                         breaks = seq(77, 80, 1))+
    cowplot::theme_map(font_family = font_rc, font_size = 14)+
    theme(
        legend.position = c(1,  .9),
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "#dadada", color = NA),
        plot.title = element_text(family = "Roboto Slab",
                                  face = 2, size = 20),
        panel.spacing = unit(0, "lines")
    )+
    labs(title = "Life expectancy at birth, MALES",
         # subtitle = "TOPALS model, pooled data 2015-19\nNational life expectancy at birth is 81 years",
         # caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

map_le_f <- df_map %>%
    filter(sex=="f", period == "2015-2019") %>%
    ggplot()+
    geom_sf(aes(fill = life_exp, geometry = geometry), color = NA)+
    geom_sf(data = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(palette = "RdBu", direction = 1,
                         breaks = seq(81, 84, 1))+
    cowplot::theme_map(font_family = font_rc, font_size = 14)+
    theme(
        legend.position = c(1,  .9),
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "#dadada", color = NA),
        plot.title = element_text(family = "Roboto Slab",
                                  face = 2, size = 20),
        panel.spacing = unit(0, "lines")
    )+
    labs(title = "Life expectancy at birth, FEMALES",
         # subtitle = "TOPALS model, pooled data 2015-19\nNational life expectancy at birth is 81 years",
         # caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

map_le_sex <- map_le_f + map_le_m

ggsave("fig/maps-life-expectancy-sex.pdf", map_le_sex,
       width = 12, height = 6, device = cairo_pdf)



# correlate e0 and q5065 --------------------------------------------------

load("dat/df_dk_mun.rda")

# diagonal plot
df_dk_mun %>%
    filter(sex == "b", period == "2015-2019") %>%
    ggplot(aes(q5065, life_exp))+
    geom_point(aes(size = e), shape = 1, color = "#4a4a4a")+
    geom_smooth(method = "lm", se = F,
                mapping = aes(weight = e),
                color = 1, size = 3/4)+
    scale_size_area(breaks = c(25, 50, 100, 250, 500))+
    # facet_row(~period)+
    theme_minimal(base_family = font_rc)+
    theme(
        legend.position = c(.95,.3),
        legend.justification = c(1,0),
        plot.background = element_rect(fill = "#dadada", color = NA)
    )+
    labs(y = "TOPALS fitted life expectancy",
         x = "Probability of dying between 50 and 65",
         size = "Populaion,\nthous.")

gg_diag_e0_q5065 <- last_plot()

ggsave("fig/life-exp-b-e0-q5065.pdf", gg_diag_e0_q5065,
       width = 5, height = 3, device = cairo_pdf)
