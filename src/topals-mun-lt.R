#===============================================================================
# 2020-04-20 -- rockwool
# Calculate Danish municipal life tables useing TOPALS
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

library(DemoTools)

source("src/fun-inset-cph-box.R")

source("src/fun-topals-fit.R")
source("src/fun-show-topals.R")


# trapez approx of life expectancy from a logmx schedule over ages 0..99
e0 = function(logmx) {
    mx = exp(logmx)
    px = exp(-mx)
    lx = c(1,cumprod(px))
    return( sum(head(lx,-1) + tail(lx,-1)) / 2)
}

# load data
load("dat/pooled_death_rates.rda")
load("dat/pooled_std.rda")

# # join standard to data
# df <- death_rates %>%
#     left_join(logmx_std)


# calculate TOPALS life tables for pooled 5y data, by sex -----------------

load("dat/pooled_5y.rda")

df <- pooled_5y %>%
    mutate(age = age %>% as.integer()) %>%
    left_join(
        std_5y %>% transmute(period, sex, age, logmx_smth)
    )

# # these are tiny island communes
# id_tiny <- c("411", "492", "563", "741", "825")

# fit TOPALS to all municipalities
df_fit <- df %>%
    filter(!id %in% c("411")) %>%
    group_by(period, sex, id, name) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = exposure, D = death, std = logmx_smth,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm"),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()

save(df_fit, file = "dat/topals-pooled-5y.rda")

# calculate life expectancy
df_le <- df_fit %>%
    group_by(period, sex, id, name) %>%
    summarise(life_exp = logmx_fit %>% e0) %>%
    ungroup()

# attach population size
load("dat/death_rates.rda")
pop_size_19 <- death_rates %>%
    filter(year == "2019") %>%
    group_by(name) %>%
    summarise(e = exposure %>% sum %>% divide_by(1e3) %>% round(2))

save(pop_size_19, file = "dat/pop_size_19.rda")

# join population size
df_le_comapre <- df_le %>%
    left_join(pop_size_19)

save(df_le_comapre, file = "dat/df_le_comapre.rda")





# pooled data 2015-2019, both sex -- compare to DST -----------------------

load("dat/pooled_1519_both.rda")

df <- pooled_1519_both %>%
    mutate(age = age %>% as.integer()) %>%
    left_join(std_1519_both %>% transmute(age, logmx_smth))

# fit TOPALS to all municipalities
both_fit <- df %>%
    filter(!id %in% c("411")) %>%
    group_by(id, name) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = exposure, D = death, std = logmx_smth,
            age_group_bounds = 0:100, max_iter = 1e3, details = T
        ) %>% extract2("logm"),
        mx_fit = logmx_fit %>% exp
    )

save(both_fit, file = "dat/topals-both-1519.rda")

# calculate life expectancy
both_le <- both_fit %>% summarise(life_exp = logmx_fit %>% e0)

# compare to DST calculations
dst_le <- readxl::read_excel("dat/dst-e0/HISBK-e0-mun.xlsx", skip = 3) %>%
    set_colnames(c("id", "name", 2004:2019))

# # join and compare
# both_comapre <- both_le %>%
#     left_join(dst_le %>% transmute(name, dst_le = `2019` %>% as.numeric))

# attach population size
load("dat/death_rates.rda")
pop_size_19 <- death_rates %>%
    filter(year == "2019") %>%
    group_by(name) %>%
    summarise(e = exposure %>% sum %>% divide_by(1e3) %>% round(2))

# join population size
both_comapre <- both_le %>%
    left_join(dst_le %>% transmute(name, dst_le = `2019` %>% as.numeric)) %>%
    left_join(pop_size_19)

# diagonal plot
both_comapre %>%
    ggplot(aes(dst_le, life_exp, size = e))+
    geom_abline(slope = 1)+
    geom_point(shape = 1)+
    scale_size_area(breaks = c(25, 50, 100, 250, 500))+
    dark_theme_minimal(base_family = font_rc)+
    theme(legend.position = c(.95,.05),
          legend.justification = c(1,0))+
    labs(y = "TOPALS fited life expectancy",
         x = "DST estimated life expectancy",
         size = "Populaion,\nthous.")

gg_compare_diag <- last_plot()

# DST vs difference
both_comapre %>%
    ggplot(aes(dst_le, (life_exp - dst_le), size = e))+
    geom_hline(yintercept = 0)+
    geom_point(shape = 1)+
    scale_size_area(breaks = c(25, 50, 100, 250, 500), guide = FALSE)+
    dark_theme_minimal(base_family = font_rc)+
    labs(y = "TOPALS minus DST",
         x = "DST estimated life expectancy",
         size = "Populaion,\nthous.")

gg_compare_diff <- last_plot()

gg_compare <- (gg_compare_diag + gg_compare_diff) +
    plot_annotation(title = "Compare life expectancy",
                    subtitle = "TOPALS vs DST, Danish municipalities",
                    theme = dark_theme_minimal(base_family = font_rc))

ggsave(filename = "fig/compare-life-exp-fit.pdf", gg_compare,
       width = 7, height = 4)

# map the results
load("dat/geodata-dk.rda")

both_map <- both_comapre %>%
    left_join(gd_dk_mun_s, by = "id") %>%
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


# proportion of non-survival 50-65 ----------------------------------------

both_q5065 <- both_fit %>%
    group_by(id, name) %>%
    mutate(lx = mx_fit %>% lt_single_mx() %>% pull(lx)) %>%
    filter(age %in% c(50, 65)) %>%
    summarise(q5065 = 1 - (lx[2] / lx[1]))

dm_q5065 <- both_q5065 %>%
    left_join(pop_size_19) %>%
    left_join(gd_dk_mun_s, by = "id") %>%
    st_as_sf() %>%
    st_transform(crs = 3044)

# map prob of dying between 50 and 65
dm_q5065 %>%
    ggplot()+
    geom_sf(aes(fill = q5065, geometry = geometry), color = NA)+
    geom_sf(data = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(palette = "RdPu", direction = 1)+
    cowplot::theme_map(font_family = font_rc, font_size = 16)+
    theme(legend.position = c(1,  .9),
          legend.justification = c(1, 1),
          legend.background = element_blank(),
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Probability of dying between 50 and 65",
         subtitle = "TOPALS model, pooled data 2015-19",
         caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

map_q5065 <- last_plot()

map_q5065_zoom <- map_q5065 %>% inset_cph_box()

maps_topals <- map_le_zoom + map_q5065_zoom

ggsave("fig/maps-le-q5065.pdf", maps_topals,
       width = 12, height = 6)


# divergent color scales -- relative diff ---------------------------------
#
# # Danish pooled values
# dk_e0 <- std_1519_both %>%
#     pull(mx_smth) %>%
#     lt_single_mx() %>%
#     pull(ex) %>%
#     extract(1)
#
# df_q5065 <- std_1519_both %>%
#     pull(mx_smth) %>%
#     lt_single_mx() %>%
#     filter(Age %in% c(50, 65)) %>%
#     summarise(q5065 = 1 - (lx[2] / lx[1]))
#
#
# # function to mutate to relative groups
# relative_categories <- function(continious_variable, center) {
#     relative <- continious_variable %>% divide_by(center)
#     categories <- relative %>% cut(c(.5, 2/3, 4/5, .95, 100/95, 5/4, 3/2, Inf))
#     n_per_age_group <- categories %>% table() %>% paste
#     new_levels <- paste0(
#         c(
#             "Below 67%", "From 67% to 80%",
#             "From 80% to 95%", "Danish average ± 5%", "From 105% to 125%",
#             "From 125% to 150%", "Above 150%"
#         ),
#         " (",
#         n_per_age_group,
#         ")"
#     )
#     categories <- categories %>% lvls_revalue(new_levels)
# }
#

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
    theme(legend.position = c(1,  .9),
          legend.justification = c(1, 1),
          legend.background = element_blank(),
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Life expectancy at birth",
         subtitle = "TOPALS model, pooled data 2015-19\nNational life expectancy at birth is 81 years",
         caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

div_le <- last_plot()

div_le_zoom <- div_le %>% inset_cph_box()

# map prob of dying between 50 and 65
dm_q5065 %>%
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
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Probability of dying between 50 and 65",
         subtitle = "TOPALS model, pooled data 2015-19\nNational probability of dying between 50 and 65 is 8.5%",
         caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

div_q5065 <- last_plot()

div_q5065_zoom <- div_q5065 %>% inset_cph_box()

maps_topals_div <- div_le_zoom + div_q5065_zoom

ggsave("fig/maps-le-q5065-div.pdf", maps_topals_div,
       width = 12, height = 6)


# understand TOPALS -------------------------------------------------------

foo <- df %>%  filter(name == "Odense")

too <- TOPALS_fit(N = foo$exposure, D = foo$death, std = foo$logmx_smth,
           age_group_bounds = 0:100, max_iter = 1000, details = T)

too %>% show_topals(hue = "#339999")

# a hardcoded function to show how TOPALS works
display_topals <- function(mun = "Odense", hue = "#33BABA") {

    foo <- df %>%  filter(name == mun)

    too <- TOPALS_fit(N = foo$exposure, D = foo$death, std = foo$logmx_smth,
                      age_group_bounds = 0:100, max_iter = 1000, details = T)

    out <- too %>% show_topals(hue = hue)

    out <- out + dark_theme_minimal(base_family = font_rc) +
        annotate("text", x = 5, y = -1, label = mun, size = 10,
                 fontface = 2, family = font_rc, color = hue,
                 alpha = .5, hjust = 0)

    return(out)
}

display_topals("Odense", hue = "#df356b")

gg_odense <- last_plot()

display_topals("Kerteminde", hue = "#eec21f")

gg_kerteminde <- last_plot()

illustrate_topals <- gg_odense / gg_kerteminde

ggsave("fig/illustrate-topals-odense-kerteminde.pdf", illustrate_topals,
       width = 5, height = 8)


# Copenhagen
display_topals("Copenhagen", hue = "cyan")
gg_copenhagen <- last_plot()

ggsave("fig/illustrate-topals-copenhagen.pdf", gg_copenhagen,
       width = 6, height = 5)

# Lolland
display_topals("Lolland", hue = "cyan")


death_rates %>%
    group_by(name, sex, age) %>%
    summarise(avg = death %>% sum) %>%
    ungroup() %>%
    filter(age %>% is_in(0:5)) %>%
    pivot_wider(names_from = age, values_from = avg) %>%
    View

# too$logm %>% plot
# lines(foo$logmx_smth)

too$logm %>% as.vector() %>% e0
too$logm %>% as.vector() %>% exp %>% lt_single_mx() %>% pull(ex) %>% extract(1)
too$std %>% e0


library(microbenchmark)

microbenchmark(
    simple = too$logm %>% as.vector() %>% e0,
    DemoTools = too$logm %>% as.vector() %>% exp %>% lt_single_mx() %>% pull(ex) %>% extract(1)
)
