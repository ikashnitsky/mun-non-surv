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


# # trapez approx of life expectancy from a logmx schedule over ages 0..99
# e0 = function(logmx) {
#     mx = exp(logmx)
#     px = exp(-mx)
#     lx = c(1,cumprod(px))
#     return( sum(head(lx,-1) + tail(lx,-1)) / 2)
# }

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

load("dat/topals-pooled-5y.rda")


# calculate life tables ---------------------------------------------------

df_mun_lt <- df_fit %>%
    group_by(period, sex, id, name) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()

save(df_mun_lt, file = "dat/mun-lt-5y.rda")



# extract  life expectancy
df_le <- df_mun_lt %>% filter(age == 0) %>%
    transmute(period, sex, id, name, life_exp = ex)

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


# proportion of non-surv 50-65
df_q5065 <- df_mun_lt %>%
    filter(age %in% c(50, 65)) %>%
    group_by(period, sex, id, name) %>%
    summarise(q5065 = 1 - (lx[2] / lx[1])) %>%
    ungroup()

# join non-surv 50-65
df_dk_mun <- df_le %>%
    left_join(df_q5065) %>%
    left_join(pop_size_19)

save(df_dk_mun, file = "dat/df_dk_mun.rda")



# pooled data 2015-2019, both sex -- compare to DST -----------------------

# load("dat/pooled_1519_both.rda")
#
# df <- pooled_1519_both %>%
#     mutate(age = age %>% as.integer()) %>%
#     left_join(std_1519_both %>% transmute(age, logmx_smth))
#
# # fit TOPALS to all municipalities
# both_fit <- df %>%
#     filter(!id %in% c("411")) %>%
#     group_by(id, name) %>%
#     mutate(
#         logmx_fit = TOPALS_fit(
#             N = exposure, D = death, std = logmx_smth,
#             age_group_bounds = 0:100, max_iter = 1e3, details = T
#         ) %>% extract2("logm"),
#         mx_fit = logmx_fit %>% exp
#     )
#
# save(both_fit, file = "dat/topals-both-1519.rda")
#
# load("dat/topals-both-1519.rda")
#
# # calculate life expectancy
# both_le <- both_fit %>% summarise(life_exp = mx_fit %>% e0)

# compare to DST calculations
dst_le <- readxl::read_excel("dat/dst-e0/HISBK-e0-mun.xlsx", skip = 3) %>%
    set_colnames(c("id", "name", 2004:2019))

#
# # attach population size
# load("dat/death_rates.rda")
# pop_size_19 <- death_rates %>%
#     filter(year == "2019") %>%
#     group_by(name) %>%
#     summarise(e = exposure %>% sum %>% divide_by(1e3) %>% round(2))

# join population size
both_comapre <- df_dk_mun %>%
    filter(sex == "b", period == "2015-2019") %>%
    left_join(dst_le %>% transmute(name, dst_le = `2019` %>% as.numeric)) %>%
    left_join(pop_size_19)

# save the joined dataset to compare TOPALS to DST
save(both_comapre, file = "dat/comapre-topals-dst-b.rda")

# # proportion of non-survival 50-65 ----------------------------------------
#
# both_q5065 <- df_mun_lt %>%
#     filter(sex == "b", period == "2015-2019") %>%
#     filter(age %in% c(50, 65)) %>%
#     group_by(id, name) %>%
#     summarise(q5065 = 1 - (lx[2] / lx[1])) %>%
#     ungroup()
#
# dm_q5065 <- both_q5065 %>%
#     left_join(pop_size_19) %>%
#     left_join(gd_dk_mun_s, by = "id") %>%
#     st_as_sf() %>%
#     st_transform(crs = 3044)

# # map prob of dying between 50 and 65
# dm_q5065 %>%
#     ggplot()+
#     geom_sf(aes(fill = q5065, geometry = geometry), color = NA)+
#     geom_sf(data = dk_cities,
#             size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
#     scale_fill_fermenter(palette = "RdPu", direction = 1)+
#     cowplot::theme_map(font_family = font_rc, font_size = 16)+
#     theme(legend.position = c(1,  .9),
#           legend.justification = c(1, 1),
#           legend.background = element_blank(),
#           plot.title = element_text(family = "Roboto Slab",
#                                     face = 2, size = 20),
#           panel.spacing = unit(0, "lines"))+
#     labs(title = "Probability of dying between 50 and 65",
#          subtitle = "TOPALS model, pooled data 2015-19",
#          caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
#          fill = NULL)
#
# map_q5065 <- last_plot()
#
# map_q5065_zoom <- map_q5065 %>% inset_cph_box()
#
# maps_topals <- map_le_zoom + map_q5065_zoom
#
# ggsave("fig/maps-le-q5065.pdf", maps_topals,
#        width = 12, height = 6)


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
