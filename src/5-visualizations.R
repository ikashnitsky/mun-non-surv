#===============================================================================
# 2025-04-24 -- mun-non-surv
# dataviz
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


source("src/0-prepare-session.R")



## f1 -- dk and se, survival of males 50-65, 2015-19 ---------------------------------------------------------

load("dat/std_dkse_5y_lt.rda")

std_dkse_5y_lt %>%
    filter(sex == "m", period == "2015-19") %>%
    ggplot(aes(age, lx_1e5, color = country))+
    geom_path(size = 1)+
    scale_color_manual(values = pal_dkse, guide = NULL)+
    scale_y_comma(limits = c(0, 1e5))

gg_bare <- last_plot()

(
    gg_labs <- gg_bare +
        annotate("text", label = c("Denmark", "Sweden"),
                 x = 65, y = c(75e3, 95e3), hjust = 1:0,
                 size = 5, color = pal_dkse, family = font_rc)+
        labs(title = "Survival of males, 2015-19",
             y = "Period life table survival, lx",
             x = "Age")
)

(
    out <-  ggdraw(gg_labs)+
        draw_plot(
            gg_bare +
                coord_cartesian(xlim = c(50, 65), ylim = c(85e3, 100e3))+
                geom_hline(
                    data = . %>% filter(age %in% c(50, 65)),
                    aes(yintercept = lx_1e5, color = country),
                    alpha = .5, size = .5
                )+
                geom_text(data = . %>% filter(age==65),
                          aes(label = lx_1e5 %>% round(-2) %>%
                                  divide_by(1e3) %>% paste0("k"),
                              y = lx_1e5 + 500, color = country),
                          x = 52.5, hjust = .5, vjust = 0,
                          size = 5, family = "ah")+
                geom_text(data = . %>% filter(age==50),
                          aes(label = lx_1e5 %>% round(-2) %>%
                                  divide_by(1e3) %>% paste0("k"),
                              y = lx_1e5, color = country),
                          x = 62.5, hjust = .5, vjust = c(1.2, -.2),
                          size = 5, family = "ah")+
                scale_y_comma(breaks = seq(85e3, 100e3, 5e3), position = "right")+
                labs(x = NULL, y = NULL)+
                theme(
                    plot.background = element_rect(color = "#7a7a7a")
                ),
            x = .2, y = .15, width = .5, height = .45
        )
)

ggsave("fig/fig1-surv-dk-se-males-ann.pdf", out, width = 7, height = 5, device = cairo_pdf)




## f2 -- illustrate TOPALS ------------------------------------------------------------

# Copenhagen, Odense, and Kerteminde

load("dat/dk_mun_pooled_topals.rda")



display_topals("Odense", hue = "#8c0032")

gg_odense <- last_plot()

display_topals("Kerteminde", hue = "#7c8500")

gg_kerteminde <- last_plot()

# Copenhagen
display_topals("Copenhagen", hue = "#005b9f")
gg_copenhagen <- last_plot()


illustrate_topals <- gg_copenhagen + (gg_odense / gg_kerteminde) +
    plot_annotation(tag_levels = "A") + plot_layout(widths = c(2,1))

ggsave("fig/fig2-illustrate-topals.pdf", illustrate_topals,
       width = 10, height = 6, device = cairo_pdf)



## f3 -- maps --------------------------------------------------------------

load("dat/geodata-dk.rda")
load("dat/dk_mun_sum_ci.rda")

dk_map <- dk_mun_sum_ci %>%
    left_join(gd_dk_mun_s %>% select(-name), by = "id") %>%
    st_as_sf() %>%
    st_transform(crs = 3044)



# DK mun -- life exp via TOPALS
dk_map %>%
    filter(sex == "b", period == "2015-19") %>%
    ggplot()+
    geom_sf(aes(fill = le_mean, geometry = geometry), color = NA)+
    geom_sf(data = dk_cities,
            size = 4.5, shape = 1, stroke = 1, color = "#da26fa")+
    scale_fill_fermenter(palette = "RdBu", direction = 1,
                         breaks = seq(79.5, 82.5, 1))+
    cowplot::theme_map(font_family = "ah", font_size = 14)+
    theme(
        legend.position = c(1,  .9),
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        plot.background = element_rect(fill = "#eeffff", color = NA),
        plot.title = element_text(face = 2, size = 20),
        panel.spacing = unit(0, "lines")
    )+
    labs(title = "Life expectancy at birth",
         subtitle = "TOPALS model, pooled data 2015-19\nNational life expectancy at birth is 81 years",
         # caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

div_le <- last_plot()

div_le_zoom <- div_le %>% inset_cph_box(y = .44)

# map prob of dying between 50 and 65
dk_map %>%
    filter(sex == "b", period == "2015-19") %>%
    ggplot()+
    geom_sf(aes(fill = q5065_mean * 100, geometry = geometry), color = NA)+
    geom_sf(data = dk_cities,
            size = 4.5, shape = 1, stroke = 1, color = "#da26fa")+
    scale_fill_fermenter(palette = "BrBG",
                         breaks = seq(6, 11, 1))+
    cowplot::theme_map(font_family = "ah", font_size = 14)+
    theme(legend.position = c(1,  .9),
          legend.justification = c(1, 1),
          legend.background = element_blank(),
          plot.background = element_rect(fill = "#eeffff", color = NA),
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Probability of dying between 50 and 65",
         subtitle = "TOPALS model, pooled data 2015-19\nNational probability of dying between 50 and 65 is 8.5%",
         # caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

div_q5065 <- last_plot()

div_q5065_zoom <- div_q5065 %>% inset_cph_box(y = .44)

maps_topals_div <- div_le_zoom + div_q5065_zoom + plot_annotation(tag_levels = "A")

ggsave("fig/f3-maps-le-q5065-div.pdf", maps_topals_div,
       width = 12, height = 6, device = cairo_pdf)



## f4 -- life expectancy ECD for Denmark and Sweden ------------------------------

load("dat/dk_mun_sum_ci.rda")
load("dat/dk_mun_size.rda")

dk_mun_sum_ci %>%
    left_join(dk_mun_size) %>%
    filter(!sex=="b") %>%
    ungroup() %>%
    group_by(sex, period) %>%
    arrange(le_mean) %>%
    mutate(
        prop_e = pop_size %>% prop.table() %>% cumsum()
    ) %>%
    ungroup() %>%
    ggplot()+
    geom_hline(yintercept = .5, size = .75, color = "#9a9a9a")+
    geom_step(data = . %>% filter(period == "2010-14"),
              aes(le_mean, prop_e, color = sex),
              size = 2/3)+
    geom_step(data = . %>% filter(period == "2015-19"),
              aes(le_mean, prop_e, color = sex),
              size = 3/2, alpha = .5)+
    geom_flag(
        data = tibble(country = "dk"),
        aes(country = country),
        x = 86, y = .125, size = 15
    )+
    scale_color_manual(values = pal)+
    scale_y_percent(position = "right", expand = c(.01, .01))+
    scale_x_continuous(
        limits = c(74, 87), breaks = seq(75, 85, 2.5), expand = c(0,0)
    )+
    theme(
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")
    )+
    labs(x = "Life expectancy",
         y = "Cumulative proprotion of the population",
         size = "Populaion,\nthous.")

f4_dk <- last_plot()


f4_legend <- ggplot()+
    coord_equal(xlim = c(0,1.3), ylim = c(0,.8))+
    annotate(
        'rect', xmin = .55, xmax = .7, ymin = .1, ymax = .25,
        fill = pal[2], color = NA
    )+
    annotate(
        'rect', xmin = .55, xmax = .7, ymin = .4, ymax = .55,
        fill = pal[1], color = NA
    )+
    annotate(
        'rect', xmin = .8, xmax = 1.05, ymin = .05, ymax = .3,
        fill = pal[2], color = NA, alpha = .5
    )+
    annotate(
        'rect', xmin = .8, xmax = 1.05, ymin = .35, ymax = .6,
        fill = pal[1], color = NA, alpha = .5
    )+
    annotate(
        'text', x = .47, y = c(.175, .475), vjust = .5, hjust = 1,
        size = 4, fontface = 1, label = c('Males', 'Females'), family = "ah", color = pal %>% rev
    )+
    annotate(
        'text', x = c(.7, 1.05), y = .65, vjust = 0, hjust = c(1, .5),
        size = 4, fontface = c(1, 2), alpha = c(1, .5), color = "#444444", label = c('2010-14', '2015-19'), family = "ah"
    )+
    theme_void()


(
    f4_out_dk <- ggdraw() +
        draw_plot(f4_dk)+
        draw_plot(f4_legend, x = 0, y = .73, width = .35, height = .25)
)

# same for Sweden

load("dat/se_mun_sum_ci.rda")
load("dat/se_mun_size.rda")

se_mun_sum_ci %>%
    left_join(se_mun_size) %>%
    filter(!sex=="b") %>%
    ungroup() %>%
    group_by(sex, period) %>%
    arrange(le_mean) %>%
    mutate(
        prop_e = pop_size %>% prop.table() %>% cumsum()
    ) %>%
    ungroup() %>%
    ggplot()+
    geom_hline(yintercept = .5, size = .75, color = "#9a9a9a")+
    geom_step(data = . %>% filter(period == "2010-14"),
              aes(le_mean, prop_e, color = sex),
              size = 2/3)+
    geom_step(data = . %>% filter(period == "2015-19"),
              aes(le_mean, prop_e, color = sex),
              size = 3/2, alpha = .5)+
    geom_flag(
        data = tibble(country = "se"),
        aes(country = country),
        x = 86, y = .125, size = 15
    )+
    scale_color_manual(values = pal)+
    scale_y_percent(position = "right", expand = c(.01, .01))+
    scale_x_continuous(
        limits = c(74, 87), breaks = seq(75, 85, 2.5), expand = c(0,0)
    )+
    theme(
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")
    )+
    labs(x = "Life expectancy",
         y = "Cumulative proprotion of the population",
         size = "Populaion,\nthous.")

f4_se <- last_plot()


(
    f4_out_se <- ggdraw() +
        draw_plot(f4_se)+
        draw_plot(f4_legend, x = 0, y = .73, width = .35, height = .25)
)


# join together with patchwork
f4_out <- f4_out_dk / f4_out_se + plot_annotation(tag_levels = "A")

ggsave(
    filename = "fig/fig4-cum-dk-and-se.pdf", plot = f4_out,
    width = 7, height = 8, device = cairo_pdf
)



## a5 -- mun life exp curves with credible intervals ---------------------

df_a5_dk <- dk_mun_sum_ci %>%
    left_join(dk_mun_size) %>%
    filter(!sex=="b") %>%
    ungroup() %>%
    group_by(sex, period) %>%
    arrange(le_mean) %>%
    mutate(
        y_end = pop_size %>% prop.table() %>% cumsum(),
        y_start = y_end %>% lag() %>% replace_na(0)
    ) %>%
    ungroup() %>%
    arrange(period, sex, le_mean)


df_a5_dk %>%
    ggplot()+
    geom_hline(yintercept = .5, size = .75, color = "#9a9a9a")+
    geom_rect(
        data = . %>% filter(period == "2010-14"),
        aes(xmin = le_100, xmax = le_900, ymin = y_start, ymax = y_end, fill = sex),
        alpha = 2/3, color = NA
    )+
    geom_segment(
        data = . %>% filter(period == "2010-14"),
        aes(le_mean, y_start, yend = y_end, color = sex),
        size = 1/3
    )+
    geom_rect(
        data = . %>% filter(period == "2015-19"),
        aes(xmin = le_100, xmax = le_900, ymin = y_start, ymax = y_end, fill = sex),
        alpha = 1/3, color = NA
    )+
    geom_segment(
        data = . %>% filter(period == "2015-19"),
        aes(le_mean, y_start, yend = y_end, color = sex),
        size = 2/3, alpha = 2/3
    )+
    geom_flag(
        data = tibble(country = "dk"),
        aes(country = country),
        x = 86, y = .125, size = 15
    )+
    scale_fill_manual(values = pal)+
    scale_color_manual(values = pal)+
    scale_y_percent(position = "right", expand = c(.01, .01))+
    scale_x_continuous(
        limits = c(74, 87), breaks = seq(75, 85, 2.5), expand = c(0,0)
    )+
    theme(
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")
    )+
    labs(x = "Life expectancy",
         y = "Cumulative proprotion of the population",
         size = "Populaion,\nthous.")

a5_dk <- last_plot()


(
    a5_dk_legend <- ggdraw() +
        draw_plot(a5_dk)+
        draw_plot(f4_legend, x = 0, y = .73, width = .35, height = .25)
)


df_a5_se <- se_mun_sum_ci %>%
    left_join(se_mun_size) %>%
    filter(!sex=="b") %>%
    ungroup() %>%
    group_by(sex, period) %>%
    arrange(le_mean) %>%
    mutate(
        y_end = pop_size %>% prop.table() %>% cumsum(),
        y_start = y_end %>% lag() %>% replace_na(0)
    ) %>%
    ungroup() %>%
    arrange(period, sex, le_mean)


df_a5_se %>%
    ggplot()+
    geom_hline(yintercept = .5, size = .75, color = "#9a9a9a")+
    geom_rect(
        data = . %>% filter(period == "2010-14"),
        aes(xmin = le_100, xmax = le_900, ymin = y_start, ymax = y_end, fill = sex),
        alpha = 2/3, color = NA
    )+
    geom_segment(
        data = . %>% filter(period == "2010-14"),
        aes(le_mean, y_start, yend = y_end, color = sex),
        size = 1/3
    )+
    geom_rect(
        data = . %>% filter(period == "2015-19"),
        aes(xmin = le_100, xmax = le_900, ymin = y_start, ymax = y_end, fill = sex),
        alpha = 1/3, color = NA
    )+
    geom_segment(
        data = . %>% filter(period == "2015-19"),
        aes(le_mean, y_start, yend = y_end, color = sex),
        size = 2/3, alpha = 2/3
    )+
    geom_flag(
        data = tibble(country = "se"),
        aes(country = country),
        x = 86, y = .125, size = 15
    )+
    scale_fill_manual(values = pal)+
    scale_color_manual(values = pal)+
    scale_y_percent(position = "right", expand = c(.01, .01))+
    scale_x_continuous(
        limits = c(74, 87), breaks = seq(75, 85, 2.5), expand = c(0,0)
    )+
    theme(
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")
    )+
    labs(x = "Life expectancy",
         y = "Cumulative proprotion of the population",
         size = "Populaion,\nthous.")

a5_se <- last_plot()


(
    a5_se_legend <- ggdraw() +
        draw_plot(a5_se)+
        draw_plot(f4_legend, x = 0, y = .73, width = .35, height = .25)
)

# join together with patchwork
a5 <- a5_dk_legend / a5_se_legend + plot_annotation(tag_levels = "A")

ggsave(
    filename = "fig/a5-cum-dk-and-se-ci.pdf", plot = a5,
    width = 7, height = 8, device = cairo_pdf
)



## a1 -- cumulative pop sizes DK and SE ------------------------------------

load("dat/dk_mun_size.rda")
load("dat/se_mun_size.rda")

dk_mun_size %>%
    bind_rows(se_mun_size, .id = "country") %>%
    filter(sex=="b", period == "2015-19", !id == "411") %>%
    group_by(country) %>%
    arrange(pop_size) %>%
    mutate(
        pop_size = pop_size %>% divide_by(1e3),
        y_end = pop_size %>% prop.table() %>% cumsum(),
        y_start = y_end %>% lag() %>% replace_na(0)
    ) %>%
    ungroup() %>%
    ggplot()+
    geom_hline(yintercept = .5, size = .5, color = "#9a9a9a")+
    geom_segment(
        aes(pop_size, y_start, yend = y_end, color = country),
        size = 1
    )+
    scale_x_continuous(trans = "log", breaks = c(5, 10, 25, 50, 100, 250, 500), position = "top")+
    scale_y_percent()+
    scale_color_manual(values = pal_dkse, guide = "none")+
    labs(
        x = "Municipality population size, thous.",
        y = "Cumulative country's population"
    )+
    geom_flag(
        data = tibble(country = "dk"),
        aes(country = country),
        x = 86 %>% log, y = .125, size = 15
    )+
    geom_flag(
        data = tibble(country = "se"),
        aes(country = country),
        x = 10 %>% log, y = .25, size = 15
    )+
    geom_text(
        data = . %>% filter(country == 1, pop_size > 100),
        aes(label = name, color = country, x = pop_size,
            y = (y_start + y_end)/2, size = pop_size),
        hjust = 1, nudge_x = -.1
    )+
    geom_text(
        data = . %>% filter(country == 2, pop_size > 200),
        aes(label = name, color = country, x = pop_size,
            y = (y_start + y_end)/2, size = pop_size),
        hjust = 0, nudge_x = .07
    )+
    scale_size_area(guide = NULL)

ggsave(filename = "fig/a1-cum-mun-pop-size-dkse.pdf",
       width = 7, height = 4, device = cairo_pdf)



## a4 -- le vs q5065 -----------------------------------------------------

load("dat/dk_mun_sum_ci.rda")
load("dat/dk_mun_size.rda")

dk_mun_sum_ci %>%
    left_join(dk_mun_size) %>%
    filter(sex == "b", period == "2015-19") %>%
    ggplot(aes(q5065_mean, le_mean))+
    geom_point(aes(size = pop_size), shape = 1, stroke = 1, color = "#004a4a")+
    geom_smooth(method = "lm", se = F,
                mapping = aes(weight = pop_size),
                color = "#26dafa", size = 3/4)+
    scale_size_area(breaks = c(25, 50, 100, 250, 500))+
    theme(
        legend.position = c(.95,.3),
        legend.justification = c(1,0)
    )+
    labs(y = "TOPALS fitted life expectancy",
         x = "Probability of dying between 50 and 65",
         size = "Populaion,\nthous.")

a4_diag_e0_q5065 <- last_plot()

ggsave("fig/a4-life-exp-b-e0-q5065.pdf", a4_diag_e0_q5065,
       width = 5, height = 3, device = cairo_pdf)




## f5 -- sex diff life exp vs male life exp --------------------------------


load("dat/dk_mun_sum_ci.rda")
load("dat/dk_mun_size.rda")

dk_mun_sum_ci %>%
    filter(!sex=="b") %>%
    select(period, sex, id, name, le_mean) %>%
    pivot_wider(names_from = sex, values_from = le_mean) %>%
    left_join(dk_mun_size %>% filter(sex=="b")) %>%
    ggplot(aes(m, m-f))+
    geom_point(shape = 1, aes(size = pop_size), color = "#004a4a", stroke = 1)+
    geom_smooth(method = "lm", se = F, mapping = aes(weight = pop_size), color = "#26dafa", size = 3/4)+
    scale_size_area(breaks = c(25, 50, 100, 250, 500))+
    facet_row(~period)+
    theme(
        legend.position = c(.55,.02),
        legend.justification = c(1,0)
    )+
    labs(y = "Sex gap in life expectancy",
         x = "Male life expectancy",
         size = "Populaion,\nthous.")

f5_diag_le_sex <- last_plot()

ggsave(filename = "fig/f5-life-exp-sex-gap.pdf", f5_diag_le_sex, width = 7, height = 4, device = cairo_pdf)


## a2 -- TOPALS vs DST -----------------------------------------------------

# compare to DST calculations
dst_le <- readxl::read_excel("dat/dst-e0/HISBK-e0-mun.xlsx", skip = 3) %>%
    set_colnames(c("id", "name", 2004:2019))

load("dat/dk_mun_sum_ci.rda")
load("dat/dk_mun_size.rda")

df_a2 <- dk_mun_sum_ci %>%
    left_join(dk_mun_size) %>%
    filter(sex=="b", period == "2015-19") %>%
    ungroup() %>%
    left_join(dst_le %>% transmute(name, dst_le = `2019` %>% as.numeric)) %>%
    drop_na(dst_le)


# diagonal plot
df_a2 %>%
    ggplot(aes(dst_le, le_mean, size = pop_size))+
    geom_abline(slope = 1, color = "#9a9a9a")+
    geom_point(
        shape = 1, color = "#004a4a")+
    scale_size_area(breaks = c(25, 50, 100, 250, 500), max_size = 10, guide = guide_legend() )+
    theme(
        legend.position.inside = c(.9,.5),
        legend.justification = c(1,0)
    )+
    labs(y = "TOPALS fitted life expectancy",
         x = "DST estimated life expectancy",
         size = "Populaion,\nthous.")

a2_compare_diag <- last_plot()

# DST vs difference
df_a2 %>%
    ggplot(aes(dst_le, (le_mean - dst_le), size = pop_size))+
    geom_hline(yintercept = 0, color = "#9a9a9a")+
    geom_point(shape = 1, color = "#004a4a")+
    scale_size_area(breaks = c(25, 50, 100, 250, 500), , max_size = 10, guide = NULL)+
    labs(y = "TOPALS minus DST",
         x = "DST estimated life expectancy",
         size = "Populaion,\nthous.")

a2_compare_diff <- last_plot()

a2_compare <- (a2_compare_diag + a2_compare_diff)

ggsave(filename = "fig/a2-compare-topals-dst.pdf", a2_compare,
       width = 7, height = 4, device = cairo_pdf)


## f6 -- clusters map ------------------------------------------------------

load("dat/geodata-dk.rda")
load("dat/clusters_dk_m2.rda")

# descriptive summary stat of clusters
tab <- dk_m2 %>%
    st_drop_geometry() %>%
    left_join(clusters_dk_m2) %>%
    group_by(cluster) %>%
    summarise(
        w_mean = matrixStats::weightedMean(le_mean, pop_size),
        w_sd = matrixStats::weightedSd(le_mean, pop_size),
        n = n(),
        e = sum(pop_size),
        mean = mean(le_mean),
        sd = sd(le_mean)
    )

# assemble the summary table as legend

(
    f6_legend_table <- ggplot(data = tibble(1:6), aes(y = seq(11,1,-2)))+
        coord_cartesian(xlim = c(0,15), ylim = c(0,14))+
        theme_void()+
        geom_point(
            shape = 15, x = 1.5, size = 7,
            color = viridis::viridis(6, option = "H", begin = .1, end = .9)
        )+
        geom_text(x = 3, label = tab$cluster, hjust = 1, family = "ah")+
        geom_text(x = 5, label = tab$n, hjust = 1, family = "ah")+
        geom_text(x = 8, label = tab$e %>% round, hjust = 1, family = "ah")+
        geom_text(x = 11, label = tab$w_mean %>% round(1), hjust = 1, family = "ah")+
        geom_text(x = 14, label = tab$w_sd %>% round(2), hjust = 1, family = "ah")+
        annotate("text",
                 x = c(3, 5, 8, 11, 14), y = 13.4, hjust = 1,
                 label = c("Cluster", "N", "Pop", "e0", "SD"),
                 fontface = 2
        )
)

# the map without CPH inset
f6_bare <- dk_m2 %>%
    left_join(clusters_dk_m2) %>%
    ggplot()+
    geom_sf(aes(fill = cluster), color = NA)+
    geom_sf(
        data = dk_cities,
        size = 4.5, shape = 1, stroke = 1, color = "#da26fa"
    )+
    scale_fill_viridis_d(option = "H", begin = .1, end = .9)+
    theme_map()+
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#eeffff", color = NA)
    )+
    # add cluster numbers on map
    geom_sf_label(
        data = . %>%
            # filter(!id == "400") %>%  # get rid of Bornholm
            rmapshaper::ms_dissolve("cluster") %>%
            st_centroid(),
        aes(label = cluster),
        color = "#dadada", fill = "#002a2a", alpha = .5,
        size = 5, fontface = 2, nudge_y = 15e3
    )


# assemble the final output
(
    f6 <- ggdraw(f6_bare) +
        draw_plot(
            f6_legend_table, x = .5, y = .6, width = .45, height = .35
        )
)

ggsave("fig/f6-map-clusters.pdf", f6,
       width = 6, height = 5)



## a3 -- choice of clusters ------------------------------------------------

load("dat/clusters_dk_m2.rda")

# Calinski-Harabasz pseudo F-statistic
# Caliński, T., & Harabasz, J. (1974). A dendrite method for cluster analysis. Communications in Statistics, 3(1), 1–27. https://doi.org/10.1080/03610927408827101
# F = {GSS / (K-1)} / {WSS / (N-K)}
# function tailored for rgeoda::skater output
ch_pseudo_f <- function(rgeoda_skater_out) {
    require(magrittr)
    cl <- rgeoda_skater_out
    k <- cl %>% extract2(1) %>% unique() %>% length()
    n <- cl %>% extract2(1) %>% length()
    gss <- cl %>% extract2(4)
    wss <- cl %>% extract2(3) %>% sum()
    pseudo_f <- {gss / (k-1)} / {wss / (n-k)}
    return(pseudo_f)
}


# common settings for exploratory plots
plot_cluster_choice <- function(clusters_df) {
    clusters_df %>%
        ggplot(aes(x, y))+
        geom_hline(yintercept = 0, size = 3/4, color = "#007a7a")+
        geom_path(size = 3/4, color = "#007a7a")+
        geom_point(size = 2)+
        coord_cartesian(ylim = c(0, NA))+
        scale_x_continuous(breaks = 2:9)
}


# test number of clusters
my_skater_ch <- function(k){
    suppressWarnings(
        rgeoda::skater(
            k, knn_w, data,
            bound_variable = dk_m2 %>% select(pop_size) %>% st_drop_geometry,
            min_bound = 582 # 10% of the total DK pop
        )
    )  %>%
        ch_pseudo_f()
}


(
    a3_cl <- 2:9 %>%
        map_dbl(my_skater_ch)%>%
        tibble(x = 2:9, y = .) %>%
        plot_cluster_choice()+
        labs(
            x = "Number of clusters",
            y = NULL,
            title = "5 nearest neighbours"
        )
)


# test number of knn
my_skater_ch_knn <- function(knn){
    suppressWarnings(
        rgeoda::skater(
            6,
            knn_weights(dk_m2, k = knn),
            data,
            bound_variable = dk_m2 %>% select(pop_size) %>% st_drop_geometry,
            min_bound = 582 # 10% of the total DK pop
        )
    )  %>%
        ch_pseudo_f()
}


(
    a3_nn <- 2:9 %>%
        map_dbl(my_skater_ch_knn)%>%
        tibble(x = 2:9, y = .) %>%
        plot_cluster_choice()+
        labs(
            x = "Number of nearest neighbours",
            y = NULL,
            title = "6 clusters"
        )
)


# assemble the final output
(
    a3 <- (a3_nn + a3_cl)+
        plot_annotation(
            tag_levels = "A",
            title = "Calinski-Harabasz pseudo F-statistic"
        )
)

ggsave("fig/a3-cluster-sensitivity.pdf", a3,
       width = 8, height = 4)



## f7 -- cluster survival 50-65 --------------------------------------------

load("dat/cl_lt_lx_ci.rda")
load("dat/std_dkse_5y_lt.rda")


se_std_lt_1519m <- std_dkse_5y_lt %>%
    filter(sex == "m", period == "2015-19", country == "Sweden")

(
    f7_surv <- cl_lt_lx_ci %>%
        mutate(
            lx_mean_1e5 = lx_mean * 1e5,
            lx_025_1e5 = lx_025 * 1e5,
            lx_975_1e5 = lx_975 * 1e5
        ) %>%
        ggplot()+
        geom_path(
            data = se_std_lt_1519m,
            aes(age, lx_1e5),
            color = 1, size = 1.5
        )+
        geom_ribbon(
            aes(age, ymin = lx_975_1e5, ymax = lx_025_1e5, fill = cluster),
            size = 1/4, alpha = .25
        )+
        geom_path(aes(age, lx_mean_1e5, color = cluster), size = 1)+
        scale_color_viridis_d(option = "H", begin = .1, end = .9, guide = NULL)+
        scale_fill_viridis_d(option = "H", begin = .1, end = .9, guide = NULL)+
        scale_y_comma(limits = c(0, 1e5), position = "right")+
        coord_cartesian(xlim = c(50, 65), ylim = c(83e3, 100e3))+
        labs(
            title = "Survival of males, 2015-19",
            y = "Period life table survival, lx",
            x = "Age"
        )+
        # annotate Swedish flag
        geom_flag(
            data = tibble(cnt = "se"),
            aes(country = cnt),
            x = 64, y = 94e3, size = 15
        )
)

load("fig/cluster-map-gg.rda")

(
    f7 <- ggdraw(f7_surv)+
        draw_plot(f6_bare, x = .02, y = .1, width = .5, height = .5)
)

ggsave(
    "fig/f7-surv-dk-clusters-se-males-5065.pdf", f7,
    width = 7, height = 5, device = cairo_pdf
)



# convert to PNG ----------------------------------------------------------

pdfs <- dir_ls("fig") %>%
    str_subset(".pdf")

dir_create("png")

convert_pdf_to_png <- function(path_to_pdf){
    path = path_to_pdf %>% paste
    pdf_convert(
        pdf = path,
        filenames = path %>%
            str_replace("fig", "png") %>%
            str_replace(".pdf", ".png"),
        dpi = 300
    )
}

pdfs %>% map(convert_pdf_to_png)
