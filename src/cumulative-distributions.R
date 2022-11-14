#===============================================================================
# 2022-10-26 -- non-surv
# cumulative distributions
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


source("src/prepare-session.R")


# Denmark males and females by period -------------------------------------

load("dat/df_le_comapre.rda")


# pal <- c("#B5223BFF", "#64B6EEFF")

pal <- c("#b085f5", "#bb4d00")

df_le_comapre %>%
    filter(!sex=="b") %>%
    ungroup() %>%
    mutate(sex = sex %>% lvls_revalue(c("FEMALE", "MALE"))) %>%
    group_by(sex, period) %>%
    arrange(life_exp) %>%
    mutate(
        prop_e = e %>% prop.table() %>% cumsum()
    ) %>%
    ungroup() %>%
    filter(!sex=="b") %>%
    ungroup() %>%
    mutate(sex = sex %>% lvls_revalue(c("FEMALE", "MALE"))) %>%
    group_by(sex, period) %>%
    arrange(life_exp) %>%
    mutate(
        prop_e = e %>% prop.table() %>% cumsum()
    ) %>%
    ungroup() %>%
    ggplot()+
    geom_hline(yintercept = .5, size = .75, color = "#9a9a9a")+
    geom_step(data = . %>% filter(period == "2010-2014"),
              aes(life_exp, prop_e, color = sex),
              size = 2/3)+
    geom_step(data = . %>% filter(period == "2015-2019"),
              aes(life_exp, prop_e, color = sex),
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
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_rect(fill = "#dadada", color = NA),
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")
    )+
    labs(x = "Life expectancy",
         y = "Cumulative proprotion of the population",
         size = "Populaion,\nthous.")

gg_dk <- last_plot()


gg_legend <- ggplot()+
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
        'text', x = .45, y = c(.175, .475), vjust = .5, hjust = 1,
        size = 4, fontface = 1, label = c('Males', 'Females'), family = font_rc, color = pal %>% rev
    )+
    annotate(
        'text', x = c(.7, 1.05), y = .65, vjust = 0, hjust = c(1, .5),
        size = 4, fontface = c(1, 2), alpha = c(1, .5), color = "#444444", label = c('2010-14', '2015-19'), family = font_rc
    )+
    theme_void()


(
    gg_out_dk <- ggdraw() +
        draw_plot(gg_dk)+
        draw_plot(gg_legend, x = 0, y = .73, width = .35, height = .25)
)



ggsave(
    filename = "fig/cum-dk-period.pdf", plot = gg_out_dk,
    width = 7, height = 4, device = cairo_pdf
)


# Sweden males and females by period -------------------------------------

load("dat/sw-df_le_comapre.rda")

sw_df_le_comapre %>%
    filter(!sex=="b") %>%
    ungroup() %>%
    mutate(sex = sex %>% lvls_revalue(c("FEMALE", "MALE"))) %>%
    group_by(sex, period) %>%
    arrange(life_exp) %>%
    mutate(
        prop_e = e %>% prop.table() %>% cumsum()
    ) %>%
    ungroup() %>%
    ggplot()+
    geom_hline(yintercept = .5, size = .75, color = "#9a9a9a")+
    geom_step(data = . %>% filter(period == "2010-2014"),
              aes(life_exp, prop_e, color = sex),
              size = 2/3)+
    geom_step(data = . %>% filter(period == "2015-2019"),
              aes(life_exp, prop_e, color = sex),
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
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_rect(fill = "#dadada", color = NA),
        legend.position = "none",
        panel.spacing.x = unit(1, "lines")
    )+
    labs(x = "Life expectancy",
         y = "Cumulative proprotion of the population",
         size = "Populaion,\nthous.")

gg_se <- last_plot()


(
    gg_out_se <- ggdraw() +
        draw_plot(gg_se)+
        draw_plot(gg_legend, x = 0, y = .73, width = .35, height = .25)
)



ggsave(
    filename = "fig/cum-se-period.pdf", plot = gg_out_se,
    width = 7, height = 4, device = cairo_pdf
)


# join together with patchwork
gg_out <- gg_out_dk / gg_out_se + plot_annotation(tag_levels = "A")

ggsave(
    filename = "fig/cum-dk-and-se.pdf", plot = gg_out,
    width = 7, height = 8, device = cairo_pdf, bg = "#dadada"
)


# # compare Denmark and Sweden ----------------------------------------------
# merge for comparison
df_dkse <- bind_rows(
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

pal_dkse <- c("#B5223BFF", "#64B6EEFF")

df_dkse %>%
    filter(sex=="b", period == "2015-2019") %>%
    ggplot(aes(e, color = country))+
    geom_hline(yintercept = .5, size = .5, color = "#fafafa")+
    stat_ecdf(size = 1)+
    scale_x_continuous(trans = "log", breaks = c(5, 10, 25, 50, 100, 250, 500))+
    scale_y_percent()+
    scale_color_manual(values = pal_dkse, guide = "none")+
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_rect(fill = "#dadada", color = NA),
        panel.grid.minor = element_blank()
    )+
    labs(
        x = "Municipality population size, thous.",
        y = "Cumulative density"
    )

ggsave(filename = "fig/cum-mun-pop-size-dkse.pdf",
       width = 7, height = 4, device = cairo_pdf)


#
# pal_dk_se <- c("#C8102E", "#c8b900")
#
# df_dk_se <- bind_rows(
#     df_le_comapre, sw_df_le_comapre, .id = "country"
# ) %>%
#     mutate(
#         country = country %>% as_factor %>%
#             lvls_revalue(new_levels = c("Denmark", "Sweden"))
#     ) %>%
#     filter(!sex=="b") %>%
#     ungroup() %>%
#     mutate(sex = sex %>% lvls_revalue(c("FEMALE", "MALE"))) %>%
#     group_by(country, sex, period) %>%
#     arrange(life_exp) %>%
#     mutate(
#         prop_e = e %>% prop.table() %>% cumsum()
#     ) %>%
#     ungroup()
#
#
# df_dkse %>%
#     # filter(sex == "f") %>%
#     ggplot()+
#     geom_hline(yintercept = .5, size = .75, color = "#9a9a9a")+
#     geom_step(data = . %>% filter(period == "2010-2014"),
#               aes(life_exp, prop_e, color = country),
#               size = 2/3)+
#     geom_step(data = . %>% filter(period == "2015-2019"),
#               aes(life_exp, prop_e, color = country),
#               size = 3/2, alpha = .5)+
#     # geom_flag(
#     #     data = tibble(country = "se"),
#     #     aes(country = country),
#     #     x = 86, y = .125, size = 15
#     # )+
#     scale_color_manual(values = pal_dkse %>% clr_darken(.2))+
#     scale_y_percent(position = "right", expand = c(.01, .01))+
#     scale_x_continuous(
#         limits = c(74, 87), breaks = seq(75, 85, 2.5), expand = c(0,0)
#     )+
#     theme_minimal(base_family = font_rc)+
#     theme(
#         legend.position = "none",
#         panel.spacing.x = unit(1, "lines")
#     )+
#     labs(x = "Life expectancy",
#          y = "Cumulative proprotion of the population",
#          size = "Population,\nthous.")
