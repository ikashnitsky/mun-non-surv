#===============================================================================
# 2020-06-09 -- rockwool
# compare DK and SE from HMD
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


source("src/prepare-session.R")
# UPD  2022-11-07 ------------------------------
# show for the national standards
source("src/fun-life-table.R")

load("dat/pooled_5y.rda")
load("dat/sw-pooled_5y.rda")

std_dkse_5y_lt <- bind_rows(
    std_5y,
    sw_std_5y,
    .id = "country"
) %>%
    mutate(
        country = country %>% as_factor() %>%
            lvls_revalue(c("Denmark", "Sweden")) %>%
            paste()
    ) %>%
    filter(!age == 100) %>%
    group_by(period, country, sex) %>%
    group_modify(~ lt(mx = .x$mx_smth, sex = "m")) %>%
    ungroup() %>%
    mutate(lx_1e5 = lx * 1e5)

save(std_dkse_5y_lt, file = "dat/std_dkse_5y_lt.rda")

# # read in HDM
# load("dat/lt1x1.rda")
#
# surv_dkse <- lt1x1 %>%
#     filter(country %>% is_in(c("DNK", "SWE")),
#            year == 2016)

# pal <- c("#df356b", "#eec21f")
# pal_dkse <- c("#ec4c5c", "#408cc1")
pal_dkse <- c("#B5223BFF", "#64B6EEFF")

std_dkse_5y_lt %>%
    filter(sex == "m", period == "2015-2019") %>%
    ggplot(aes(age, lx_1e5, color = country))+
    # geom_rect(aes(xmin = 50, xmax = 65, ymin = -Inf, ymax = Inf),
    #           fill = "#222222", color = NA, alpha = .1)+
    geom_path(size = 1)+
    scale_color_manual(values = pal_dkse, guide = NULL)+
    scale_y_comma(limits = c(0, 1e5))+
    theme_minimal(base_family = font_rc, base_size = 16)+
    theme(
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#dadada", color = NA)
    )

gg_bare <- last_plot()

(
    gg_labs <- gg_bare +
        annotate("text", label = c("Denmark", "Sweden"),
                 x = 65, y = c(75e3, 95e3), hjust = 1:0,
                 size = 5, color = pal_dkse, family = font_rc)+
        labs(title = "Survival of males, Denmark and Sweden, 2015-19",
             y = "Period life table survival, lx",
             x = "Age")
)


ggsave("fig/surv-dk-se-males.pdf", gg_labs %>% ggdraw(), width = 7, height = 5, device = cairo_pdf)


(
    out <-  ggdraw(gg_labs)+
        draw_plot(
            gg_bare +
                coord_cartesian(xlim = c(50, 65), ylim = c(85e3, 100e3))+
                geom_hline(dat = . %>% filter(age %in% c(50, 65)),
                           aes(yintercept = lx_1e5, color = country))+
                geom_text(data = . %>% filter(age==65),
                           aes(label = lx_1e5 %>% round(-2) %>%
                                   divide_by(1e3) %>% paste0("k"),
                               y = lx_1e5 + 500, color = country),
                          x = 52.5, hjust = .5, vjust = 0,
                          size = 5, family = font_rc)+
                geom_text(data = . %>% filter(age==50),
                          aes(label = lx_1e5 %>% round(-2) %>%
                                  divide_by(1e3) %>% paste0("k"),
                              y = lx_1e5, color = country),
                          x = 62.5, hjust = .5, vjust = c(1.2, -.2),
                          size = 5, family = font_rc)+
                scale_y_comma(breaks = seq(85e3, 100e3, 5e3), position = "right")+
                labs(x = NULL, y = NULL)+
                theme(
                    plot.background = element_rect(fill = "#dadada", color = "#7a7a7a")
                ),
            x = .2, y = .15, width = .5, height = .45
        )
)

ggsave("fig/surv-dk-se-males-ann.pdf", out, width = 7, height = 5, device = cairo_pdf)
