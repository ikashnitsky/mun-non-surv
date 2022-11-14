#===============================================================================
# 2022-11-07 -- non-surv
# life tables for spatial clusters
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================



source("src/prepare-session.R")
source("src/fun-topals-fit.R")
source("src/fun-show-topals.R")
source("src/fun-life-table.R")

load("dat/exposures.rda")
load("dat/deaths.rda")
load("dat/clusters_dk_m2.rda")
load("dat/pooled_5y.rda")

# ONLY MALES
# pooled 2015-2019
pooled_1519_clusters <- left_join(
    exposures,
    deaths %>% select(-name),
    by = c("id", "year", "sex", "age")
) %>%
    filter(year %in% paste(2015:2019), sex == "m") %>%
    left_join(clusters_dk_m2) %>%
    drop_na() %>%
    group_by(cluster, age) %>%
    summarise(death = death %>% sum(na.rm = T),
              exposure = exposure %>% sum(na.rm = T)) %>%
    ungroup() %>%
    mutate(death_rate = death / exposure) %>%
    # fix irregularities when deaths are larger than exposures
    mutate(
        death_rate = case_when(
            death_rate %>% is_weakly_greater_than(1) ~ 1,
            death_rate %>% is.infinite() ~ 1,
            death_rate %>% is.nan() ~ 0,
            TRUE ~ death_rate
        )
    )

# join standard
df <- pooled_1519_clusters %>%
    mutate(age = age %>% as.integer()) %>%
    left_join(
        std_5y %>%
            filter(sex == "m", period == "2015-2019") %>%
            transmute(age, logmx_smth)
    )

# fit TOPALS to all 6 clusters
df_fit <- df %>%
    group_by(cluster) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = exposure, D = death, std = logmx_smth,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% c(),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()

# calculate full life tables
df_lt <- df_fit %>%
    group_by(cluster) %>%
    group_modify(~ lt(mx = .x$mx_fit, sex = "m"))



# plot cluster survival curves against DK and SE --------------------------

load("dat/std_dkse_5y_lt.rda")


se_std_lt_1519m <- std_dkse_5y_lt %>%
    filter(sex == "m", period == "2015-2019", country == "Sweden")

(
    gg_surv <- df_lt %>%
        mutate(lx_1e5 = lx * 1e5) %>%
        ggplot()+
        geom_path(
            data = se_std_lt_1519m,
            aes(age, lx_1e5),
            color = "#64B6EE", size = 2
        )+
        geom_path(aes(age, lx_1e5, color = cluster), size = 1)+
        scale_color_viridis_d()+
        scale_y_comma(limits = c(0, 1e5), position = "right")+
        coord_cartesian(xlim = c(50, 65), ylim = c(83e3, 100e3))+
        theme_minimal(base_family = font_rc, base_size = 16)+
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "none"
        )+
        labs(
            title = "Survival of males, Denmark (6 clusters) and Sweden, 2015-19",
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

out <- ggdraw(gg_surv)+
    draw_plot(gg, x = .02, y = .1, width = .5, height = .5)+
    theme(plot.background = element_rect(fill = "#dadada", color = NA))

ggsave(
    "fig/surv-dk-clusters-se-males-5065.pdf", out,
    width = 7, height = 5, device = cairo_pdf
)
