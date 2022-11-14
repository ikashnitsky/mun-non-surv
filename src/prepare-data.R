#===============================================================================
# UPD  2020-04-19 -- rockwool
# prepare danish municipal data
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

source("src/prepare-session.R")


# municipalities geodata --------------------------------------------------

# Danish municipality geodata -- voting areas
# https://dk.okfn.org/2015/02/05/kort-over-afstemningsomrader-frigives-efter-pres-fra-open-knowledge-danmark/

# spatial data imported from
# 2020-covid19/dk-mun





# municipalities id list --------------------------------------------------

id_name <- readxl::read_xlsx("dat/dst-pop/BY2-1019.xlsx",
                              skip = 3, col_names = FALSE) %>%
    tidyr::fill(1:16, .direction = "down") %>%
    # set names
    set_colnames(
        c("sex_code", "sex", "id", "name", "age", "age_text", 2010:2019)
    ) %>%
    select(id, name) %>%
    distinct()

save(id_name, file = "data/id_name.rda")

# population data ---------------------------------------------------------

pop_1019 <- readxl::read_xlsx("dat/dst-pop/BY2-1019.xlsx",
                                 skip = 3, col_names = FALSE) %>%
    tidyr::fill(1:16, .direction = "down") %>%
    # set names
    set_colnames(
        c("sex_code", "sex", "id", "name", "age", "age_text", 2010:2019)
    ) %>%
    select(-sex_code, -age_text) %>%
    # clean identifier columns
    mutate(
        id, name,
        sex = sex %>% as_factor %>% lvls_revalue(c("m", "f")) %>% fct_rev(),
        age = age %>% as.numeric()
    )



pop_20 <- readxl::read_xlsx("dat/dst-pop/FOLK1A-20q1.xlsx",
                            skip = 3, col_names = FALSE) %>%
    tidyr::fill(1:8, .direction = "down") %>%
    # set names
    set_colnames(
        c("yeark", "yearq", "id", "name", "age", "age_text", "m", "f")
    ) %>%
    pivot_longer(names_to = "sex", cols = m:f) %>%
    transmute(
        id,
        sex = sex %>% as_factor(),
        age = age %>% as.numeric(),
        `2020` = value
    )

pop <- left_join(pop_1019, pop_20, by = c("id", "sex", "age")) %>%
    pivot_longer(names_to = "year", cols = `2010`:`2020`) %>%
    droplevels() %>%
    transmute(id, name, year, sex, age, pop = value)


save(pop, file = "dat/pop.rda")

# mid-year population

exposures <- pop %>%
    group_by(id, name, sex, age) %>%
    mutate(exposure = (pop + lead(pop))/2) %>%
    ungroup() %>%
    filter(!year=="2020") %>%
    # open age category 99+
    mutate(
        age = age %>% as_factor() %>%
            lvls_revalue(c(0:98, rep("99", 27)))
    ) %>%
    group_by(id, name, year, sex, age) %>%
    summarise(exposure = sum(exposure, na.rm = T)) %>%
    ungroup() %>%
    mutate(age = age %>% paste %>% as.numeric())

save(exposures, file = "dat/exposures.rda")

# deaths ------------------------------------------------------------------

deaths <- readxl::read_xlsx("dat/dst-deaths/FOD207-0619.xlsx",
                              skip = 3, col_names = FALSE) %>%
    tidyr::fill(1:20, .direction = "down") %>%
    # set names
    set_colnames(
        c("sex_code", "sex", "age", "age_text", "id", "name", 2006:2019)
    ) %>%
    select(-sex_code, -age_text) %>%
    filter(!age=="TOT") %>%
    # clean identifier columns
    mutate(
        id, name,
        sex = sex %>% as_factor %>% lvls_revalue(c("m", "f")) %>% fct_rev(),
        age = age %>% str_replace("99-", "99") %>% as.numeric()

    ) %>%
    pivot_longer(names_to = "year", cols = `2006`:`2019`) %>%
    droplevels() %>%
    transmute(id, name, year, sex, age, death = value)

save(deaths, file = "data/deaths.rda")


# calculate death rates ---------------------------------------------------

death_rates <- left_join(exposures, deaths %>% select(-name),
                         by = c("id", "year", "sex", "age")) %>%
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

save(death_rates, file = "dat/death_rates.rda")



# pooled death rates ------------------------------------------------------

# pooled_death_rates <- left_join(exposures, deaths %>% select(-name),
#                          by = c("id", "year", "sex", "age")) %>%
#     group_by(id, name, sex, age) %>%
#     summarise(death = death %>% sum(na.rm = T),
#               exposure = exposure%>% sum(na.rm = T)) %>%
#     ungroup() %>%
#     mutate(death_rate = death / exposure) %>%
#     # fix irregularities when deaths are larger than exposures
#     mutate(
#         death_rate = case_when(
#             death_rate %>% is_weakly_greater_than(1) ~ 1,
#             death_rate %>% is.infinite() ~ 1,
#             death_rate %>% is.nan() ~ 0,
#             TRUE ~ death_rate
#         )
#     )
#
# save(pooled_death_rates, file = "data/pooled_death_rates.rda")


# pooled 2015-2019
pooled_1519 <- left_join(exposures, deaths %>% select(-name),
                                by = c("id", "year", "sex", "age")) %>%
    filter(year %in% paste(2015:2019)) %>%
    group_by(id, name, sex, age) %>%
    summarise(death = death %>% sum(na.rm = T),
              exposure = exposure%>% sum(na.rm = T)) %>%
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

# pooled 2010-2014
pooled_1014 <- left_join(exposures, deaths %>% select(-name),
                         by = c("id", "year", "sex", "age")) %>%
    filter(year %in% paste(2010:2014)) %>%
    group_by(id, name, sex, age) %>%
    summarise(death = death %>% sum(na.rm = T),
              exposure = exposure%>% sum(na.rm = T)) %>%
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

# DK standard -------------------------------------------------------------

# # load full HMD tife tables prepared earlier in the 2020-virginia-sex-gap-e0
# load("data/lt1x1.rda")
#
# logmx_std <- lt1x1 %>%
#     filter(country == "DNK", year %in% 2010:2019) %>%
#     transmute(year, sex, age, logmx_std = mx %>% log) %>%
#     # fix Inf
#     mutate(
#         logmx_std = case_when(
#             !logmx_std %>% is.finite() ~ -12,
#             TRUE ~ logmx_std
#         )
#     )
#
# save(logmx_std, file = "data/logmx_std.rda")

library(DemoTools)
library(ungroup)

# # calculate the standard on pooled data
# pooled_std <- death_rates %>%
#     group_by(age, sex) %>%
#     summarise(n = exposure %>% sum(na.rm = T),
#               d = death %>% sum(na.rm = T)) %>%
#     group_by(sex) %>%
#     mutate(
#         # smooth with pclm
#         n_smth = n %>% pclm(x = 0:99, y = ., nlast = 1,
#                             control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
#             extract("fitted") %>% unlist(),
#         d_smth = d %>% pclm(x = 0:99, y = ., nlast = 1,
#                             control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
#             extract("fitted") %>% unlist()
#     ) %>%
#     ungroup() %>%
#     mutate(
#         mx = d / n,
#         logmx = mx %>% log,
#         mx_smth = d_smth / n_smth,
#         logmx_smth = mx_smth %>% log
#     )
#
# save(pooled_std, file = "data/pooled_std.rda")

# # what are the optimised params?
# pooled_std %>% filter(sex == "m") %>% pull(d) %>%
#     pclm(x = 0:99, y = ., nlast = 1) %>% extract("smoothPar")
#
# cbind(
#     pooled_std %>% filter(sex == "m") %>% pull(mx) %>% lt_single_mx() %>% pull(ex),
#     pooled_std %>% filter(sex == "m") %>% pull(mx_smth) %>% lt_single_mx()%>% pull(ex)
# ) %>% View()
#
# pooled_std %>% filter(sex == "m") %>%
#     ggplot(aes(age))+
#     geom_point(aes(y = logmx))+
#     geom_path(aes(y = logmx_smth))
#
# pooled_std %>% filter(sex == "m") %>%
#     ggplot(aes(age))+
#     geom_point(aes(y = n))+
#     geom_path(aes(y = n_smth))
#
# pooled_std %>% filter(sex == "m") %>%
#     ggplot(aes(age))+
#     geom_point(aes(y = d))+
#     geom_path(aes(y = d_smth))

# calculate pooled standard for 2015-19
std_1519 <- pooled_1519 %>%
    group_by(age, sex) %>%
    summarise(n = exposure %>% sum(na.rm = T),
              d = death %>% sum(na.rm = T)) %>%
    group_by(sex) %>%
    # smooth with pclm
    mutate(
        n_smth = n %>% pclm(x = 0:99, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist(),
        d_smth = d %>% pclm(x = 0:99, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist()
    ) %>%
    ungroup() %>%
    mutate(
        mx = d / n,
        logmx = mx %>% log,
        mx_smth = d_smth / n_smth,
        logmx_smth = mx_smth %>% log
    )


# calculate pooled standard for 2015-19
std_1014 <- pooled_1014 %>%
    group_by(age, sex) %>%
    summarise(n = exposure %>% sum(na.rm = T),
              d = death %>% sum(na.rm = T)) %>%
    group_by(sex) %>%
    # smooth with pclm
    mutate(
        n_smth = n %>% pclm(x = 0:99, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist(),
        d_smth = d %>% pclm(x = 0:99, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist()
    ) %>%
    ungroup() %>%
    mutate(
        mx = d / n,
        logmx = mx %>% log,
        mx_smth = d_smth / n_smth,
        logmx_smth = mx_smth %>% log
    )



# both sex ----------------------------------------------------------------
# pooled 2015-2019 both sex

pooled_1519_both <- left_join(exposures, deaths %>% select(-name),
                         by = c("id", "year", "sex", "age")) %>%
    filter(year %in% paste(2015:2019)) %>%
    group_by(id, name, age) %>%
    summarise(death = death %>% sum(na.rm = T),
              exposure = exposure%>% sum(na.rm = T)) %>%
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


# calculate pooled standard for 2015-19
std_1519_both <- pooled_1519 %>%
    group_by(age) %>%
    summarise(n = exposure %>% sum(na.rm = T),
              d = death %>% sum(na.rm = T)) %>%
    ungroup() %>%
    # smooth with pclm
    mutate(
        n_smth = n %>% pclm(x = 0:99, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist(),
        d_smth = d %>% pclm(x = 0:99, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist()
    ) %>%
    ungroup() %>%
    mutate(
        mx = d / n,
        logmx = mx %>% log,
        mx_smth = d_smth / n_smth,
        logmx_smth = mx_smth %>% log
    )

# pooled 2010-2014 both sex
pooled_1014_both <- left_join(exposures, deaths %>% select(-name),
                              by = c("id", "year", "sex", "age")) %>%
    filter(year %in% paste(2010:2014)) %>%
    group_by(id, name, age) %>%
    summarise(death = death %>% sum(na.rm = T),
              exposure = exposure%>% sum(na.rm = T)) %>%
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


# calculate pooled standard for 2010-14
std_1014_both <- pooled_1014 %>%
    group_by(age) %>%
    summarise(n = exposure %>% sum(na.rm = T),
              d = death %>% sum(na.rm = T)) %>%
    ungroup() %>%
    # smooth with pclm
    mutate(
        n_smth = n %>% pclm(x = 0:99, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist(),
        d_smth = d %>% pclm(x = 0:99, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist()
    ) %>%
    ungroup() %>%
    mutate(
        mx = d / n,
        logmx = mx %>% log,
        mx_smth = d_smth / n_smth,
        logmx_smth = mx_smth %>% log
    )



# merge pooled-5 data and standards together ------------------------------

pooled_5y <- bind_rows(
    pooled_1014_both %>%
        mutate(sex = "b", period = "2010-2014"),
    pooled_1014 %>%
        mutate(period = "2010-2014"),
    pooled_1519_both %>%
        mutate(sex = "b", period = "2015-2019"),
    pooled_1519 %>%
        mutate(period = "2015-2019")
) %>%
    # fix zero exposures
    mutate(
        exposure = case_when(
            exposure %>% equals(0) ~ 1e-8,
            TRUE ~ exposure
        )
    )

std_5y <- bind_rows(
    std_1014_both %>%
        mutate(sex = "b", period = "2010-2014"),
    std_1014 %>%
        mutate(period = "2010-2014"),
    std_1519_both %>%
        mutate(sex = "b", period = "2015-2019"),
    std_1519 %>%
        mutate(period = "2015-2019")
)

save(pooled_5y, std_5y, file = "dat/pooled_5y.rda")



# explore the data --------------------------------------------------------
options(scipen = 999)

load("dat/pooled_5y.rda")

# total population
death_rates %>%
    filter(year == "2019") %>%
    group_by(name) %>%
    summarise(e = exposure %>% sum %>% divide_by(1e3) %>% round()) %>%
    arrange(e) %>%
    mutate(name = name %>% as_factor()) %>%
    ggplot(aes(e, name, fill = e > 50))+
    geom_col()+
    scale_fill_manual(
        values = c("#4ebaaa" %>% clr_darken, "#4ebaaa"), guide = "none"
    )+
    geom_vline(xintercept = 50, color = "#7c8500")+
    geom_text(
        aes(label = e),
        hjust = 0, nudge_x = 10,
        size = 3,
        family = font_rc
    )+
    scale_x_continuous(expand = expansion(mult = c(0, .07)))+
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_rect(fill = "#dadada", color = NA),
        panel.grid.minor = element_blank()
    )+
    labs(subtitle = "Mid year population, 2019",
         y = NULL, x = "Population, thousands")

gg_pop <- last_plot()

# total deaths
death_rates %>%
    filter(year == "2019") %>%
    group_by(name) %>%
    summarise(d = death %>% sum) %>%
    arrange(d) %>%
    mutate(name = name %>% as_factor()) %>%
    ggplot(aes(d, name, fill = d > 500))+
    geom_col()+
    scale_fill_manual(
        values = c("#e4e65e" %>% clr_darken, "#e4e65e"), guide = "none"
    )+
    geom_vline(xintercept = 500, color = "#005b4f")+
    geom_text(
        aes(label = d),
        hjust = 0, nudge_x = 30,
        size = 3,
        family = font_rc
    )+
    scale_x_continuous(expand = expansion(mult = c(0, .07)))+
    theme_minimal(base_family = font_rc)+
    theme(
        plot.background = element_rect(fill = "#dadada", color = NA),
        panel.grid.minor = element_blank()
    )+
    labs(subtitle = "Deaths, 2019",
         y = NULL, x = "Deaths")

gg_death <- last_plot()

out <- gg_pop + gg_death

ggsave("fig/explore-population-size.pdf", out,
       width = 12, height = 12, device = cairo_pdf)


# proportion of non-survival 50-65 ----------------------------------------

load("dat/pop_size_19.rda")

df_q5065 <- df_fit %>%
    group_by(period, sex, id, name) %>%
    mutate(lx = mx_fit %>% lt_single_mx() %>% pull(lx)) %>%
    filter(age %in% c(50, 65)) %>%
    summarise(q5065 = 1 - (lx[2] / lx[1]))

dm_q5065 <- df_q5065 %>%
    left_join(pop_size_19) %>%
    left_join(gd_dk_mun_s %>% select(-name), by = "id") %>%
    st_as_sf() %>%
    st_transform(crs = 3044)

save(df_q5065, dm_q5065, file = "dat/non-surv-5065.rda")

