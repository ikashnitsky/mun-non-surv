#===============================================================================
# UPD  2020-10-29 -- rockwool
# prepare swedish municipal data
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

source("src/prepare-session.R")


# sw population -----------------------------------------------------------

sw_pop <- readxl::read_xlsx("dat/scb-pop/p1020.xlsx",
                                 skip = 3, col_names = FALSE) %>%
    tidyr::fill(1:17, .direction = "down") %>%
    # set names
    set_colnames(
        c("id", "name", "age", "age_text", "sex_code", "sex", 2010:2020)
    ) %>%
    select(-sex_code, -age_text) %>%
    # clean identifier columns
    mutate(
        id, name,
        sex = sex %>% as_factor %>% lvls_revalue(c("m", "f")) %>% fct_rev(),
        age = age %>% str_remove("\\+") %>% as.numeric()
    ) %>%
    pivot_longer(names_to = "year", cols = `2010`:`2020`) %>%
    droplevels() %>%
    transmute(id, name, year, sex, age, pop = value)

save(sw_pop, file = "dat/sw-pop-1020.rda")


# sw exposures ------------------------------------------------------------


sw_exposures <- sw_pop %>%
    group_by(id, name, sex, age) %>%
    mutate(exposure = (pop + lead(pop))/2) %>%
    ungroup() %>%
    filter(!year=="2020") %>%
    # open age category 99+
    mutate(
        age = age %>% as_factor()
    ) %>%
    group_by(id, name, year, sex, age) %>%
    summarise(exposure = sum(exposure, na.rm = T)) %>%
    ungroup() %>%
    mutate(age = age %>% paste %>% as.numeric())

save(sw_exposures, file = "dat/sw-exposures.rda")

# sw deaths  --------------------------------------------------------------

sw_deaths <- readxl::read_xlsx("dat/scb-deaths/d1019.xlsx",
                                    skip = 3, col_names = FALSE) %>%
    tidyr::fill(1:16, .direction = "down") %>%
    # set names
    set_colnames(
        c("id", "name", "age", "age_text", "sex_code", "sex", 2010:2019)
    ) %>%
    select(-sex_code, -age_text) %>%
    # clean identifier columns
    mutate(
        id, name,
        sex = sex %>% as_factor %>% lvls_revalue(c("m", "f")) %>% fct_rev(),
        age = age %>% str_remove("\\+") %>% as.numeric()
    ) %>%
    pivot_longer(names_to = "year", cols = `2010`:`2019`) %>%
    droplevels() %>%
    transmute(id, name, year, sex, age, death = value)

save(sw_deaths, file = "dat/sw-deaths-1019.rda")

# calculate death rates ---------------------------------------------------

sw_death_rates <- left_join(sw_exposures, sw_deaths %>% select(-name),
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

save(sw_death_rates, file = "dat/sw-death-rates.rda")



# pooled death rates in 5y periods ----------------------------------------


# pooled 2015-2019
pooled_1519 <- left_join(sw_exposures, sw_deaths %>% select(-name),
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
pooled_1014 <- left_join(sw_exposures, sw_deaths %>% select(-name),
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


# the standard for sweden in poooled 5 years ------------------------------

# calculate pooled standard for 2015-19
std_1519 <- pooled_1519 %>%
    group_by(age, sex) %>%
    summarise(n = exposure %>% sum(na.rm = T),
              d = death %>% sum(na.rm = T)) %>%
    group_by(sex) %>%
    # smooth with pclm
    mutate(
        n_smth = n %>% pclm(x = 0:100, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist(),
        d_smth = d %>% pclm(x = 0:100, y = ., nlast = 1,
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
        n_smth = n %>% pclm(x = 0:100, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist(),
        d_smth = d %>% pclm(x = 0:100, y = ., nlast = 1,
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

pooled_1519_both <- left_join(sw_exposures, sw_deaths %>% select(-name),
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
        n_smth = n %>% pclm(x = 0:100, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist(),
        d_smth = d %>% pclm(x = 0:100, y = ., nlast = 1,
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
pooled_1014_both <- left_join(sw_exposures, sw_deaths %>% select(-name),
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
        n_smth = n %>% pclm(x = 0:100, y = ., nlast = 1,
                            control = list(lambda = 1e3, kr = 3, deg = 3)) %>%
            extract("fitted") %>% unlist(),
        d_smth = d %>% pclm(x = 0:100, y = ., nlast = 1,
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

sw_pooled_5y <- bind_rows(
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

sw_std_5y <- bind_rows(
    std_1014_both %>%
        mutate(sex = "b", period = "2010-2014"),
    std_1014 %>%
        mutate(period = "2010-2014"),
    std_1519_both %>%
        mutate(sex = "b", period = "2015-2019"),
    std_1519 %>%
        mutate(period = "2015-2019")
)

save(sw_pooled_5y, sw_std_5y, file = "dat/sw-pooled_5y.rda")

