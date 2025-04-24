#===============================================================================
# 2025-04-24 -- mun-non-surv
# prepare municipal data -- DK and SE
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

source("src/0-prepare-session.R")


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

save(id_name, file = "dat/id_name.rda")

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

dk_pop <- left_join(pop_1019, pop_20, by = c("id", "sex", "age")) %>%
    pivot_longer(names_to = "year", cols = `2010`:`2020`) %>%
    droplevels() %>%
    transmute(id, name, year, sex, age, pop = value)


# mid-year population

dk_exposures <- dk_pop %>%
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




# deaths ------------------------------------------------------------------

dk_deaths <- readxl::read_xlsx("dat/dst-deaths/FOD207-0619.xlsx",
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



# calculate death rates ---------------------------------------------------

dk_death_rates <- left_join(dk_exposures, dk_deaths %>% select(-name),
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

save(dk_pop, dk_deaths, dk_exposures, dk_death_rates, file = "dat/dk_raw_counts.rda")



# pooled death rates ------------------------------------------------------

# pooled 5 year periods 2010-14 and 2015-19
dk_mun_sex <- left_join(
    dk_exposures, dk_deaths %>% select(-name),
    by = c("id", "year", "sex", "age")
) %>%
    filter(year %in% paste(2010:2019)) %>%
    mutate(
        period = year %>%
            as.numeric() %>%
            case_match(
            2010:2014 ~ "2010-14",
            2015:2019 ~ "2015-19"
        )
    ) %>%
    group_by(id, name, period, sex, age) %>%
    summarise(
        death = death %>% sum(na.rm = T),
        exposure = exposure %>% sum(na.rm = T)
    ) %>%
    ungroup() %>%
    # fix zero exposures
    mutate(
        exposure = exposure %>%
            case_match(0 ~ 1, .default = exposure)
    )

# calculate both sex
dk_mun_both <- dk_mun_sex %>%
    group_by(id, name, period, age) %>%
    summarise(
        death = death %>% sum(na.rm = T),
        exposure = exposure %>% sum(na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(sex = "b")

# join to the two sex dataset
dk_mun_pooled <- dk_mun_sex %>%
    bind_rows(dk_mun_both) %>%
    arrange(id, period, sex, age)


# save
save(dk_mun_pooled, file = "dat/dk_mun_pooled_raw.rda")


# pop size of the municipalities, mid-period ------------------------------------------

load("dat/dk_mun_pooled_raw.rda")

dk_mun_size <- dk_mun_pooled %>%
    group_by(id, name, period, sex) %>%
    summarise(
        pop_size = exposure %>% sum(na.rm = T) %>% divide_by(5)
    ) %>%
    ungroup()

# save
save(dk_mun_size, file = "dat/dk_mun_size.rda")


# DK standard -------------------------------------------------------------

# To produce Danish standard, I will take all HMD pooled population and use it as a standard for TOPALS estimation of the DK national standard
# This part of the data preparation is not easily reproducible since I'm using a local version of the full HMD database. You would need to download and unpack it on your own machine in order to replicate this part of the code. Yet, you can find the output in the 'dat' folder of this repository, so you can use it.
# Source: https://www.mortality.org/Data/ZippedDataFiles

# Remove duplicate and irrelevant populations, East EU
hmd_populations_to_exlude <- c("BGR", "BLR", "CZE", "CHL", "DEUTE", "DEUTW", "FRACNP", "GBRCENW", "HKG", "HRV", "HUN", "ISR", "KOR", "LTU", "LVA", "NZL_MA", "NZL_NP", "POL", "RUS", "SVN", "SVK", "TWN", "UKR")

# deaths
hmd_d <- fread_hmd_dir("/data/hmd/deaths/Deaths_1x1/") %>%
    filter(! country %in% hmd_populations_to_exlude) %>%
    pivot_longer(female:total, names_to = "sex", values_to = "deaths") %>%
    mutate(sex = sex %>% str_sub(1, 1) %>% str_replace("t", "b")) %>%
    filter(year %in% 2010:2019)

# exposures
hmd_e <- fread_hmd_dir("/data/hmd/exposures/Exposures_1x1/") %>%
    filter(! country %in% hmd_populations_to_exlude) %>%
    pivot_longer(female:total, names_to = "sex", values_to = "exposure") %>%
    mutate(sex = sex %>% str_sub(1, 1)%>% str_replace("t", "b")) %>%
    filter(year %in% 2010:2019)

hmd_m <- left_join(hmd_d, hmd_e)

# pool populations together
hmd_pooled_std <- hmd_m %>%
    mutate(
        period = year %>% case_match(
            2010:2014 ~ "2010-14",
            2015:1019 ~ "2015-19"
        )
    ) %>%
    drop_na() %>%
    group_by(period, sex, age) %>%
    summarise(
        deaths = deaths %>% sum(na.rm = T),
        exposure = exposure %>% sum(na.rm = T)
    ) %>%
    mutate( mx = deaths / exposure)

# visualize
hmd_pooled_std %>%
    filter(period == "2015-19") %>%
    ggplot(aes(age, y = mx %>% log(), color = sex))+
    geom_line()

# Kannisto smooth old age

# smooth death rates above age 95
hmd_mx_old <- hmd_pooled_std %>%
    filter(age %>% is_weakly_greater_than(80)) %>%
    group_by(period, sex) %>%
    mutate(mx = kannisto_smooth(mx, deaths, exposure))

# join back the smoothed rates
hmd_pooled_std_smooth <- bind_rows(
    hmd_pooled_std %>% filter(age %>% is_less_than(80)),
    hmd_mx_old
) %>%
    arrange(period, sex, age) %>%
    transmute(period, sex, age, mx_std = mx, logmx_std = mx %>% log())

save(hmd_pooled_std_smooth, file = "dat/hmd_pooled_std_smooth.rda")

# # check
# hmd_pooled_std %>%
#     filter(period == "2015-19", sex == "m") %>%
#     ggplot(aes(age, y = mx %>% log()))+
#     geom_point(color = 2)+
#     geom_point(
#         data = hmd_pooled_std_smooth %>%
#             filter(period == "2015-19", sex == "m"),
#         aes(y = logmx_std),
#         color = 1
#     )


# Denmark -- produce a smooth standard for the pooled 5y periods
dk_pooled <- hmd_m %>%
    filter(country == "DNK") %>%
    mutate(
        period = year %>% case_match(
            2010:2014 ~ "2010-14",
            2015:1019 ~ "2015-19"
        )
    ) %>%
    drop_na() %>%
    group_by(period, sex, age) %>%
    summarise(
        deaths = deaths %>% sum(na.rm = T),
        exposure = exposure %>% sum(na.rm = T)
    )

# Danish raw death rates are jerky !!!
dk_pooled %>%
    filter(period == "2015-19") %>%
    ggplot(aes(age, y = (deaths/exposure) %>% log(), color = sex))+
    geom_point()

# attach European standard info

dk_pooled_std <- dk_pooled %>%
    left_join(hmd_pooled_std_smooth) %>%
    # we don't need ages 100+ since
    #we don't have them for the municipalities
    filter(age %>% is_less_than(100))

dk_fitted_std <- dk_pooled_std %>%
    group_by(period, sex) %>%
    group_modify(~{
        TOPALS_fit(
            N = .x$exposure, D = .x$deaths,
            std = .x$logmx_std,
            age_group_bounds   = 0:100,
            max_iter = 1e4, details = T
        ) %>%
            extract2("logm") %>% c() %>%
            tibble(logmx_std = .) %>%
            mutate(age = 0:99)
    })

# have a look
dk_fitted_std %>%
    ggplot(aes(age, y = logmx_std, color = sex))+
    geom_line()+
    facet_row(~period)

# save
save(dk_fitted_std, file = "dat/dk_fitted_std.rda")



# SE: now repeat all the same for Sweden --------------------------------------


# se population

se_pop <- readxl::read_xlsx("dat/scb-pop/p1020.xlsx",
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



# se exposures


se_exposures <- se_pop %>%
    group_by(id, name, sex, age) %>%
    mutate(exposure = (pop + lead(pop))/2) %>%
    ungroup() %>%
    filter(!year=="2020") %>%
    # open age category 99+
    mutate(
        age = age %>% as_factor() %>%
            lvls_revalue(c(0:98, rep("99", 2)))
    ) %>%
    group_by(id, name, year, sex, age) %>%
    summarise(exposure = sum(exposure, na.rm = T)) %>%
    ungroup() %>%
    mutate(age = age %>% paste %>% as.numeric())


# se deaths

se_deaths <- readxl::read_xlsx("dat/scb-deaths/d1019.xlsx",
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
    transmute(id, name, year, sex, age, death = value) %>%
    # open age category 99+
    mutate(
        age = age %>% as_factor() %>%
            lvls_revalue(c(0:98, rep("99", 2)))
    ) %>%
    group_by(id, name, year, sex, age) %>%
    summarise(death = sum(death, na.rm = T)) %>%
    ungroup() %>%
    mutate(age = age %>% paste %>% as.numeric())


# calculate death rates

se_death_rates <- left_join(se_exposures, se_deaths %>% select(-name),
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

save(se_pop, se_exposures, se_deaths, se_death_rates, file = "dat/se-raw-counts.rda")

# pop size of the municipalities, mid-period

load("dat/se_mun_pooled_raw.rda")

se_mun_size <- se_mun_pooled %>%
    group_by(id, name, period, sex) %>%
    summarise(
        pop_size = exposure %>% sum(na.rm = T) %>% divide_by(5)
    ) %>%
    ungroup()

# save
save(se_mun_size, file = "dat/se_mun_size.rda")



# SE pooled 5 year periods 2010-14 and 2015-19
se_mun_sex <- left_join(
    se_exposures, se_deaths %>% select(-name),
    by = c("id", "year", "sex", "age")
) %>%
    filter(year %in% paste(2010:2019)) %>%
    mutate(
        period = year %>%
            as.numeric() %>%
            case_match(
                2010:2014 ~ "2010-14",
                2015:2019 ~ "2015-19"
            )
    ) %>%
    group_by(id, name, period, sex, age) %>%
    summarise(
        death = death %>% sum(na.rm = T),
        exposure = exposure %>% sum(na.rm = T)
    ) %>%
    ungroup() %>%
    # fix zero exposures
    mutate(
        exposure = exposure %>%
            case_match(0 ~ 1, .default = exposure)
    )

# calculate both sex
se_mun_both <- se_mun_sex %>%
    group_by(id, name, period, age) %>%
    summarise(
        death = death %>% sum(na.rm = T),
        exposure = exposure %>% sum(na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(sex = "b")

# join to the two sex dataset
se_mun_pooled <- se_mun_sex %>%
    bind_rows(se_mun_both) %>%
    arrange(id, period, sex, age)


# save
save(se_mun_pooled, file = "dat/se_mun_pooled_raw.rda")


# Sweden -- produce a smooth standard for the pooled 5y periods
se_pooled <- hmd_m %>%
    filter(country == "SWE") %>%
    mutate(
        period = year %>% case_match(
            2010:2014 ~ "2010-14",
            2015:1019 ~ "2015-19"
        )
    ) %>%
    drop_na() %>%
    group_by(period, sex, age) %>%
    summarise(
        deaths = deaths %>% sum(na.rm = T),
        exposure = exposure %>% sum(na.rm = T)
    )

# Are Swedish raw death rates also jerky?
se_pooled %>%
    filter(period == "2015-19") %>%
    ggplot(aes(age, y = (deaths/exposure) %>% log(), color = sex))+
    geom_point()

# attach European standard info

se_pooled_std <- se_pooled %>%
    left_join(hmd_pooled_std_smooth) %>%
    # we don't need ages 100+ since
    #we don't have them for the municipalities
    filter(age %>% is_less_than(100))

se_fitted_std <- se_pooled_std %>%
    group_by(period, sex) %>%
    group_modify(~{
        TOPALS_fit(
            N = .x$exposure, D = .x$deaths,
            std = .x$logmx_std,
            age_group_bounds   = 0:100,
            max_iter = 1e4, details = T
        ) %>%
            extract2("logm") %>% c() %>%
            tibble(logmx_std = .) %>%
            mutate(age = 0:99)
    })

# have a look
se_fitted_std %>%
    ggplot(aes(age, y = logmx_std, color = sex))+
    geom_line()+
    facet_row(~period)

# save
save(se_fitted_std, file = "dat/se_fitted_std.rda")


# 5y life tables for the national standards -------------------------------

load("dat/dk_fitted_std.rda")
load("dat/se_fitted_std.rda")

std_dkse_5y_lt <- bind_rows(
    dk_fitted_std,
    se_fitted_std,
    .id = "country"
) %>%
    mutate(
        country = country %>% as_factor() %>%
            lvls_revalue(c("Denmark", "Sweden")) %>%
            paste()
    ) %>%
    filter(!age == 100) %>%
    group_by(period, country, sex) %>%
    group_modify(~ lt(mx = .x$logmx %>% exp, a0 = .3)) %>%
    ungroup() %>%
    mutate(lx_1e5 = lx * 1e5)

save(std_dkse_5y_lt, file = "dat/std_dkse_5y_lt.rda")


# SOULD NOT BE HERE -----------------------------------------------------------------

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


