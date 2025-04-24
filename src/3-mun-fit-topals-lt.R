#===============================================================================
# 2025-04-24 -- mun-non-surv
# Fit TOPALS to municipal data + Poisson bootstrap
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

source("src/0-prepare-session.R")



# calculate TOPALS life tables for pooled 5y data, by sex -----------------

load("dat/dk_mun_pooled_raw.rda")
load("dat/dk_fitted_std.rda")

dk_ready_for_topals <- dk_mun_pooled %>%
    mutate(age = age %>% as.integer()) %>%
    left_join(dk_fitted_std)

# # these are tiny island communes
# id_tiny <- c("411", "492", "563", "741", "825")

# fit TOPALS to all municipalities
dk_fit <- dk_ready_for_topals %>%
    filter(!id %in% c("411")) %>% # remove Christiansø
    group_by(period, sex, id, name) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = exposure, D = death, std = logmx_std,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% as.vector(),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()

save(dk_fit, file = "dat/dk_mun_pooled_topals.rda")

load("dat/dk_mun_pooled_topals.rda")



# calculate life tables and extract quantities like e0 and p5065 ---------------------------------------------------

dk_mun_lt <- dk_fit %>%
    group_by(period, sex, id, name) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()

save(dk_mun_lt, file = "dat/dk_mun_lt.rda")

# summarize life tables
dk_mun_sum <- dk_mun_lt %>%
    group_by(period, sex, id, name) %>%
    summarise(
        le = ex %>% first(),
        q5065 = 1 - (lx[66] / lx[51])
    )


# bootstrap confidence intervals ------------------------------------------
# To lighten the github repository the ~2GB simulated data is not uploaded there. Please note, it will take considerable time to simulate this data locally again. The mentioned times are on my machine. The produced data is saved in the "tmp" local directory that is excluded from syncing to github. Only the

dk_ready_for_boot <- dk_ready_for_topals %>%
    group_by(period, sex, id, name) %>%
    expand_grid(sim_id = 1:5e2) %>%
    group_by(period, sex, id, name, age) %>%
    mutate(death_sim = rpois(5e2, death)) %>%
    ungroup()

# fit TOPALS to the simulated death counts ~ 37 min
tictoc::tic()
dk_boot_fit <- dk_ready_for_boot %>%
    filter(!id %in% c("411")) %>% # remove Christiansø
    group_by(period, sex, id, name, sim_id) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = exposure, D = death_sim, std = logmx_std,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% as.vector(),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()
tictoc::toc()

# save to temp location
tictoc::tic()
save(dk_boot_fit, file = "~/tmp-simulated/dk_boot_fit.rda")
tictoc::toc()

# calculate simulated life tables ~ 12 min
tictoc::tic()
dk_boot_lt <- dk_boot_fit %>%
    group_by(period, sex, id, name, sim_id) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()
tictoc::toc()

# save to temp location ~ 1 min
tictoc::tic()
save(dk_boot_lt, file = "~/tmp-simulated/dk_boot_lt.rda")
tictoc::toc()

load("~/tmp-simulated/dk_boot_lt.rda")


# summarize simulated life tables with credible intervals
tictoc::tic()
dk_mun_sum_ci <- dk_boot_lt %>%
    group_by(period, sex, id, name, sim_id) %>%
    summarise(
        le = ex[1],
        q5065 = 1 - (lx[66] / lx[51])
    ) %>%
    group_by(period, sex, id, name) %>%
    summarise(
        le_mean = le %>% mean(na.rm = T),
        le_025 = le %>% quantile(.025),
        le_100 = le %>% quantile(.1),
        le_500 = le %>% quantile(.5),
        le_900 = le %>% quantile(.9),
        le_975 = le %>% quantile(.975),
        q5065_mean = q5065 %>% mean(na.rm = T),
        q5065_025 = q5065 %>% quantile(.025),
        q5065_100 = q5065 %>% quantile(.1),
        q5065_500 = q5065 %>% quantile(.5),
        q5065_900 = q5065 %>% quantile(.9),
        q5065_975 = q5065 %>% quantile(.975)
    ) %>%
    ungroup()

tictoc::toc()

# save the summary table to permanent location
save(dk_mun_sum_ci, file = "dat/dk_mun_sum_ci.rda")


# SWEDEN ------------------------------------------------------------------

## TOPALS 5y data, by sex -----------------

load("dat/se_mun_pooled_raw.rda")
load("dat/se_fitted_std.rda")

se_ready_for_topals <- se_mun_pooled %>%
    mutate(age = age %>% as.integer()) %>%
    left_join(
        se_fitted_std %>% transmute(period, sex, age, logmx_std)
    )


# fit TOPALS to all municipalities
se_fit <- se_ready_for_topals %>%
    group_by(period, sex, id, name) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = exposure, D = death, std = logmx_std,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% as.vector(),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()

save(se_fit, file = "dat/se_mun_pooled_topals.rda")

load("dat/se_mun_pooled_topals.rda")



## calculate life tables and extract quantities like e0 and p5065 ---------------------------------------------------

se_mun_lt <- se_fit %>%
    group_by(period, sex, id, name) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()

save(se_mun_lt, file = "dat/se_mun_lt.rda")

# summarize life tables
se_mun_sum <- se_mun_lt %>%
    group_by(period, sex, id, name) %>%
    summarise(
        le = ex %>% first(),
        q5065 = 1 - (lx[66] / lx[51])
    )


## bootstrap confidence intervals ------------------------------------------
# To lighten the github repository the ~2GB simulated data is not uploaded there. Please note, it will take considerable time to simulate this data locally again. The mentioned times are on my machine. The produced data is saved in the "tmp" local directory that is excluded from syncing to github. Only the

tictoc::tic()
se_ready_for_boot <- se_ready_for_topals %>%
    group_by(period, sex, id, name) %>%
    expand_grid(sim_id = 1:5e2) %>%
    group_by(period, sex, id, name, age) %>%
    mutate(death_sim = rpois(5e2, death)) %>%
    ungroup()
tictoc::toc()

# fit TOPALS to the simulated death counts ~ 178 min
tictoc::tic()
se_boot_fit <- se_ready_for_boot %>%
    group_by(period, sex, id, name, sim_id) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = exposure, D = death_sim, std = logmx_std,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% as.vector(),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()
tictoc::toc()

# save to temp location
tictoc::tic()
save(se_boot_fit, file = "~/tmp-simulated/se_boot_fit.rda")
tictoc::toc()

# calculate simulated life tables ~  min
tictoc::tic()
se_boot_lt <- se_boot_fit %>%
    filter(sim_id < 201) %>%
    group_by(period, sex, id, name, sim_id) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()
tictoc::toc()

# save to temp location ~ 1 min
tictoc::tic()
save(se_boot_lt, file = "~/tmp-simulated/se_boot_lt.rda")
tictoc::toc()



# summarize simulated life tables with credible intervals
tictoc::tic()
se_mun_sum_ci <- se_boot_lt %>%
    group_by(period, sex, id, name, sim_id) %>%
    summarise(
        le = ex[1],
        q5065 = 1 - (lx[66] / lx[51])
    ) %>%
    group_by(period, sex, id, name) %>%
    summarise(
        le_mean = le %>% mean(na.rm = T),
        le_025 = le %>% quantile(.025),
        le_100 = le %>% quantile(.1),
        le_500 = le %>% quantile(.5),
        le_900 = le %>% quantile(.9),
        le_975 = le %>% quantile(.975),
        q5065_mean = q5065 %>% mean(na.rm = T),
        q5065_025 = q5065 %>% quantile(.025),
        q5065_100 = q5065 %>% quantile(.1),
        q5065_500 = q5065 %>% quantile(.5),
        q5065_900 = q5065 %>% quantile(.9),
        q5065_975 = q5065 %>% quantile(.975)
    ) %>%
    ungroup()
tictoc::toc()

# save the summary table to permanent location
save(se_mun_sum_ci, file = "dat/se_mun_sum_ci.rda")
