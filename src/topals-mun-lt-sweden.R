#===============================================================================
# 2020-10-29 -- indexation
# Calculate Swedish municipal life tables useing TOPALS
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

source("src/prepare-session.R")


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

load("dat/sw-pooled_5y.rda")

sw_df <- pooled_5y %>%
    mutate(age = age %>% as.integer()) %>%
    left_join(
        std_5y %>% transmute(period, sex, age, logmx_smth)
    )

# # these are tiny island communes
# id_tiny <- c("411", "492", "563", "741", "825")

# fit TOPALS to all municipalities
sw_df_fit <- sw_df %>%
    group_by(period, sex, id, name) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = exposure, D = death, std = logmx_smth,
            age_group_bounds = 0:101, max_iter = 1e4, details = T
        ) %>% extract2("logm"),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()

save(sw_df_fit, file = "dat/sw-topals-pooled-5y.rda")

# calculate life expectancy
sw_df_le <- sw_df_fit %>%
    group_by(period, sex, id, name) %>%
    summarise(life_exp = logmx_fit %>% e0) %>%
    ungroup()

# attach population size
load("dat/sw-death_rates.rda")
sw_pop_size_19 <- sw_death_rates %>%
    filter(year == "2019") %>%
    group_by(name) %>%
    summarise(e = exposure %>% sum %>% divide_by(1e3) %>% round(2))

save(sw_pop_size_19, file = "dat/sw-pop_size_19.rda")

# join population size
sw_df_le_comapre <- sw_df_le %>%
    left_join(sw_pop_size_19)

save(sw_df_le_comapre, file = "dat/sw-df_le_comapre.rda")
