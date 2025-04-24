#===============================================================================
# 2025-04-24 -- mun-non-surv
# spatial clusters life tables + bootstrap
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

source("src/0-prepare-session.R")



# calculate spatial clusters ----------------------------------------------


# load Danish data
load("dat/geodata-dk.rda")
load("dat/dk_mun_sum_ci.rda")
load("dat/dk_mun_size.rda")

dk_m2 <- dk_mun_sum_ci %>%
    filter(sex == "m", period == "2015-19") %>%
    select(-sex) %>%
    left_join(gd_dk_mun_s %>% select(-name), by = "id") %>%
    left_join(dk_mun_size %>% filter(sex == "b", period == "2015-19")%>%
                  select(-sex)) %>%
    mutate(pop_size = pop_size %>% divide_by(1e3)) %>%
    st_as_sf() %>%
    st_transform(crs = 3044)



# https://spatialanalysis.github.io/workshop-notes/spatial-clustering.html#spatial-clustering-in-rgeoda


data <- dk_m2 %>% select(le_mean) %>% st_drop_geometry()

knn_w <- knn_weights(dk_m2, k = 5)

clusters <- rgeoda::skater(
    6, knn_w, data,
    bound_variable = dk_m2 %>% select(pop_size) %>% st_drop_geometry,
    min_bound = 582 # 10% of the total DK pop
)
clusters

# arbitrary choice of parameters:
# knn k=5, skater k=6, min_bound = 582 -- 10% of the total DK pop

# save the final clusters
clusters_dk_m2 <- dk_m2 %>%
    transmute(
        id,
        cluster = clusters$Clusters %>% as_factor
    ) %>%
    st_drop_geometry()

save(dk_m2, clusters_dk_m2, file = "dat/clusters_dk_m2.rda")

{dk_m2 %>%
        left_join(clusters_dk_m2) %>%
        ggplot()+
        geom_sf(aes(fill = cluster), color = NA)+
        geom_sf(
            data = dk_cities,
            size = 4.5, shape = 1, stroke = 1, color = "#da26fa"
        )+
        scale_fill_viridis_d(option = "H", begin = .1, end = .9)+
        theme_map()} %>%
    inset_cph_box()


# cluster life tables -----------------------------------------------------

load("dat/clusters_dk_m2.rda")
load("dat/dk_fitted_std.rda")
load("dat/dk_mun_pooled_raw.rda")

load("dat/dk_mun_pooled_raw.rda")
load("dat/dk_fitted_std.rda")

cl_ready_for_topals <- dk_mun_pooled %>%
    mutate(age = age %>% as.integer()) %>%
    # only males and 2015-19
    filter(sex=="m", period == "2015-19") %>%
    right_join(clusters_dk_m2) %>%
    group_by(period, sex, cluster, age) %>%
    summarise(
        death = death %>% sum(na.rm = T),
        exposure = exposure %>% sum(na.rm = T)
    ) %>%
    ungroup() %>%
    left_join(dk_fitted_std)

# # these are tiny island communes
# id_tiny <- c("411", "492", "563", "741", "825")

# fit TOPALS to all municipalities
cl_fit <- cl_ready_for_topals %>%
    group_by(period, sex, cluster) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = exposure, D = death, std = logmx_std,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% as.vector(),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()


# calculate life tables and extract quantities like e0 and p5065

cl_mun_lt <- cl_fit %>%
    group_by(period, sex, cluster) %>%
    group_modify(~ lt(mx = .x$mx_fit, sex = "m")) %>%
    ungroup()

save(cl_mun_lt, file = "dat/cl_mun_lt.rda")


# bootstrap confidence intervals ------------------------------------------

cl_ready_for_boot <- cl_ready_for_topals %>%
    group_by(period, sex, cluster) %>%
    expand_grid(sim_id = 1:5e2) %>%
    group_by(period, sex, cluster, age) %>%
    mutate(death_sim = rpois(5e2, death)) %>%
    ungroup()

# fit TOPALS to the simulated death counts ~ 10 sec
tictoc::tic()
cl_boot_fit <- cl_ready_for_boot %>%
    group_by(period, sex, cluster, sim_id) %>%
    mutate(
        logmx_fit = TOPALS_fit(
            N = exposure, D = death_sim, std = logmx_std,
            age_group_bounds = 0:100, max_iter = 1e4, details = T
        ) %>% extract2("logm") %>% as.vector(),
        mx_fit = logmx_fit %>% exp
    ) %>%
    ungroup()
tictoc::toc()

# calculate simulated life tables ~ 4 sec
tictoc::tic()
cl_boot_lt <- cl_boot_fit %>%
    group_by(period, sex, cluster, sim_id) %>%
    group_modify(~ lt(mx = .x$mx_fit, a0 = .3)) %>%
    ungroup()
tictoc::toc()


# summarize simulated life tables with credible intervals
cl_lt_lx_ci <- cl_boot_lt %>%
    group_by(period, sex, cluster, age) %>%
    summarise(
        lx_mean = lx %>% mean(na.rm = T),
        lx_025 = lx %>% quantile(.025) %>% unname,
        lx_100 = lx %>% quantile(.1) %>% unname,
        lx_500 = lx %>% quantile(.5) %>% unname,
        lx_900 = lx %>% quantile(.9) %>% unname,
        lx_975 = lx %>% quantile(.975) %>% unname
    ) %>%
    ungroup()


# save the summary table to permanent location
save(cl_lt_lx_ci, file = "dat/cl_lt_lx_ci.rda")



