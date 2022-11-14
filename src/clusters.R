#===============================================================================
# 2022-11-04 -- non-surv
# Spatial clusters
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

source("src/prepare-session.R")
# library(spdep)

source("src/fun-inset-cph-box.R")

# load Danish data
load("dat/geodata-dk.rda")
load("dat/df_le_comapre.rda")

dk <- df_le_comapre %>%
    left_join(gd_dk_mun_s %>% select(-name), by = "id") %>%
    st_as_sf() %>%
    st_transform(crs = 3044)


dk_m2 <- dk %>% filter(sex == "m", period == "2015-2019")

{
    dk_m2 %>%
        ggplot()+
        geom_sf(aes(fill = life_exp), color = NA)+
        scale_fill_viridis_b(breaks = seq(77, 82, 1), option = "H")+
        # scale_fill_paletteer_binned(
        #     `"scico::lajolla"`,
        #     breaks = seq(77, 82, 1), direction = -1
        # )+
        theme_map()+
        labs(fill = "e0")
} %>%
    inset_cph_box()

map_e0m <- last_plot()


foo <- dk %>% filter(period == "2015-2019", !sex == "b") %>%
    pivot_wider(names_from = sex, values_from = life_exp)


# https://spatialanalysis.github.io/workshop-notes/spatial-clustering.html#spatial-clustering-in-rgeoda
library(rgeoda)

data <- dk_m2 %>% select(life_exp) %>% st_drop_geometry()
# data <- foo %>% select(f, m)
knn_w <- knn_weights(dk_m2, k = 5)

clusters <- rgeoda::skater(
    6, knn_w, data,
    bound_variable = dk_m2 %>% select(e) %>% st_drop_geometry,
    min_bound = 582 # 10% of the total DK pop
)
clusters

# arbitrary choice of parameters:
# knn k=5, skater k=6, min_bound = 582 -- 10% of the total DK pop

# save the final clusters
clusters_dk_m2 <- dk_m2 %>%
    transmute(
        id,
        cluster = clusters$Clusters %>% paste
    ) %>%
    st_drop_geometry()

save(clusters_dk_m2, file = "dat/clusters_dk_m2.rda")

{dk_m2 %>%
    mutate(cluster = clusters$Clusters %>% as_factor) %>%
    ggplot()+
    geom_sf(aes(fill = cluster), color = NA)+
    scale_fill_viridis_d()+
    theme_map()} %>%
    inset_cph_box()

# descriptive summary stat of clusters
tab <- dk_m2 %>%
    st_drop_geometry() %>%
    mutate(cluster = clusters$Clusters %>% as_factor) %>%
    group_by(cluster) %>%
    summarise(
        w_mean = matrixStats::weightedMean(life_exp, e),
        w_sd = matrixStats::weightedSd(life_exp, e),
        n = n(),
        e = sum(e),
        mean = mean(life_exp),
        sd = sd(life_exp)
    )

# assemble the summary table as legend

(
    legend_table <- ggplot(data = tibble(1:6), aes(y = seq(11,1,-2)))+
        coord_cartesian(xlim = c(0,15), ylim = c(0,14))+
        theme_void()+
        geom_point(
            shape = 15, x = 1.5, size = 7,
            color = viridis::viridis(6)
        )+
        geom_text(x = 3, label = tab$cluster, hjust = 1)+
        geom_text(x = 5, label = tab$n, hjust = 1)+
        geom_text(x = 8, label = tab$e %>% round, hjust = 1)+
        geom_text(x = 11, label = tab$w_mean %>% round(1), hjust = 1)+
        geom_text(x = 14, label = tab$w_sd %>% round(2), hjust = 1)+
        annotate("text",
            x = c(3, 5, 8, 11, 14), y = 13.4, hjust = 1,
            label = c("Cluster", "N", "Pop", "e0", "SD"),
            fontface = 2
        )
)

# the map without CPH inset
gg <- dk_m2 %>%
    mutate(cluster = clusters$Clusters %>% as_factor) %>%
    ggplot()+
    geom_sf(aes(fill = cluster), color = NA)+
    scale_fill_viridis_d()+
    theme_map()+
    theme(
        legend.position = "none"
    )+
    # add cluster numbers on map
    geom_sf_label(
        data = . %>%
            # filter(!id == "400") %>%  # get rid of Bornholm
            rmapshaper::ms_dissolve("cluster") %>%
            st_centroid(),
        aes(label = cluster),
        color = "#dadada", fill = "#2a2a2a", alpha = .5,
        size = 5, fontface = 2, nudge_y = 15e3
    )

save(gg, file = "fig/cluster-map-gg.rda")


# assemble the final output
(
    out <- ggdraw(gg) +
        draw_plot(
            legend_table, x = .5, y = .6, width = .45, height = .35
        )
)

ggsave("fig/map-clusters.pdf", out,
       width = 6, height = 5)

# Show 4 clusters
clusters <- rgeoda::skater(
    4, knn_w, data,
    bound_variable = dk_m2 %>% select(e) %>% st_drop_geometry,
    min_bound = 582 # 10% of the total DK pop
)
clusters

dk_m2 %>%
    mutate(clusters = clusters$Clusters %>% as_factor) %>%
    ggplot()+
    geom_sf(aes(fill = clusters), color = NA)+
    scale_fill_viridis_d()+
    theme_map()+
    theme(legend.position = "none")

ggsave("fig/map-clusters-4.pdf",
       width = 6, height = 5)


# show kmeans solution with 6 clusters
km_clusters <- kmeans(data, 6, nstart = 100)


dk_m2 %>%
    mutate(clusters = km_clusters$cluster %>% as_factor) %>%
    ggplot()+
    geom_sf(aes(fill = clusters), color = NA)+
    scale_fill_viridis_d()+
    theme_map()+
    theme(legend.position = "none")

ggsave("fig/map-clusters-kmeans-6.pdf",
       width = 6, height = 5)


# choose the # of clusters -------------------------------------------------

# data <- dk_m2 %>% select(life_exp) %>% st_drop_geometry()
# knn_w <- knn_weights(dk_m2, k = 5)
#
# custom_skater <- function(k = 6, knn = 5, min_pop = 582){
#     suppressWarnings(
#         rgeoda::skater(
#             k,
#             knn_weights(dk_m2, k = knn),
#             data,
#             bound_variable = dk_m2 %>% select(e) %>% st_drop_geometry,
#             min_bound = min_pop # 10% of the total DK pop
#         )
#     )  %>%
#         extract2(5)
# }
#
# foo <- 2:10 %>%
#     map_dbl(custom_skater) %>%
#     tibble(x = 2:10, ratio_ss = .)
#
# foo %>%
#     ggplot(aes(x, ratio_ss))+
#     geom_point()
#
#
# # test ratio_ss with varying knn
# custom_skater_knn <- function(knn){
#     suppressWarnings(
#         rgeoda::skater(
#             6,
#             knn_weights(dk_m2, k = knn),
#             data,
#             bound_variable = dk_m2 %>% select(e) %>% st_drop_geometry,
#             min_bound = 582 # 10% of the total DK pop
#         )
#     )  %>%
#         extract2(5)
# }
#
# 2:10 %>%
#     map_dbl(custom_skater_knn) %>%
#     tibble(x = 2:10, ratio_ss = .) %>%
#     ggplot(aes(x, ratio_ss))+
#     geom_point()+
#     coord_cartesian(ylim = c(0, NA))



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
    wss <- cl %>% extract2(3)
    pseudo_f <- {gss / (k-1)} / {wss / (n-k)}
    return(pseudo_f)
}

clusters %>% ch_pseudo_f()

# common settings for exploratory plots
plot_cluster_choice <- function(clusters_df) {
    clusters_df %>%
        ggplot(aes(x, y))+
        geom_hline(yintercept = 0, size = 3/4, color = "#7a7a7a")+
        geom_path(size = 3/4, color = "#7a7a7a")+
        geom_point(size = 2)+
        coord_cartesian(ylim = c(0, NA))+
        scale_x_continuous(breaks = 2:9)+
        theme_minimal()+
        theme(
            panel.grid.minor = element_blank()
        )
}


# test number of clusters
my_skater_ch <- function(k){
    suppressWarnings(
        rgeoda::skater(
            k, knn_w, data,
            bound_variable = dk_m2 %>% select(e) %>% st_drop_geometry,
            min_bound = 582 # 10% of the total DK pop
        )
    )  %>%
        ch_pseudo_f()
}



2:9 %>%
    map_dbl(my_skater_ch)%>%
    tibble(x = 2:9, y = .) %>%
    plot_cluster_choice()+
    labs(
        x = "Number of clusters",
        y = NULL,
        title = "Calinski-Harabasz pseudo F-statistic, 5 nearest neighbours"
    )

ggsave("fig/cluster-ch-5knn.pdf",
       width = 5.2, height = 3)





# test number of knn
my_skater_ch_knn <- function(knn){
    suppressWarnings(
        rgeoda::skater(
            6,
            knn_weights(dk_m2, k = knn),
            data,
            bound_variable = dk_m2 %>% select(e) %>% st_drop_geometry,
            min_bound = 582 # 10% of the total DK pop
        )
    )  %>%
        ch_pseudo_f()
}


2:9 %>%
    map_dbl(my_skater_ch_knn)%>%
    tibble(x = 2:9, y = .) %>%
    plot_cluster_choice()+
    labs(
        x = "Number of nearest neighbours",
        y = NULL,
        title = "Calinski-Harabasz pseudo F-statistic, 6 clusters"
    )

ggsave("fig/cluster-ch-5k.pdf",
       width = 5.2, height = 3)
