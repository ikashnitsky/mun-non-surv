#===============================================================================
# 2022-10-12 -- non-surv
# map both Denmark and Sweden
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

# load Swedish municipal geodata from previous project
# TO DO: find reproducible download
sw <- read_sf("~/SYNC/PhD/SWEDEN/adm/municipalities.laea.shp")

sw %>%
    ggplot()+
    geom_sf()


load("dat/sw-df_le_comapre.rda")

load("dat/sw-exposures.rda")


sw %>%
    left_join(sw_df_le_comapre, c("KNKOD" = "id")) %>%
    filter(sex == "m", period == "2015-2019") %>%
    ggplot()+
    geom_sf(aes(fill = life_exp), color = NA)+
    scale_fill_viridis_b()





load("dat/geodata-dk.rda")
load("dat/df_le_comapre.rda")

dk <- df_le_comapre %>%
    left_join(gd_dk_mun_s %>% select(-name), by = "id") %>%
    st_as_sf() %>%
    st_transform(crs = 3044)

# 2015-19, by sex
dk %>%
    filter(!sex=="b", period == "2015-2019") %>%
    ungroup() %>%
    mutate(sex = sex %>% lvls_revalue(c("FEMALE", "MALE"))) %>%
    ggplot()+
    geom_sf(aes(fill = life_exp, geometry = geometry), color = NA)+
    geom_sf(dat = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(palette = "YlGnBu", direction = 1)+
    facet_grid(~sex)+
    cowplot::theme_map(font_family = font_rc, font_size = 16)+
    theme(legend.position = c(1, .9),
          legend.justification = c(1, 1),
          legend.background = element_blank(),
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Life expectancy at birth",
         subtitle = "TOPALS model, pooled data 2015-19",
         caption = "data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)



sw %>%
    ggplot()+
    geom_sf()+
    geom_sf(data = dk)
