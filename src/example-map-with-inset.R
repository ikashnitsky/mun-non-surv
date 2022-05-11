#===============================================================================
# 2022-05-11 -- mun le
# A minimal example of building a map of Danish regions
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com, @ikashnitsky
#===============================================================================

source("src/prepare-session.R")
source("src/fun-inset-cph-box.R") # this one only adds an inset showing CPH area
# note: this function is hardcoded to the geographical coordinates of CPH

load("dat/geodata-dk.rda")
load("dat/death_rates.rda")
pop_size_19 <- death_rates %>%
    filter(year == "2019") %>%
    group_by(name, id) %>%
    summarise(e = exposure %>% sum %>% divide_by(1e3) %>% round(2))

df_map <-  gd_dk_mun_s %>%
    left_join(pop_size_19, by = "id")

# DK mun -- life exp via TOPALS
df_map %>%
    ggplot()+
    geom_sf(aes(fill = e, geometry = geometry), color = NA)+
    # geom_sf(data = bord, color = "#ffffff", size = .5)+
    geom_sf(data = dk_cities,
            size = 4.5, shape = 1, stroke = .5, color = "#ffffff")+
    scale_fill_fermenter(
        palette = "YlGnBu", direction = 1, breaks = c(25, 50, 100)
    )+
    # scale_fill_manual(values = pal.25, guide = guide_legend(ncol = 1))+
    cowplot::theme_map(font_family = font_rc, font_size = 16)+
    theme(legend.position = c(1,  .9),
          legend.justification = c(1, 1),
          legend.background = element_blank(),
          plot.title = element_text(family = "Roboto Slab",
                                    face = 2, size = 20),
          panel.spacing = unit(0, "lines"))+
    labs(title = "Population size of Danish municipalities",
         subtitle = "in thousands, 2019",
         caption = "Data: DST | Design: Ilya Kashnitsky @ikashnitsky",
         fill = NULL)

map_pop <- last_plot()

map_pop_zoom <- map_pop %>% inset_cph_box()

ggsave("fig/map-mun-pop-sizes.pdf", map_pop_zoom, device = cairo_pdf, bg = "#ffffff",
       width = 6, height = 6)
