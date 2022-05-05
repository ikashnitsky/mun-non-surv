#===============================================================================
# 2020-06-09 -- rockwool
# compare DK and SE from HMD
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


source("R/prepare-session.R")

# read in HDM
load("data/lt1x1.rda")

surv_dkse <- lt1x1 %>% 
    filter(country %>% is_in(c("DNK", "SWE")),
           year == 2016)

pal <- c("#df356b", "#eec21f")

surv_dkse %>% 
    filter(sex=="m") %>% 
    ggplot(aes(age, lx, color = country))+
    # geom_rect(aes(xmin = 50, xmax = 65, ymin = -Inf, ymax = Inf),
    #           fill = "#222222", color = NA, alpha = .1)+
    geom_path(size = 1)+
    scale_color_manual(values = pal, guide = NULL)+
    scale_y_comma()+
    dark_theme_minimal(base_family = font_rc, base_size = 16)+
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#000000", color = NA))

gg_bare <- last_plot()

gg_bare +
    annotate("text", label = c("Denmark", "Sweden"),
             x = 65, y = c(75e3, 95e3), hjust = 1:0,
             size = 5, color = pal, family = font_rc)+
    labs(title = "Survival of males",
         subtitle = "Denmark and Sweden, 2016, HMD",
         y = "Period life table survival, lx", 
         x = "Age")

gg_labs <-  last_plot()

ggsave("figures/surv-hmd-dk-se-males.pdf", gg_labs, width = 7, height = 5)
    

(
    out <-  ggdraw(gg_labs)+
        draw_plot(
            gg_bare +
                coord_cartesian(xlim = c(50, 65), ylim = c(85e3, 100e3))+
                geom_hline(data = . %>% filter(age %in% c(50, 65)),
                           aes(yintercept = lx, color = country))+
                geom_text(data = . %>% filter(age==65),
                           aes(label = lx %>% round(-2) %>% 
                                   divide_by(1e3) %>% paste0("k"), 
                               y = lx + 500, color = country),
                          x = 52.5, hjust = .5, vjust = 0, 
                          size = 5, family = font_rc)+
                geom_text(data = . %>% filter(age==50),
                          aes(label = lx %>% round(-2) %>% 
                                  divide_by(1e3) %>% paste0("k"), 
                              y = lx, color = country),
                          x = 62.5, hjust = .5, vjust = c(1.2, -.2), 
                          size = 5, family = font_rc)+
                scale_y_comma(breaks = seq(85e3, 100e3, 5e3))+
                labs(x = NULL, y = NULL)+
                theme(
                    plot.background = element_rect(fill = "#000000", 
                                                   color = "#ffffff")
                ), 
            x = .2, y = .15, width = .5, height = .45
        )
)

ggsave("figures/surv-hmd-dk-se-males-ann.pdf", out, width = 7, height = 5)
