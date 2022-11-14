# a function to create inset zoom stamp for Copenhagen area

# wrap inset CPH map as a function
inset_cph_box <- function(map_gg, stamp_size = .37, x = .43, y = .50) {
    require(sf)
    require(cowplot)

    # the box
    chp_box <- st_buffer(st_point(c(715e3,6180e3)), dist = 25e3) %>%
        st_bbox() %>%
        st_as_sfc() %>%
        st_set_crs(3044)

    # add the box to the map
    map_box <- map_gg +
        geom_sf(data = chp_box, fill = NA, color = "grey25")

    # Zoom Copenhagen area
    map_zoom <- map_gg +
        coord_sf(xlim = c(690e3, 740e3), ylim = c(6155e3, 6205e3))+
        theme_void()+
        labs(title = NULL, subtitle = NULL, caption = NULL)+
        theme(
            legend.position = "none",
            panel.border = element_rect(color = "grey25", fill = NA, size = 1)
        )

    # compose map
    map_out <- ggdraw()+
        draw_plot(map_box)+
        draw_plot(map_zoom, x = x, y = y,
                  width = stamp_size, height = stamp_size)

    return(map_out)
}
