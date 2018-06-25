PlotField <- function(dat = fieldStars, x_sz = 5, y_sz = 5) {
    data <- fieldStars %>%
        mutate(Lab = NO - 700) %>%
        mutate(Lab = as.character(Lab)) %>%
        mutate(Lab = if_else(Lab == "-100", "MAXI", Lab)) %>%
        mutate(X = PX_D - RA_D[1]) %>%
        mutate(Y = PY_D + DEC_D[1])

    cntr <- data %>%
        filter(Lab == "MAXI") %>%
        select(RA_D, DEC_D) %>%
        as.numeric

    scl <- c(x_sz, y_sz) / 60.0

    xlim <- cntr[1] + 0.5 * c(-1, 1) * scl[1]
    ylim <- cntr[2] + 0.5 * c(-1, 1) * scl[2]

    img <- jpeg::readJPEG(file.path("Data", "ASASSN-18ey-BRIR5x5.jpg"))
    img2 <- array(0.5, dim = dim(img) + c(0, 0, 1))
    img2[, , 1:3] <- img

    (data %>%
        ggplot(aes(x = X, y = Y)) +
        DefaultTheme() +
        annotation_custom(rasterGrob(
                img2,
                width = unit(1, "npc"),
                height = unit(1, "npc"))) +
        geom_point() +
        geom_text(aes(label = Lab), nudge_y = 0.002) +
        scale_x_continuous(
            limits = -rev(xlim),
            expand = c(0, 0)) +
        scale_y_continuous(
            limits = ylim,
            expand = c(0, 0))) %>% list
}


if (IsRun()) {
    PlotField() %>%
        GGPlot2Grob %>%
        GrobPlot
        
}