EmptyLabels <- function(x) rep("", length(x))

PlotField <- function(dat = fieldStars,
                      x_sz = 5, y_sz = 5,
                      tckSz = 0.02,
                      isTex = FALSE,
                      decDig = 2) {

    fctr <- 1e-2
    data <- fieldStars %>%
        mutate(Lab = NO - 700) %>%
        mutate(Lab = as.character(Lab)) %>%
        mutate(Lab = if_else(Lab == "-100", "MAXI", Lab)) %>%
        mutate(X = PX_D - RA_D[1]) %>%
        mutate(Y = PY_D + DEC_D[1]) %>%
        mutate(XUpp = X + fctr * XPol, XLwr = X - fctr * XPol) %>%
        mutate(YUpp = Y + fctr * YPol, YLwr = Y - fctr * YPol)

    cntr <- data %>%
        filter(Lab == "MAXI") %>%
        select(RA_D, DEC_D) %>%
        as.numeric

    scl <- c(x_sz, y_sz) / 60.0

    xlim <- cntr[1] + 0.5 * c(-1, 1) * scl[1]
    ylim <- cntr[2] + 0.5 * c(-1, 1) * scl[2]

    img <- jpeg::readJPEG(file.path("Data", "ASASSN-18ey-BRIR5x5.jpg"))
    img2 <- array(0.35, dim = dim(img) + c(0, 0, 1))
    img2[, , 1:3] <- img

    xlab <- ifelse(isTex,
                   "\\alpha,~\\mathrm{deg}",
                   expression(alpha * ", deg"))
    ylab <- ifelse(isTex,
                   "\\delta,~\\mathrm{deg}",
                   expression(delta * ", deg"))

    frmt_1 <- sprintf("%%.%df", decDig)
    frmt_2 <- sprintf(".%%0%d.0f", decDig)


    xStep <- FancyStep(xlim)
    xBreaks <- GenerateBreaks(xlim, xStep, 0.1 * xStep) %>%
        map(multiply_by, -1)
    xLabs <- xBreaks %>%
        extract2("Large") %>%
        multiply_by(-1)
    ind <- length(xLabs)
    anch <- xLabs %>% extract(ind) %>% floor
    xLabs <- map2_dbl(xLabs, seq_len(length(xLabs)),
        ~ ifelse(.y == ind, .x, .x - anch)) %>%
        round(decDig) %>%
        map2(seq_len(length(.)), function(x, y) {
            if (y == ind)
                sprintf(frmt_1, x)
            else
                sprintf(frmt_2, x * 10 ^ decDig)
        }) %>% unlist

    yStep <- FancyStep(ylim)
    yBreaks <- GenerateBreaks(ylim, yStep, 0.1 * yStep)
    yLabs <- yBreaks$Large
    ind <- 1
    anch <- yLabs %>% extract(ind) %>% floor
    yLabs <- map2_dbl(yLabs, seq_len(length(yLabs)),
        ~ ifelse(.y == ind, .x, .x - anch)) %>%
        round(decDig) %>%
        map2(seq_len(length(.)), function(x, y) {
            if (y == ind)
                sprintf(frmt_1, x)
            else
                sprintf(frmt_2, x * 10 ^ decDig)
            }) %>% unlist

    plt <- data %>%
        ggplot(aes(x = X, y = Y)) +
        DefaultTheme() +
        annotation_custom(rasterGrob(
                img2,
                width = unit(1, "npc"),
                height = unit(1, "npc"))) +
        geom_point() +
        geom_segment(
            aes(x = XLwr, y = YLwr, xend = XUpp, yend = YUpp),
            size = 1) +
        geom_text(aes(label = Lab), nudge_y = 0.002) +
        scale_x_continuous(
            name = xlab,
            limits = -rev(xlim),
            breaks = xBreaks$Small,
            labels = EmptyLabels,
            expand = c(0, 0)) +
        scale_y_continuous(
            name = ylab,
            limits = ylim,
            breaks = yBreaks$Small,
            labels = EmptyLabels,
            expand = c(0, 0))

    plt <- plt %>%
        GGPlotCustomTicks("bot",
                          xBreaks$Large,
                          xLabs,
                          tckSz) %>%
        GGPlotCustomTicks("left",
                          yBreaks$Large,
                          yLabs,
                          tckSz)
    return(list(plt))
}


if (IsRun()) {
    PlotField() %>%
        GGPlot2Grob(innerMar =
            list(b = unit(1, "cm"),
                 l = unit(1, "cm"))) %>%
        GrobPlot
        
}