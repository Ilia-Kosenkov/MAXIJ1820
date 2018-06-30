EmptyLabels <- function(x) rep("", length(x))

PlotField <- function(coord = starCoords,
                      pol = starPol,
                      x_sz = 5, y_sz = 5,
                      bandInfo = Bands %>% slice(1),
                      tckSz = 0.02,
                      image = FALSE,
                      isTex = FALSE,
                      decDig = 2) {
    cols <- c("#000000", brewer.pal(6, "Paired")[6])
    pchs <- c(19, 15)
    ltys <- c(2, 1)
    szs <- c(2, 3)
    fctr <- 1.2e-2

    data <- pol %>%
        filter(FIL == bandInfo %>% pull(ID)) %>%
        right_join(starCoords, by = "ID", suffix = c("", ".other")) %>%
        mutate(FieldLab = if_else(Lab == "MAXI", "", Lab)) %>%
        mutate(MAXILab = if_else(Lab == "MAXI", "MAXI", "")) %>%
        mutate(Group = if_else(Lab == "MAXI", 2, 1)) %>%
        mutate(Group = as.factor(Group)) %>%
        mutate(X = PX_D - RA_D[1]) %>%
        mutate(Y = PY_D + DEC_D[1]) %>%
        #mutate(XUpp = X + fctr * q, XLwr = X - fctr * q) %>%
        #mutate(YUpp = Y + fctr * u, YLwr = Y - fctr * u) %>%
        mutate(PltAng = (pa + 90) / 180 * pi) %>%
        mutate(XUpp = X + fctr * p * cos(PltAng)) %>%
        mutate(XLwr = X - fctr * p * cos(PltAng)) %>%
        mutate(YUpp = Y + fctr * p * sin(PltAng)) %>%
        mutate(YLwr = Y - fctr * p * sin(PltAng)) %>%
        filter(row_number() == 1 | ID > 0)
    #data <- fieldStars %>%
        #mutate(Lab = NO - 700) %>%
        #mutate(Lab = as.character(Lab)) %>%
        #mutate(Lab = if_else(Lab == "-100", "MAXI", Lab)) %>%
        #mutate(FieldLab = if_else(Lab == "MAXI", "", Lab)) %>%
        #mutate(MAXILab = if_else(Lab == "MAXI", "MAXI", "")) %>%
        #mutate(Group = if_else(Lab == "MAXI", 2, 1)) %>%
        #mutate(Group = as.factor(Group)) %>%
        #mutate(X = PX_D - RA_D[1]) %>%
        #mutate(Y = PY_D + DEC_D[1]) %>%
        #mutate(XUpp = X + fctr * XPol, XLwr = X - fctr * XPol) %>%
        #mutate(YUpp = Y + fctr * YPol, YLwr = Y - fctr * YPol) %>%
        #slice(c(2:n(), 1))

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
                   "$\\alpha,~\\mathrm{deg}$",
                   expression(alpha * ", deg"))
    ylab <- ifelse(isTex,
                   "$\\delta,~\\mathrm{deg}$",
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
        ggplot(aes(x = X, y = Y, col = Group)) +
        DefaultTheme() + {
            if (image)
                annotation_custom(rasterGrob(
                        img2,
                        width = unit(1, "npc"),
                        height = unit(1, "npc")))
            else
                list()
            } +
        geom_segment(
            aes(x = XLwr, y = YLwr, xend = XUpp, yend = YUpp,
                linetype = Group), size = 1.1) +
        geom_point(aes(shape = Group, size = Group)) +
        geom_text(aes(label = FieldLab), nudge_y = 0.001, nudge_x = 0.002) +
        geom_text(aes(label = MAXILab), nudge_y = -0.001, nudge_x = - 0.006,
            size = 4.5) +
        scale_x_continuous(
            name = xlab,
            limits = -rev(xlim),
            breaks = xBreaks$Small,
            labels = EmptyLabels,
            expand = c(0, 0),
            sec.axis = sec_axis(~.,
                breaks = xBreaks$Small,
                labels = EmptyLabels)) +
        scale_y_continuous(
            name = ylab,
            limits = ylim,
            breaks = yBreaks$Small,
            labels = EmptyLabels,
            expand = c(0, 0),
            sec.axis = sec_axis(~.,
                breaks = yBreaks$Small,
                labels = EmptyLabels)) +
        scale_color_manual(
            breaks = c(1, 2),
            values = cols,
            guide = FALSE) +
        scale_shape_manual(
            breaks = c(1, 2),
            values = pchs,
            guide = FALSE) +
        scale_size_manual(
            breaks = c(1, 2),
            values = szs,
            guide = FALSE) +
        scale_linetype_manual(
            breaks = c(1, 2),
            values = ltys,
            guide = FALSE) +
        GGCustomTextAnnotation(bandInfo %>% pull(Band),
                               x = -Inf, y = Inf,
                               hjust = -2, vjust = 2,
                               gp = gpar(
                                    fontface = "italic",
                                    fontsize = 15))

    x0 <- xlim[2] - 0.05 * diff(xlim)
    y0 <- ylim[1] + 0.05 * diff(ylim)

    scl <- tibble(x = -x0, xend = -x0 + 1 * fctr,
                  y = y0, yend = y0)

    plt <- plt +
        geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
            data = scl, inherit.aes = FALSE, size = 1.5) +
        GGCustomTextAnnotation(ifelse(isTex,
                                "$p = 0.5 \\%$",
                                expression(italic(p) * " = " * 0.5 * "%")),
                               x = -x0 + 1 * fctr + 0.025 * diff(xlim),
                               y = y0,
                               vjust = 0.35)

    plt <- plt %>%
        GGPlotCustomTicks("bot",
                          xBreaks$Large,
                          xLabs,
                          tckSz) %>%
        GGPlotCustomTicks("top",
                          xBreaks$Large,
                          rep("", length(xLabs)),
                          tckSz) %>%
        GGPlotCustomTicks("left",
                          yBreaks$Large,
                          yLabs,
                          tckSz) %>%
        GGPlotCustomTicks("right",
                          yBreaks$Large,
                          rep("", length(yLabs)),
                          tckSz)
    return(list(plt))
}

PlotWorker <- function(bandInfo, isTex) {
    plt <-
        PlotField(isTex = isTex, image = TRUE,
            bandInfo = bandInfo) %>%
        GGPlot2Grob(innerMar =
            list(b = unit(1, "cm"),
                 l = unit(1, "cm"),
                 t = unit(1, "cm"),
                 r = unit(1, "cm")))


    if (isTex) {
        if (!dir.exists(file.path("Output", "Plots")))
            dir.create(file.path("Output", "Plots"), recursive = TRUE)

        pth <- file.path("Output", "Plots",
            sprintf("field_%s", bandInfo %>% pull(Band)))

        pth_tex <- paste0(pth, ".tex")
        pth_pdf <- paste0(pth, ".pdf")

        tikz(pth_tex,
            width = 5.5, height = 5.5,
            standAlone = TRUE)
        tryCatch({
        GrobPlot(plt)
    },
            finally = dev.off())

        Tex2Pdf(pth_tex, verbose = TRUE)
        Pdf2Eps(pth_pdf)
        Pdf2Eps(pth_pdf, "-eps", "eps")
    }
    else {
        GrobPlot(plt)
    }
}

if (IsRun()) {
    1:nrow(Bands) %>% walk(~PlotWorker(Bands %>% slice(.x), TRUE))
}