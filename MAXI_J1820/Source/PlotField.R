EmptyLabels <- function(x) rep("", length(x))

PlotField <- function(pol, avg,
                      coord = starCoords,
                      x_sz = 5, y_sz = 5,
                      bandInfo = Bands %>% slice(1),
                      tckSz = 0.035,
                      image = FALSE,
                      isTex = FALSE,
                      decDig = 2) {
    cols <- c("#000000", brewer.pal(6, "Paired")[c(6, 2)])
    pchs <- c(19, 15, 15)
    ltys <- c(5, 1, 1)
    szs <- c(1.75, 2.25, 2.25)
    fctr <- 1.2e-2

    avgData <- avg %>%
        filter(FIL == bandInfo %>% pull(ID)) %>%
        inner_join(coord, by = c("AvgID" = "NO"),
            suffix = c("", ".other")) %>%
        mutate(Group = 1:n()) %>%
        select(PX_D, PY_D, RA_D, DEC_D, A, P, Group) %>%
        arrange(desc(Group))

    avgRA <- avgData %>% pull(RA_D) %>% mean
    avgDEC <- avgData %>% pull(DEC_D) %>% mean

    avgData %<>%
        select(-RA_D, - DEC_D) %>%
        mutate(Lab = "")

    data <- pol %>%
        filter(FIL == bandInfo %>% pull(ID)) %>%
        left_join(coord, by = c("ID" = "NO"),
            suffix = c("", ".other")) %>%
        mutate(Group = 0) %>%
        mutate(Lab = as.character(ID.other)) %>%
        select(PX_D, PY_D, A, P, Group, Lab) %>%
        bind_rows(avgData) %>%
        mutate(X = PX_D - avgRA) %>%
        mutate(Y = PY_D + avgDEC) %>%
        mutate(PltAng = (A + 90) / 180 * pi) %>%
        mutate(XUpp = X + fctr * P * cos(PltAng)) %>%
        mutate(XLwr = X - fctr * P * cos(PltAng)) %>%
        mutate(YUpp = Y + fctr * P * sin(PltAng)) %>%
        mutate(YLwr = Y - fctr * P * sin(PltAng)) %>%
        mutate(Group = as.factor(Group)) %>%
        select(X, Y, Group, Lab,
            XLwr, YLwr, XUpp, YUpp)

    scl <- c(x_sz, y_sz) / 60.0

    xlim <- avgRA + 0.5 * c(-1, 1) * scl[1]
    ylim <- avgDEC + 0.5 * c(-1, 1) * scl[2]

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
        geom_point(aes(shape = Group, size = Group)) +
        geom_segment(
            aes(x = XLwr, y = YLwr, xend = XUpp, yend = YUpp,
                linetype = Group), size = 1.1) +
        geom_text(aes(X, Y, label = Lab),
            nudge_x = scl[1] * 0.025,
            nudge_y = scl[2] * 0.02) +
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
                                    fontsize = 15)) +
        geom_point(data = data %>% slice(n() - 1:0))

    x0 <- xlim[2] - 0.05 * diff(xlim)
    y0 <- ylim[1] + 0.10 * diff(ylim)

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

PlotWorker <- function(data, avg, bandInfo, isTex, prefix = "") {
    sz <- 0.75
    szSmall <- 0.15
    plt <-
        PlotField(pol = data, avg = avg,
            isTex = isTex, image = TRUE,
            bandInfo = bandInfo) %>%
        GGPlot2Grob(innerMar =
            list(b = unit(sz, "cm"),
                 l = unit(sz, "cm"),
                 t = unit(szSmall, "cm"),
                 r = unit(szSmall, "cm")))


    if (isTex) {
        if (!dir.exists(file.path("Output", "Plots")))
            dir.create(file.path("Output", "Plots"), recursive = TRUE)

        pth <- file.path("Output", "Plots",
            sprintf("%sfield_%s",
                ifelse(nzchar(prefix), paste0(prefix, "_"), ""),
                bandInfo %>% pull(Band)))

        pth_tex <- paste0(pth, ".tex")
        pth_pdf <- paste0(pth, ".pdf")

        tikz(pth_tex,
            width = 3.5, height = 3.5,
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

    isTex <- TRUE
    bandInfo <- Bands

    avgData <- CombineResults() %>%
        mutate(AvgID = 10L * as.integer(ID / 10))
    starData <- CombineResults(pattern = "field_.\\.txt") %>%
        select(-Type)

    starData %>% {
            map(pull(bandInfo, Band),
                function(x) filter(., Band == x))
        }  %>%
        walk2(seq.int(length.out = nrow(bandInfo)),
            ~ PlotWorker(.x, 
                    avgData %>%
                        filter(Band == extract2(bandInfo, .y, "Band")),
                bandInfo %>% slice(.y), isTex, "comb"))

    starData %>% {
            map(pull(bandInfo, Band),
                function(x) filter(., Band == x))
        }  %>%
        walk2(seq.int(length.out = nrow(bandInfo)),
            ~ PlotWorker(.x, 
                    avgData %>%
                        filter(Band == extract2(bandInfo, .y, "Band")) %>%
                        filter(Type == "before"),
                bandInfo %>% slice(.y), isTex, "before"))

    starData %>% {
            map(pull(bandInfo, Band),
                function(x) filter(., Band == x))
        }  %>%
        walk2(seq.int(length.out = nrow(bandInfo)),
            ~ PlotWorker(.x, 
                    avgData %>%
                        filter(Band == extract2(bandInfo, .y, "Band")) %>%
                        filter(Type == "after"),
                bandInfo %>% slice(.y), isTex, "after"))
}