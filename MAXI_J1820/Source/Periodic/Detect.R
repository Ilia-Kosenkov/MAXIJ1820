ReadAllInput <- function(path = file.path("Data", "maxi1820")) {
    if (!dir.exists(path))
        stop("Directory not found.")

    ls <- dir(path, pattern = ".*csv", full.names = TRUE)

    files <- ls %>%
        file.info() %>%
        as.tibble %>%
        mutate(path = rownames(.)) %>%
        filter(size > 0) %>%
        mutate(band = str_match(path, "maxi(.)[0-9]{3}.csv")[, 2])

    foreach(b = Bands$Band,
        .final = function(x) setNames(x, Bands$Band)) %dopar% {
        files %>%
            filter(band == b) %>%
            pull(path) %>%
            map(read_csv, col_type = cols()) %>%
            map(~setNames(.x, c("JD", "Ref", "Obs")))
    }
}

ProcessData_1 <- function(data, band = "R") {

        foreach(x = data[[band]]) %dopar% {
            ProcessObservations2(data = x, Bands %>% filter(Band == "R"))
        }    %>%
        bind_rows %>%
        arrange(JD) %>%
        filter(Err < 0.1)
}

ProcessData_2 <- function(data, band = "R", by = 16) {
    
    foreach(x = data[[band]]) %dopar% {
        n <- floor(nrow(x) / by)
        res <- seq_int_len(n) %>% map(~ProcessObservations2(
                data = slice(x, 1:by + by * (.x - 1)),
                Bands %>% filter(Band == band))) %>%
            bind_rows

        if (n * by < nrow(x)) {
            lst <- ProcessObservations2(data = slice(x, (n*by + 1) : nrow(x)),
                Bands %>% filter(Band == band))
            res <- bind_rows(res, lst)
        }
        return(res)
    } %>%
    bind_rows
}

PlotData <- function(data, fold = 12.2,
    titles = list()) {

    foldInDays <- fold / 24.0

    lData <- data %>%
        mutate(FoldJD = (JD  / foldInDays) %% 1)

    list(
         pol =
            ggplot(data, aes(x = JD, y = P)) +
            DefaultTheme() +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_point() +
            geom_errorbar(aes(ymin = P - Err, ymax = P + Err)) +
            ggtitle(titles$P),
        ang = ggplot(data, aes(x = JD, y = PA)) +
            DefaultTheme() +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_point() +
            geom_errorbar(aes(ymin = PA - AErr, ymax = PA + AErr)) +
            ggtitle(titles$PA),
        polFold = ggplot(lData, aes(x = FoldJD, y = P)) +
            geom_point() +
            DefaultTheme() +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_errorbar(aes(ymin = P - Err, ymax = P + Err)) +
            ggtitle(titles$PFol))
}

PlotPeriodogram <- function(result, title = "LS") {
    w <- seq(1e-10, 4 * pi - 1e-10, length.out = 100)

    xBreaks2 <- c(6:21, 24, 28, 36, 48, 72, 96) / 24


    prdg <- result %>%
        mutate(P0 = P - mean(P)) %>%
        mutate(PA0 = PA - mean(PA)) %$%
        Scargle.Periodogram(w, JD, PA0)

    temp <- tibble(x = w / 2 / pi, y = prdg)

    ggplot(temp, aes(x, y)) +
    DefaultTheme() +
    geom_line() +
    geom_point() +
    scale_x_continuous(name = expression(nu),
                       sec.axis = sec_axis(~.,
                       name = expression(P * ", h"),
                       breaks = 1 / xBreaks2,
                       labels = xBreaks2 * 24)) +
    ylab("PSD") +
    theme(axis.text.x.top = element_text(vjust = 1,
            margin = margin(b = 0.6, unit = "npc")),
            plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
}

GenerateOuput <- function(data, band, avgBy, pFol) {

    dirPath <- file.path("Output", "Periods", band)
    if (!dir.exists(dirPath))
        dir.create(dirPath, recursive = TRUE)

    pathTemp <- file.path(dirPath, glue("{band}-by-{avgBy/4}"))
    pathDat <- pathTemp %+% ".dat"
    pathPlot <- pathTemp %+% ".tex"

    result <- ProcessData_2(data, band, by = avgBy) %>%
        filter(P < 1.2) %>%
        filter(Err < 0.4) %>%
        filter(AErr < 10)

    result %>%
        select(-Cov, -SGx, -SGy) %>%
        WriteFixed(pathDat,
                    frmt = c("%16.6f", rep("%12.6f", 4),
                            rep("%9.2f", 2), "%4d"))

    plots <- PlotData(result, pFol,
            title = list(
            P = glue("Polarization of {band}, averaged by {avgBy / 4} obs."),
            PA = glue("Angle of {band}, averaged by {avgBy / 4} obs."),
            PFol = glue("Pol of {band}, averaged by {avgBy / 4} obs. \\
                and folded with {pFol} h")))

    plots %<>% append(list(PlotPeriodogram(result,
        title = glue("LS-spectrum of pol. for {band} filter"))))

    tikz(pathPlot, width = 8, height = 6, standAlone = TRUE)
    tryCatch({
                plots %>%
                    GGPlot2Grob(innerMar =
                              margin(0.5, 1, 0.5, 1, unit = "cm")) %>%
                    GrobPlot
             },
             finally = dev.off())

    Tex2Pdf(pathPlot, verbose = FALSE)
}

if (IsRun()) {


    obs <<- ReadAllInput()

    #pol <<- ProcessData_2(obs, "R", by = 4)

    band <- "R"
    avgBy <- 4
    pFol <- 17

    #for (b in Bands$Band) {

        #cat(glue("Processing filter {b}:\n"))
        #from <- 2
        #to <- 45
        #bar <- txtProgressBar(from - 1, to, from - 1, style = 3, char = "|")
        #from:to %>%
            #walk(function(x) {
                #GenerateOuput(obs, b, x * 4, pFol)
                #setTxtProgressBar(bar, x)
            #})

        #close(bar)
    #}

    #GenerateOuput(obs, band, 50 * 4, pFol)

    #result <<- ProcessData_2(obs, band, by = avgBy) %>%
        #mutate(Err = 0.01)# %>%
        #filter(P < 1.2) %>%
        #filter(Err < 0.4) %>%
        #filter(AErr < 10) %T>% print(n = nrow(.))

    PlotData(result, pFol,
        title = list(
            P = glue("Polarization of {band}, averaged by {avgBy / 4} obs."),
            PA = glue("Angle of {band}, averaged by {avgBy / 4} obs."),
            PFol = glue("Pol of {band}, averaged by {avgBy / 4} obs. \\
                    and folded with {pFol} h"))) %>%
        GGPlot2Grob %>%
        GrobPlot

    PlotPeriodogram(result,
        title = glue("LS-spectrum of {band} filter")) %>%
        GGPlot2Grob(innerMar = margin(1, 1, 1, 1, unit = "cm")) %>%
        GrobPlot
}