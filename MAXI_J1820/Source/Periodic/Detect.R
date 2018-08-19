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
        res <- 1:n %>% map(~ProcessObservations2(data = slice(x, 1:by + by * (.x - 1)),
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

PlotData <- function(data) {

    list(pol =
        ggplot(data, aes(x = JD, y = P)) +
        geom_point() +
        geom_line() +
        geom_errorbar(aes(ymin = P - Err, ymax = P + Err)),
    ang = ggplot(data, aes(x = JD, y = PA)) +
        geom_point() +
        geom_line() +
        geom_errorbar(aes(ymin = PA - AErr, ymax = PA + AErr)))
}

PlotPeriodogram <- function(result) {
    w <- seq(1e-10, 4 * pi - 1e-10, length.out = 100)

    xBreaks2 <- c(6:22, 24, 28, 36, 48, 72, 96) / 24


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
                       name = expression(P ~ ", h"),
                       breaks = 1 / xBreaks2,
                       labels = xBreaks2 * 24
                       ))
}

if (IsRun()) {


    #obs <<- ReadAllInput()

    result <<- ProcessData_2(obs, by = 12) %>%
        filter(P < 1.5) %>%
        filter(Err < 0.4 || is.infinite(Err)) %>%
        filter(AErr < 20 || is.infinite(AErr)) %T>% print(n = nrow(.))

    PlotData(result) %>%
        GGPlot2Grob %>%
        GrobPlot

    PlotPeriodogram(result) %>%
        GGPlot2Grob %>%
        GrobPlot
}