.Initialize <- function() {

    library(tidyverse)
    library(magrittr)
    library(rlang)
    library(jpeg)
    library(RColorBrewer)
    library(tikzDevice)
    library(grid)

    devtools::install_github("Ilia-Kosenkov/RLibs/package",
                             dependencies = FALSE)

    library(RLibs)

    if (!dir.exists(file.path("Source")))
        stop("No `Source` directory found.")

    files <- dir(file.path("Source"), pattern = "*.R", full.names = TRUE)
    files <- files[!grepl("initialize\\.r", files, TRUE)]
    for (f in files)
        source(f)
    }

.LoadData <- function() {
    if (!dir.exists(file.path("Data")))
        stop("No `Data` directory found.")

    starCoords <<- read.table(file.path("Data", "star_stats.dat"),
                        TRUE, stringsAsFactors = FALSE) %>%
                as.tibble

    starPol <<- read.table(file.path("Data", "star_pol.dat"),
        TRUE, stringsAsFactors = FALSE) %>%
        as.tibble

    Bands <<- read_table(file.path("Data", "Bands.dat"),
                        col_types = cols())

    starPol2 <<- read_delim(file.path("Data", "star_pol2.dat"),
                            delim = "\t", col_types = cols())

    averages <<- read.table(file.path("Data", "Averages.dat"),
            TRUE,
            stringsAsFactors = FALSE) %>%
        as.tibble
}

.PrepareData <- function(
    xrange = c(1, 600),
    yrange = c(1, 600),
    degSize = c(5, 5) / 60.0) {

    idDig <- 2

    IdConv <- function(x)
        x %>%
            divide_by(10 ^ idDig) %>%
            map_dbl(~.x - floor(.x)) %>%
            multiply_by(10 ^ idDig) %>%
            round %>%
            as.integer

    starCoords <<- starCoords %>%
        mutate(ID = IdConv(NO)) %>%
        mutate(RA_D = Ra2Degrees(RA)) %>%
        mutate(DEC_D = Dec2Degrees(DEC)) %>%
        mutate(PX_D = - 0.5 * degSize[1] +
            (PX - xrange[1]) / diff(xrange) * degSize[1]) %>%
        mutate(PY_D = - 0.5 * degSize[2] +
            (PY - yrange[1]) / diff(yrange) * degSize[2]) %>%
        mutate(Lab = if_else(ID == 00L, "MAXI", as.character(ID)))

    cols <- Bands %>%
        pull(Band) %>%
        map(~paste0(c("p", "a"), .x) %>% map(as.name))


    starPol2 <<- Bands %>%
        pull(Band) %>%
        map(~select(starPol2, ID, d, ends_with(.x))) %>%
        map2(cols, ~ rename(.x,
                !!"p" := !!.y[[1]], !!"pa" := !!.y[[2]])) %>%
        map2(Bands %>% pull(ID), ~ mutate(.x, FIL = .y)) %>%
        bind_rows %>%
        mutate(q = p * cos(pa / 90 * pi),
               u = p * sin(pa / 90 * pi)) %>%
        mutate(NO = 700L + ID)

    starPol <<- starPol %>%
        mutate(ID = if_else(NO < 700, 600L, NO)) %>%
        mutate(ID = IdConv(ID))
}

.Compile <- function() {
    CompileFortran(file.path("Source", "Fortran"))
}

IsRun <- function() {
    exists(".IsInitialized") && get0(".IsInitialized")
}

if (!exists(".IsInitialized") ||
    !(rlang::`%||%`(get0(".IsInitialized"), FALSE))) {
    .Initialize()
    .LoadData()
    .PrepareData()
    .Compile()

    .IsInitialized <<- TRUE
}
