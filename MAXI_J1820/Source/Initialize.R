.Initialize <- function() {

    library(tidyverse)
    library(magrittr)
    library(RLibs)
    library(rlang)
    library(jpeg)
    library(RColorBrewer)
    library(tikzDevice)

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

    idDig <- 2

    IdConv <- function(x)
        x %>%
            divide_by(10 ^ idDig) %>%
            map_dbl(~.x - floor(.x)) %>%
            multiply_by(10 ^ idDig) %>%
            round %>%
            as.integer

    starCoords <<- read.table(file.path("Data", "star_stats.dat"),
                        TRUE, stringsAsFactors = FALSE) %>%
                as.tibble %>%
                mutate(ID = IdConv(NO))

    starPol <<- read.table(file.path("Data", "star_pol.dat"),
        TRUE, stringsAsFactors = FALSE) %>%
        as.tibble %>%
        mutate(ID = if_else(NO < 700, 600L, NO)) %>%
        mutate(ID = IdConv(ID))

    Bands <<- read_table(file.path("Data", "Bands.dat"),
                        col_types = cols())
}

.PrepareData <- function(
    xrange = c(1, 600),
    yrange = c(1, 600),
    degSize = c(5, 5) / 60.0) {
    starCoords <<- starCoords %>%
        mutate(RA_D = Ra2Degrees(RA)) %>%
        mutate(DEC_D = Dec2Degrees(DEC)) %>%
        mutate(PX_D = - 0.5 * degSize[1] +
            (PX - xrange[1]) / diff(xrange) * degSize[1]) %>%
        mutate(PY_D = - 0.5 * degSize[2] +
            (PY - yrange[1]) / diff(yrange) * degSize[2])
    starPol <<- starPol %>%
        mutate(XPol = p * cos(2 * pa / 180 * pi)) %>%
        mutate(YPol = p * sin(2 * pa / 180 * pi))

}

IsRun <- function() {
    exists(".IsInitialized") && get0(".IsInitialized")
}

if (!exists(".IsInitialized") ||
    !(rlang::`%||%`(get0(".IsInitialized"), FALSE))) {
    .Initialize()
    .LoadData()
    .PrepareData()

    .IsInitialized <<- FALSE
}
