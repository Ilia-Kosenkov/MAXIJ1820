.Initialize <- function() {

    library(tidyverse)
    library(magrittr)
    library(rlang)
    library(jpeg)
    library(RColorBrewer)
    library(tikzDevice)
    library(grid)
    library(glue)
    library(foreach)

    devtools::install_github("Ilia-Kosenkov/RLibs/package",
                             dependencies = FALSE)

    library(RLibs)

    if (!dir.exists(file.path("Source")))
        stop("No `Source` directory found.")

    files <- dir(file.path("Source"), pattern = ".*\\.R", full.names = TRUE, recursive = TRUE)
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

    #starPol <<- read.table(file.path("Data", "star_pol.dat"),
        #TRUE, stringsAsFactors = FALSE) %>%
        #as.tibble

    Bands <<- read_table(file.path("Data", "Bands.dat"),
                        col_types = cols())

    #starPol2 <<- read_delim(file.path("Data", "star_pol2.dat"),
                            #delim = "\t", col_types = cols())

    #averages <<- read.table(file.path("Data", "Averages.dat"),
            #TRUE,
            #stringsAsFactors = FALSE) %>%
        #as.tibble
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

}

.Compile <- function() {
    CompileFortran(file.path("Source", "Fortran"))
}

.SetUpCluster <- function(n = parallel::detectCores() - 1) {
    if (n < 2)
        warning(glue("The number of cores ({n}) is too small. \\
            It is recommended to allocate at least 2 workers. \\
            No parallel background is created."))
    else {
        assign(".Cluster", Cluster$new(n), envir = .GlobalEnv)
        .Cluster$Register()
    }
}

.InitCluster <- function(cluster = .Cluster) {
    clusterCall(cluster$ClusterDesc, source,
                file.path("Source", "Initialize.R"))

    success <- clusterCall(cluster$ClusterDesc, get0,
                            ".IsInitialized") %>%
        unlist %>%
        all

    if (!success) {
        .Cluster$Dispose()
        rm(list = ".Cluster", envir = .GlobalEnv)
        warning("Cluster was not properly initalized.")
    }
    else
        message(glue("Cluster {cluster$ID} was initialized with data."))
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

if (rlang::`%||%`(get0(".IsInitialized"), FALSE) &&
    rlang::`%||%`(get0(".SetParallel"), FALSE) &&
    !exists(".Cluster")) {

    .SetUpCluster()
    .InitCluster()

}
