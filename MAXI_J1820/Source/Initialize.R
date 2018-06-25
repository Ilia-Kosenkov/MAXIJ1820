.Initialize <- function() {

    library(tidyverse)
    library(magrittr)
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

    fieldStars <<- read.table(file.path("Data", "field_stars.dat"), TRUE) %>%
        as.tibble
}

if (!exists(".IsInitialized") ||
    (rlang::`%||%`(get0(".IsInitialzied") == FALSE))) {
    .Initialize()
    .LoadData()
}
