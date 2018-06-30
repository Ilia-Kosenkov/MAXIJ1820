ReadDescriptor <- function(path = file.path("Test", "Bin.txt")) {
    lns <- read_lines(path)

    if (lns %>%
            last %>%
            str_detect(fixed("end", ignore_case = TRUE)) %>%
            not)
        stop("Unexpected end of file. `end` token is missing.")

    fileName <- lns %>% extract(1)

    params <- lns %>%
        extract(2) %>%
        str_split("\ ") %>%
        unlist

    fullPath <- ParsePath(path) %>%
        extract2(1) %>%
        extract2(1) %>%
        extract(1:(length(.) - 1)) %>%
        c(fileName) %>% paste(collapse = .Platform$file.sep)

    return(list(DataFile = fullPath,
                ObsIdRange = params %>% extract(1:2) %>% as.integer,
                Star = params %>% extract(3),
                FilterId = params %>% extract(4) %>% as.integer))
}

ReadData <- function(path) {
    data <- read.csv(path) %>%
        as.tibble %>%
        rename(JD = T..JD.)
}


if (IsRun()) {
    desc <- ReadDescriptor()
    ReadData(desc$DataFile)
}