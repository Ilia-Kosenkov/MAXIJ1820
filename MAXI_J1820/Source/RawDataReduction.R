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

    return(list(DataFile = fileName,
                ObsIdRange = params %>% extract(1:2) %>% as.integer,
                Star = params %>% extract(3),
                FilterId = params %>% extract(4) %>% as.integer))
}

ReadData <- function(path) {

}


if (IsRun()) {
    ReadDescriptor() %T>% print
}