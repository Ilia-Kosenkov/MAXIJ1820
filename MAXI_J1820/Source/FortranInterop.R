ExecuteUnix <- function(command, winSubSys = "bash") {
    sysName <- Sys.info()["sysname"]

    if (grepl("win(dows)?", sysName, ignore.case = TRUE)) {
        isShellAvailale <- try({
            system(paste(winSubSys, "-version"),
                ignore.stderr = TRUE, ignore.stdout = TRUE, intern = TRUE)
        }, TRUE) %>% is_empty()

        if (!isShellAvailale)
            stop(sprintf("Shell %s is unavailable.", winSubSys))

        shell(sprintf("bash -c \"%s\"", command))
    }
    else if (grepl("Lin(ux)?", sysName, ignore.case = TRUE)) {
        system(command)
    }
    else stop("Unknown system.")
}

CompileFortran <- function(path) {
    files <- dir(path, pattern = "(for)|(f90)", full.names = TRUE)

    output <- file.path("Binary")

    nms <- files %>% map_chr(~ParsePath(.x) %>%
        extract2(1) %>%
        extract2(1) %>%
        last) %>%
        paste0(".out")

    map2(files, nms,
        ~ sprintf("gfortran %s -o %s", .x, file.path(output, .y))) %>%
        map(ExecuteUnix)

}

if (IsRun()) {
}