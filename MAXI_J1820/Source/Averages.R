GetRawFileNames <- function(path, pattern = ".*maxi?18([bvr])([0-9]+).csv") {
    fls <- dir(path, "*.csv", full.names = TRUE)

    fls %>% str_match_all(pattern) %>%
        discard(is_empty) %>%
        map(as.tibble) %>%
        bind_rows %>%
        setNames(c("Path", "Band", "ID")) %>%
        mutate(Band = toupper(Band), ID = as.integer(ID))
}

GetFileSizes <- function(path) {
    data <- path %>% map_int(~read_csv(.x, col_types = cols()) %>%
        nrow())
}

GenerateInputFiles <- function(files, bandInfo = Bands, idPrefix = 600) {
    writeFile <- function(data, path) {
        sink(path)
        seq.int(length.out = nrow(data)) %>%
            map(~sprintf("%s\n1%4d%4d%2d\n",
                data %>% extract(.x, "Path"),
                data %>% extract(.x, "FlSz") %>% as.integer,
                data %>% extract(.x, "ID") %>% as.integer + idPrefix,
                data %>% extract(.x, "BandID") %>% as.integer)) %>%
            map(cat)
        cat("end")
        sink()
    }


    bands <- files %>%
        pull(Band) %>%
        unique

    bands %>%
        map(~filter(files, Band == .x) %>% arrange(ID)) %>%
        setNames(bands) %>%
        map(~writeFile(.x,
            sprintf("%sin.txt", .x %>% extract(1, "Band"))))
}

GatherRawOutput <- function(path) {
    files <- dir(path,
        pattern = "(in\\.txt)|(lpo.\\.txt)|(te.\\.txt)|(res_.\\.txt)",
        full.names = TRUE,
        ignore.case = TRUE)
    dirPath <- file.path("Output", "RAW")

    if (!dir.exists(dirPath))
        dir.create(dirPath)

    files %>%
        map(~sprintf("unix2dos %s", .x)) %>%
        walk(ExecuteUnix)

    files %>%
        walk(~file.copy(.x, dirPath, overwrite = TRUE))

    files %>%
        walk(file.remove)
}

ApplyCorrections <- function(bandInfo = Bands,
                starFile = file.path("Test", "maxi.sta")) {
    bandInfo %>%
        pull(Band) %>%
        map(~sprintf("printf \'%s\\nte%s.txt\\n0\\n0\\n\' | %s && %s",
                starFile,
                .x,
                file.path(".", "Binary", "koko.out"),
                sprintf("mv PRKOKO.txt res_%s.txt", .x))) %>%
        walk(ExecuteUnix)
}

ProcessFiles <- function(files) {
    GenerateInputFiles(files)
    Bands %>%
        pull(Band) %>%
        map(~file.path(".", "Binary", sprintf("polco%s.out", .x))) %>%
        walk(ExecuteUnix)

    ApplyCorrections()

    GatherRawOutput(".")
}

SplitInTwo <- function(files, date, bandInfo = Bands) {

    bandInfo %>%
        pull(Band) %>%
        map(~filter(files, Band == .x) %>% arrange(ID)) %>%
        setNames(bandInfo %>% pull(Band)) %>%
        map(~pull(.x, Path)) %>%
        map(function(x) map(x, read_csv, col_types = cols())) %>%
        map(bind_rows) %>%
        map(function(x)
            list(Before = filter(x, `T (JD)` <= date),
                 After = filter(x, `T (JD)` > date)))
}

PrepareAvgData <- function(date = 2458222.5) {
    rawFiles <- GetRawFileNames(file.path("Test", "RAW")) %>%
        mutate(FlSz = GetFileSizes(Path)) %>%
        left_join(Bands, by = "Band", suffix = c("", ".bnd")) %>%
        rename(BandID = ID.bnd)

    dirName <- file.path("Data", "RunTime")

    if (!dir.exists(dirName))
        dir.create(dirName, recursive = TRUE)

    splits <- rawFiles %>% SplitInTwo(date)

    splits %>%
        walk2(Bands %>% pull(Band),
            function(x, b) {
                x %>%
                extract2("Before") %>%
                write_csv(path =
                    file.path(dirName, sprintf("maxi_before_%s.txt", b)))
                x %>%
                extract2("After") %>%
                write_csv(path =
                    file.path(dirName, sprintf("maxi_after_%s.txt", b)))
            })
}

if (IsRun()) {
  
    PrepareAvgData()
   #ProcessFiles(rawFiles)
}