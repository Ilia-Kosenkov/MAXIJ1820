GetRawFileNames <- function(path) {
    fls <- dir(path, "*.csv", full.names = TRUE)

    obsBeforeFls <- fls %>%
        str_detect("maxi[0-9]{2}[bvr]{1}") %>% {
            extract(fls, .)
        }

    obsAfterFls <- fls %>%
        str_detect("max18[bvr]{1}[0-9]{2}") %>% {
            extract(fls, .)
        }

    obsBefore <- obsBeforeFls %>%
        str_match(".*maxi[0-9]{2}([bvr]{1}).*") %>% {
            map(c("b", "v", "r"), function(x) .[.[, 2] == x, ])
        } %>% 
        map(~extract(.x, , 1)) %>%
        map(function(x) map(x, read_csv, col_types = cols()) %>% bind_rows) %>%
        setNames(nm = c("b", "v", "r"))

    obsAfter <- obsAfterFls %>%
        str_match(".*max18([bvr]{1}).*") %>% {
            map(c("b", "v", "r"), function(x) .[.[, 2] == x,])
        } %>%
        map(~extract(.x,, 1)) %>%
        map(function(x) map(x, read_csv, col_types = cols()) %>% bind_rows) %>%
        setNames(nm = c("b", "v", "r"))

    obsBefore %>%
        walk2(names(.),
              function(data, name)
                  write_csv(data,
                            file.path(path,
                                      paste0("comb_before_", name, ".csv"))))

    obsAfter %>%
        walk2(names(.),
              function(data, name)
                  write_csv(data,
                            file.path(path,
                                      paste0("comb_after_", name, ".csv"))))

}

if (IsRun()) {
    GetRawFileNames(file.path("Test", "RAW"))
}