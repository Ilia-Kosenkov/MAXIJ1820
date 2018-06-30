ParsePath <- function(path, dirSep = "/", extSep = "\\.") {
    pattern <- sprintf("(.*)%s([[:alnum:]]+?)$", extSep)
    match <- str_match(path, pattern)
    pth2FileName <- match %>% extract(, 2) %>% as.character
    ext <- match %>% extract(, 3) %>% as.character

    pth2FileName %>% map2(ext, ~ c(str_split(.x, dirSep), .y))
}

Pdf2Eps <- function(path, additionalParams = "", ext = "ps") {
    walk(path, function(src) {
       parsedPath <- ParsePath(src, "[/\\\\]") %>% extract2(1)
       target <- parsedPath %>%
            extract2(1) %>%
            paste(collapse = .Platform$file.sep) %>%
            paste(ext, sep = ".")

        system(sprintf("pdftops %s %s %s",
              additionalParams,
              src,
              target))
    })
}