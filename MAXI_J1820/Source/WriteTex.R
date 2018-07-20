WriteTex_MAXI <- function(path = file.path("Output", "maxi.dat")) {
    Bands %>% slice(1:3) %>% pull(Band) %>%
    map(function(x) {
        CombineResults(pattern = sprintf("maxi_%s.txt", x)) %>%
            select(ID, JD, P, Ep, A, Ea) %>%
            transmute(
                ID, JD,
                !!sym(paste0("P", x)) :=
                    sprintf("\\red{$%4.2f  \\pm  %4.2f$}   &", P, Ep),
                !!sym(paste0("A", x)) :=
                    sprintf("\\red{$%4.1f  \\pm  %3.1f$}   &", A, Ea)) %>%
                        select(ID, JD, matches(sprintf("(P|A)%s", x)))
    }) %>%
    reduce(~inner_join(.x, .y, by = "ID")) %>% {
        temp <- .
        val <- names(.) %>%
            keep(~str_detect(.x, "JD.*")) %>%
            map(~pull(temp, .x)) %>%
                reduce2(seq.int(length.out = length(.)-1),
                        function(a, x, y)(a * (y - 1) + x) / y)
        temp %>%
            select(-starts_with("JD"), -ID) %>%
            mutate(JD = val) %>%
            select(JD, everything())
    } %>%
    mutate(JD = sprintf("\\red{%10.5f}   &", JD - 2458000.5)) %>%
    mutate(!!sym(names(.)[ncol(.)]) :=
        str_replace_all(.[[ncol(.)]], "&", "\\\\\\\\")) %>%
    WriteFixed(path,
        frmt = c("%10s", rep("%30s", ncol(.) - 1)))
}

WriteTex_Field <- function(path = file.path("Output", "field.dat")) {

    pat <- Bands %>% pull(Band) %>%
        reduce(paste0, .init = "[") %>%
        paste0("]")

    Bands %>% slice(1:3) %>% pull(Band) %>%
    map(function(x) {
        CombineResults(pattern = sprintf("field_%s.txt", x)) %>%
            select(ID, P, Ep, A, Ea) %>%
            transmute(
                ID,
                !!sym(paste0("P", x)) :=
                    sprintf("\\red{$%4.2f  \\pm  %4.2f$}   &", P, Ep),
                !!sym(paste0("A", x)) :=
                    sprintf("\\red{$%4.1f  \\pm  %3.1f$}   &", A, Ea))
    }) %>%
    reduce(~inner_join(.x, .y, by = "ID")) %>%
    inner_join(starCoords, by = c("ID" = "NO")) %>%
    mutate(ID = as.integer(round(10 * (ID / 10 - floor(ID / 10))))) %>%
    mutate(PI = sprintf("$%4.2f  \\pm  %4.2f$   &", PAR, EPAR)) %>%
    select(-PAR, - EPAR) %>%
    select(ID, PI, matches(sprintf("[PA]%s", pat), ignore.case = FALSE)) %>%
    mutate(ID = sprintf("%5d   &", ID)) %>%
    mutate(!!sym(names(.)[ncol(.)]) :=
        str_replace_all(.[[ncol(.)]], "&", "\\\\\\\\")) %>%
    WriteFixed(path,
        frmt = c("%10s", "%24s", rep("%30s", ncol(.) - 2)))
}

if (IsRun()) {
    #WriteTex_MAXI()
    WriteTex_Field()
   
}