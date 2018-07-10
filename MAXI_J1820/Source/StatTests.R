HottellingT2Test <- function(mean1, mean2, sigma1, sigma2, n1, n2) {
    nu1 <- n1 - 1L
    nu2 <- n2 - 1L
    k <- 2L
    S <- 1.0 / (nu1 + nu2) * (nu1 * sigma1 + nu2 * sigma2)

    I_S <- solve(S)

    XY <- as.numeric(mean1 - mean2)

    T2 <- (n1 * n2 * t(XY) %*% I_S %*% (XY) / (n1 + n2)) %>% as.numeric
    f <- (n1 + n2 - 1 - k) * T2 / ((n1 + n2 - 2) * k)
    d1 <- k
    d2 <- n1 + n2 - 1L - k

    p <- pf(f, d1, d2)
    p_inv <- pf(f, d1, d2, lower.tail = FALSE)
    lgp <- pf(f, d1, d2, log.p = TRUE) / log(10)
    lgp_inv <- pf(f, d1, d2, lower.tail = FALSE, log.p = TRUE) / log(10)
    #beta <- d1 * f / (d1 * f + d2)
    #p0 <- (1 - beta) ^ (d2 / 2)
    #p1 <- pbeta(1 - beta, d2 / 2, d1 / 2)
    #p2 <- 1 - pbeta(beta, d1 / 2, d2 / 2)
    return(tibble("T^2" = T2, "f" = f,
        "p" = p, "1-p" = p_inv,
        "lg(p)" = lgp,
        "lg(1-p)" = lgp_inv,
        "d1" = d1, "d2" = d2))
           #"beta" = beta,
        #"p0" = p0, "p1" = p1, "d1" = d1, "d2" = d2)
}

if (IsRun()) {
    calculateStats <- function(data) {
        mean1 <- data %>%
            extract(1, c("PX", "PY")) %>%
            as.numeric
        mean2 <- data %>%
            extract(2, c("PX", "PY")) %>%
            as.numeric

        sigma1 <- data %>%
            extract(1, "Ep") %>%
            as.numeric %>%
            multiply_by(matrix(c(1, 0, 0, 1), ncol = 2))

        sigma2 <- data %>%
            extract(2, "Ep") %>%
            as.numeric %>%
            multiply_by(matrix(c(1, 0, 0, 1), ncol = 2))

        n1 <- data %>%
            extract(1, "Nobs") %>%
            as.integer

        n2 <- data %>%
            extract(2, "Nobs") %>%
            as.integer
        HottellingT2Test(mean1, mean2, sigma1, sigma2, n1, n2)
    }
    Bands %>%
        pull(ID) %>%
        map(~filter(averages, FIL == .x) %>% arrange(ID)) %>%
        map(calculateStats) %T>% print
}