HottellingT2Test <- function(mean1, mean2, sigma1, sigma2, n1, n2) {
    nu1 <- n1 - 1
    nu2 <- n2 - 1
    k <- 2
    S <- 1.0 / (nu1 + nu2) * (nu1 * sigma1 + nu2 * sigma2)

    I_S <- solve(S)

    XY <- as.numeric(mean1 - mean2)

    T2 <- n1 * n2 * t(XY) %*% I_S %*% (XY) / (n1 + n2)
    f <- (n1 + n2 - 1 - k) * T2 / ((n1 + n2 - 2) * k)
    d1 <- k
    d2 <- n1 + n2 - 1 - k

    p3 <- pf(f, d1, d2)

    beta <- d1 * f / (d1 * f + d2)
    p0 <- (1 - beta) ^ (d2 / 2)
    p1 <- pbeta(1 - beta, d2 / 2, d1 / 2)
    p2 <- 1 - pbeta(beta, d1 / 2, d2 / 2)
    return(c("T^2" = T2, "f" = f, "beta" = beta,
        "p0" = p0, "p1" = p1, "d1" = d1, "d2" = d2))
}

if (IsRun()) {
}