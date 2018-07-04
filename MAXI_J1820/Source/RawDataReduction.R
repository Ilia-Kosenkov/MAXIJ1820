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
        rename(JD = T..JD., Ref = Ref1, Obs = Obj1) 
}

ProcessObservations <- function(data,
                        corrPosAng = 35.9,
                        eqtrialCorrFactor = 0.034907) {
    nObsPerMes <- 4

    GetPX <- function(x)
        100.0 * (x[1] - x[3])

    GetPY <- function(x)
        100.0 * (x[2] - x[4])

    # Store mean polarizations between iterations
    pxMean <- rep(0, nrow(data) / nObsPerMes)
    pyMean <- rep(0, nrow(data) / nObsPerMes)

    std <- 0
    ndis <- 0

    trnsfData <- data %>%
        mutate(Q = 10 ^ (0.4 * Obs)) %>%
        mutate(Id = (1:n() - 1) %/% nObsPerMes) %>%
        mutate(Id = as.integer(Id) + 1L) %>%
        group_by(Id)

    prepData <- trnsfData %>%
        summarise(mJD = mean(JD), sQ = sum(Q),
                  PX = GetPX(Q) / sQ,
                  PY = GetPY(Q) / sQ)

    for (i in 1:15) {
        locData <- prepData %>%
            mutate(WX = 1, WY = 1) %>%
            mutate(mPX = pxMean, mPY = pyMean) %>%
            mutate(dX = abs(PX - mPX), dY = abs(PY - mPY)) %>% {
                if (i > 1) {
                    mutate(., WX = if_else(dX > std, (std / dX) ^ 2, WX)) %>%
                    mutate(WX = if_else(WX < 0.11, 0, WX)) %>%
                    mutate(WY = if_else(dY > std, (std / dY) ^ 2, WY)) %>%
                    mutate(WY = if_else(WY < 0.11, 0, WY))
                }
                else
                    .

            }

        ndis <- locData %>%
            select(WX, WY) %>%
            summarise(sum(WX < 1), sum(WY < 1)) %>%
            as.numeric %>% sum

        rf <- 50 * ndis / nrow(prepData)

        pX <- locData %$% {
            WX %*% PX / sum(WX)
        } %>% as.numeric
        pY <- locData %$% {
            WY %*% PY / sum(WY)
        } %>% as.numeric

        tObs <- locData %>%
            pull(mJD) %>%
            mean

        p <- sqrt(pX ^ 2 + pY ^ 2)
        a <- (90 / pi * atan2(pY, pX) + corrPosAng) %>%
            divide_by(180) %>% {
                . - floor(.)
            } %>%
            multiply_by(180)

        sgX <- locData %$% {
            WX %*% (PX - pX) ^ 2 / sum(WX)
        } %>% as.numeric

        sgY <- locData %$% {
            WY %*% (PY - pY) ^ 2 / sum(WY)
        } %>% as.numeric


        sg <- sqrt((sgX + sgY) / 2 / (nrow(prepData) - 1))
        eAng <- 90 / pi * atan2(sg, p)

        corrA <- a * eqtrialCorrFactor
        corrPx <- p * cos(corrA)
        corrPy <- p * sin(corrA)

        pxMean <- rep(pX, nrow(prepData))
        pyMean <- rep(pY, nrow(prepData))

        rf <- 50 * ndis / nrow(prepData)
        if (i == 1)
            std <- sg * sqrt(nrow(prepData))
        else if (abs(rf - 31) > 1)
            std <- std  + std * 0.008 * (rf - 31)
        print(c(p, a))
    }

}

if (IsRun()) {
    desc <- ReadDescriptor()
    data <- ReadData(desc$DataFile)
    ProcessObservations(data)
}