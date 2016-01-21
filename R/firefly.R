library(plotly)
library(hydroPSO)

getIntensity <- function(goal, x, pars) {
    -1 * goal(x)
}

getMovement <- function(beacon, moved, dim, pars) {
    newPos <- moved

    i <- 1
    while(i <= dim) {
        newPos[i] <- newPos[i] + (0.25 * (beacon[i] - newPos[i]))

        i <- i + 1
    }

    newPos
}

firefly <- function(goal, dim, pars) {
    t <- 1

    n <- pars$n
    maxIter <- pars$maxIter
    absorption = pars$gamma
    rangeStart <- pars$rangeStart
    rangeEnd <- pars$rangeEnd

    worms <- matrix(, nrow= n, ncol=dim)

    # Worms initializer
    i <- 1
    while(i <= n) {
        j <- 1
        while(j <= dim) {
            worms[i, j] <- runif(1, min = rangeStart, max = rangeEnd + 1)

            j <- j + 1
        }

        i <- i + 1
    }

    print(worms)

    while(t <= maxIter) {
        #         cat('Iteration: ')
        #         cat(t)
        #         cat("\n")

        i = 1

        while(i <= n) {
            #             cat('i = ')
            #             cat(i)
            #             cat("\n")

            j = 1

            while(j < i) {
                #                 cat('j = ')
                #                 cat(j)
                #                 cat("\n")

                wormJ = worms[j, ]
                wormI = worms[i, ]

                intensityJ <- getIntensity(goal, wormJ)
                intensityI <- getIntensity(goal, wormI)

                #                 print(intensityJ)
                #                 print(intensityI)

                if (intensityJ > intensityI) {
                    newPos <- getMovement(wormJ, wormI, dim)

                    worms[i, ] <- newPos
                }

                j <- j + 1
            }

            i <- i + 1
        }

        t <- t + 1
    }

    print(worms)

    worms
}


fireflyBest <- function(goal, dim, pars) {
    t <- 1

    n <- pars$n
    maxIter <- pars$maxIter
    absorption = pars$gamma
    rangeStart <- pars$rangeStart
    rangeEnd <- pars$rangeEnd

    worms <- matrix(, nrow= n, ncol=dim)

    # Worms initializer
    i <- 1
    while(i <= n) {
        j <- 1
        while(j <= dim) {
            worms[i, j] <- runif(1, min = rangeStart, max = rangeEnd + 1)

            j <- j + 1
        }

        i <- i + 1
    }

    mtx <- matrix(, nrow = n, ncol = 3)
    mtx[, 1] = worms[, 1]
    mtx[, 2] = worms[, 2]

    i <- 1
    while(i <= n) {
        mtx[i, 3] = goal(worms[i, ])

        i <- i + 1
    }
    print(mtx)

    while(t <= maxIter) {
#         cat('Iteration: ')
#         cat(t)
#         cat("\n")

        i = 1

        while(i <= n) {
#             cat('i = ')
#             cat(i)
#             cat("\n")

            j = 1
            moved <- 0

            wormI = worms[i, ]
            intensityI <- getIntensity(goal, wormI)
            currentIntensity <- intensityI - 1
            currentJ <- j


            while(j <= n) {
                if (i == j) {
                    j <- j + 1
                    next
                }
#                 cat('j = ')
#                 cat(j)
#                 cat("\n")

                wormJ = worms[j, ]
                intensityJ <- getIntensity(goal, wormJ)
                if (currentIntensity < intensityJ) {
                    currentIntensity <- intensityJ
                    currentJ <- j
                }

#                 print(intensityJ)
#                 print(intensityI)

#                 if (intensityJ > intensityI) {
#                     newPos <- getMovement(wormJ, wormI, dim)
#                     moved = 1
#                     worms[i, ] <- newPos
#                 }

                j <- j + 1
            }

            if (currentIntensity > intensityI) {
                wormJ = worms[currentJ, ]
                newPos <- getMovement(wormJ, wormI, dim)
                moved = 1
                worms[i, ] <- newPos
            }

            # TODO Move randomly if not moved.
            #if (moved == 0)
            #    moveRand

            i <- i + 1
        }

        t <- t + 1
    }

    #print(worms)

    worms
}

ackley2Matrix <- function(dim, min, max, res, getMatrix = TRUE) {
    dist <- (max - min)
    sizes <- (dist * (1 / res)) + 1

    # print(sizes)

    result <- matrix(, ncol = sizes, nrow = sizes)
    axis <- numeric(sizes)

    i <- min
    a <- 1
    while(i <= max) {
        j <- min
        b <- 1

        while(j <= max) {
#             print(a)
#             print(b)
            result[a, b] =  hydroPSO::ackley(c(i, j))

            j <- j + res
            b <- b + 1
        }

        axis[a] = i

        i <- i + res
        a <- a + 1
    }

    if (getMatrix) {
        result
    } else {
        axis
    }
}

plotAckley <- function(dim, min, max, res) {
    data = ackley2Matrix(dim, min, max, res, TRUE)
    axis = ackley2Matrix(dim, min, max, res, FALSE)

    plotly::plot_ly(x = axis, y = axis, z = data, type = "surface")
}

plotFirefly <- function(goal, dim, pars) {
    n <- pars$n

    mtx <- matrix(, nrow = n, ncol = 3)

    ff <- firefly(goal, dim, pars)

    mtx[, 1] = ff[, 1]
    mtx[, 2] = ff[, 2]

    i <- 1
    while(i <= n) {
        mtx[i, 3] = goal(ff[i, ])

        i <- i + 1
    }

    print(mtx)

    df <- setNames(data.frame(mtx), c("x", "y", "z"))

    range <- list(range = c(-10, 10), autorange = F)
    l <- list(xaxis = range, yaxis = range)

    p <- plotly::plot_ly(df, x = x, y = y, z = z,
                    type = "scatter3d", mode = "markers")
    lay <- layout( p, xaxis = range, yaxis = range)
    p
}

