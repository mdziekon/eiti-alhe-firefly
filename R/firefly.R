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
