# Draws fireflies positions and their values on 2D or 3D plot.
# example:
# plotFirefly(
#     firefly,                  # firefly algorithm to use
#     hydroPSO::ackley,         # goal function
#     3,                        # no. of dimensions
#     data.frame(
#         n = 20,               # fireflies count
#         maxIter = 100,        # iterations
#         gamma = 1,            # attraction parameter
#         rangeStart = -10,     # range values
#         rangeEnd = 10,
#         mode = 2              # 2 for 2-dimensional plot, 3 for 3D
#    )
# )
plotFirefly <- function(algorithm, goal, dim, pars) {
    # Run algorithm.
    ff <- algorithm(goal, dim, pars)

    # Prepare data.
    n <- pars$n
    mtx <- matrix(, nrow = n + 2, ncol = 3)

    i <- 1
    while(i <= n) {
        mtx[i, 1] = ff[i, 1]
        mtx[i, 2] = ff[i, 2]
        mtx[i, 3] = goal(ff[i, ])

        i <- i + 1
    }

    # Some extra points to align the plot.
    mtx[i, 1] = pars$rangeStart
    mtx[i, 2] = pars$rangeStart
    mtx[i+1, 1] = pars$rangeEnd
    mtx[i+1, 2] = pars$rangeEnd

    # Draw the plot.
    df <- setNames(data.frame(mtx), c("x", "y", "z"))

    if (pars$mode == 2) {
        plotly::plot_ly(df, x = x, y = y, z = z,
                    type = "scatter", mode = "markers")
    }
    else {
        plotly::plot_ly(df, x = x, y = y, z = z,
                        type = "scatter3d", mode = "markers")
    }
}

# Drawd 3D plot of given function.
# goalFunc = ackleyF |

# example:
# plotGoal(
#     ackleyF,
#     -10 ,
#     10,
#     0.1
# )
plotGoal <- function(goalFunc, min, max, res) {
    data = func2matrix(goalFunc, min, max, res)

    plotly::plot_ly(x = data$axis, y = data$axis, z = data$result, type = "surface")
}

# Converts values of goal function to matrix.
func2matrix <- function(goalFunc, min, max, res) {
    dist <- (max - min)
    sizes <- (dist * (1 / res)) + 1

    result <- matrix(, ncol = sizes, nrow = sizes)
    axis <- numeric(sizes)

    i <- min
    a <- 1
    while(i <= max) {
        j <- min
        b <- 1

        while(j <= max) {
            result[a, b] =  goalFunc(c(i, j))

            j <- j + res
            b <- b + 1
        }

        axis[a] = i

        i <- i + res
        a <- a + 1
    }

    list(result = result, axis = axis)
}





