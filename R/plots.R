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
plotFirefly <- function(algorithm, goal, dim, pars, name = "") {
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
        p <- plotly::plot_ly(df, x = x, y = y, z = z,
                    type = "scatter", mode = "markers")
        p %>%
            layout(title = name)
    }
    else {
        p <- plotly::plot_ly(df, x = x, y = y, z = z,
                        type = "scatter3d", mode = "markers")
        p %>%
            layout(title = name)
    }
}

# Draws given set of positions on a 2D or 3D plot.
# Only works for 2-dimensional set of coordinates.
#  data - matrix with positions and values.
#  mode - 2 for 2D plot, 3 for 3D.
#  ranges - list of lists with min and max elements for every axis
#  name - name for the plot
#
# example:
# plotResult(
#     data,
#     2,
#     list(
#         list(min = -10, max = 10),
#         list(min = -10, max = 10)
#     ),
#     "Title"
# )
plotResult <- function(data, mode, ranges, name)
{
    # Prepare data.
    n <- length(data[, 1])
    mtx <- matrix(, nrow = n + 2, ncol = 3)

    i <- 1
    while(i <= n) {
        mtx[i, 1] = data[i, 1]
        mtx[i, 2] = data[i, 2]
        mtx[i, 3] = data[i, 3]

        i <- i + 1
    }

    # Some extra points to align the plot.
    mtx[i, 1] = ranges[[1]]$min
    mtx[i, 2] = ranges[[2]]$min
    mtx[i+1, 1] = ranges[[1]]$max
    mtx[i+1, 2] = ranges[[2]]$max

    # Draw the plot.
    df <- setNames(data.frame(mtx), c("x", "y", "z"))

    if (mode == 2) {
        p <- plotly::plot_ly(df, x = x, y = y, z = z,
                             type = "scatter", mode = "markers")
        p %>%
            layout(title = name)
    }
    else {
        p <- plotly::plot_ly(df, x = x, y = y, z = z,
                             type = "scatter3d", mode = "markers")
        p %>%
            layout(title = name)
    }
}

# Drawd 3D plot of given goal function.
# goalFunc = ackleyF |

# example:
# plotGoal(
#     ackley,
#     -10 ,
#     10,
#     0.1
# )
plotGoal <- function(goalFunc, min, max, res, name = "") {
    data = func2matrix(goalFunc, min, max, res)

    z <- data$result

    p <-plotly::plot_ly(x = data$axis, y = data$axis, z = z, type = "contour")
    p %>%
        layout(title = name, xaxis = list(title = "x"),
               yaxis = list(title = "y"), zaxis = list(title = "z"))

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
