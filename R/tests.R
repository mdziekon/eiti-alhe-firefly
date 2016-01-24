test <- function(seed = 0, dim = 2, count = 15, iters = 10)
{
    r <- seed
    if (r == 0)
        r <- runif(1, 0, 10000000)

    print(r)
    set.seed(r)

#     dim <- 2
#     iters <- 20
#     count <- 20

    result <- ffa_meta(
        goal = multimodal,
        dimensions = dim,
        params = list(
            fflies_count = count,
            iterations = iters,
            coefficients = list(
                absorption = 1,
                randomness = 0.1,
                attraction_base = 1,
                attraction_min = 0.05
            ),
            ranges = list(
                list(min = -10, max = 10),
                list(min = -10, max = 10)
            ),
            rand_scaling = c(1, 1)
        )
    )

    title = "FFA for Multimodal Function"
    popul = "( n = "
    iterStr = ", iters. = "

    print(result$x)
    print(result$y)

    if (dim == 2)
    {
        plotResult(result$p$end, result$p$mode, result$p$ranges,
                   paste(c(title, popul, count, iterStr, iters,"). Best : (",
                           format(round(result$x[1], 3), nsmall = 3), ",",
                           format(round(result$x[2], 3), nsmall = 3), ") ->",
                           format(round(result$y[1], 3), nsmall = 3)), collapse = " "))
    }

}
