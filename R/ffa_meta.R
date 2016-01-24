source("R/ffa_meta_scheme.R")

ffa_meta <- function (...) {
    # ffa_meta(goal = ..., dimensions = ..., params = list(...))
    # goal - goal function
    # dimensions - number of dimensions of the problem to solve
    # params - list of additional parameters
    #   fflies_count - fireflies population size
    #   iterations - iterations count
    #   coefficients - calculation coefficients
    #     absorption - light absorption over distance
    #     randomness - randomness coefficient
    #     attraction_base - base attraction
    #     attraction_min - min attraction
    #   ranges - list of problem world ranges (per dimension)
    #     min - minimal size of the world
    #     max - maximal size of the world
    #   rand_scaling - vector of scaling values for random move factor (per dimension)
    #     (optional)

    # --- Load arguments

    args <- list(...)

    goal <- args$goal
    dimensions <- args$dimensions
    params <- args$params

    fflies_count <- params$fflies_count
    iterations <- params$iterations
    coefficients <- params$coefficients
    ranges <- params$ranges
    rand_scaling <- params$rand_scaling

    if (is.null(rand_scaling)) {
        # Initialize with "1"'s
        rand_scaling <- rep(1, times = dimensions)
    }

    initHistory <- function () {
        # Create matrix of "dimensions + 1" columns for each firefly
        # Additional column (indexed "dimension + 1") holds current goal function value (in firefly's position)

        fflies <- list()

        for(i in 1:fflies_count) {
            coords <- c()

            for(j in 1:dimensions) {
                coords <- c(coords, runif(1, min = ranges[[j]]$min, max = ranges[[j]]$max))
            }

            fflies[[length(fflies) + 1]] <- list(coordinates = coords, quality = NaN)
        }

        return(fflies)
    }

    initModel <- function (history) {
        return (list(
            params = params,
            vars = list(
                iteration = 1
            )
        ));
    }

    termination <- function (history, model) {
        return (model$vars$iteration > iterations)
    }

    # Run the metaheuristics engine
    run_history <- metaheuristicRun(initHistory, initModel, termination, goal)

    result <- list(
        init = run_history[1:fflies_count],
        end = historyPop(run_history, fflies_count)
    );

    best_idx <- 1
    best_val <- result$end[[best_idx]]$quality
    for(i in 1:length(result$end)) {
        if (result$end[[i]]$quality < best_val) {
            best_val <- result$end[[i]]$quality
            best_idx <- i
        }
    }

    final_result <- list(
        x = result$end[[best_idx]]$coordinates,
        y = best_val
    )

    # Return best point in the last population state
    return(final_result)
}
