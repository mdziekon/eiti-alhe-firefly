ffa <- function (...) {
    # ffa(goal = ..., dimensions = ..., params = list(...))
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

    # --- Algorithm initialization

    fflies_current <- init_fflies(goal, dimensions, fflies_count, ranges)

    result <- list(init = fflies_current, end = fflies_current);

    # --- Main loop

    for(i in 1:iterations) {
        fflies_prev <- fflies_current

        fflies_current <- move_fflies(goal, fflies_current, fflies_prev, coefficients, ranges, rand_scaling)
    }

    result$end <- fflies_current;

    return(result)
}

init_fflies <- function (goal, dimensions, fflies_count, ranges) {
    # Create matrix of "dimensions + 1" columns for each firefly
    # Additional column (indexed "dimension + 1") holds current goal function value (in firefly's position)

    fflies <- matrix(0, nrow = fflies_count, ncol = dimensions + 1)

    for(i in 1:fflies_count) {
        for(j in 1:dimensions) {
            fflies[i, j] <- runif(1, min = ranges[[j]]$min, max = ranges[[j]]$max)
        }

        fflies[i, dimensions + 1] <- goal(fflies[i, 1:dimensions])
    }

    return(fflies)
}

move_fflies <- function(goal, fflies_current, fflies_prev, coefficients, ranges, rand_scaling) {
    # Moves all fireflies towards every relatively more bright firefly

    dims <- dim(fflies_current)
    fflies_count <- dims[1]
    dimensions <- dims[2] - 1

    for(i in 1:fflies_count) {
        moved <- FALSE

        for(j in 1:fflies_count) {
            move_result <- move_fly(fflies_current[i, ], fflies_prev[j, ], coefficients, ranges, rand_scaling)

            moved <- moved || move_result$moved
            fflies_current[i, ] <- move_result$fly
        }

        if (moved == FALSE) {
            # Force random move when not moved at all
            fflies_current[i, 1:dimensions] <- fflies_current[i, 1:dimensions] + randomize_move_vector(dimensions, coefficients, rand_scaling)

            fflies_current[i, ] <- apply_bounds(fflies_current[i, ], ranges)
        }

        fflies_current[i, dimensions + 1] <- goal(fflies_current[i, 1:dimensions])
    }

    return(fflies_current)
}

move_fly <- function(fly_current, fly_adjacent, coefficients, ranges, rand_scaling) {
    # Returns movement vector of one fly moving towards adjacent one

    dimensions <- length(fly_current) - 1
    value_idx <- dimensions + 1

    result = list(
        moved = FALSE,
        fly = fly_current
    );

    # Implemented as minimalization problem solver,
    # therefore when "current" firefly has higher value than the adjacent one,
    # it should be moved
    if (fly_current[value_idx] <= fly_adjacent[value_idx]) {
        # Jump out early
        return(result)
    }

    distance <- euclidean_distance(fly_current, fly_adjacent)
    attraction <- calc_attraction(distance, coefficients)

    result <- calc_dimension_move(fly_current, fly_adjacent, attraction)
    result <- result + randomize_move_vector(dimensions, coefficients, rand_scaling)

    fly_current[1:dimensions] <- fly_current[1:dimensions] + result

    fly_current <- apply_bounds(fly_current, ranges)

    result = list(
        moved = TRUE,
        fly = fly_current
    )

    return(result)
}

randomize_move_vector <- function(dimensions, coefficients, rand_scaling) {
    vector <- numeric(dimensions)

    for(i in 1:dimensions) {
        rand <- runif(1, min = 0, max = 1)
        vector[i] <- coefficients$randomness * rand_scaling[i] * (rand - 0.5)
    }

    return(vector)
}

euclidean_distance <- function(fly_left, fly_right) {
    dimensions <- length(fly_left) - 1

    aggr <- 0

    for(i in 1:dimensions) {
        diff <- fly_left[i] - fly_right[i]
        aggr <- aggr + (diff * diff)
    }

    aggr <- sqrt(aggr)

    return(aggr)
}

calc_attraction <- function(distance, coefficients) {
    attraction <- coefficients$attraction_base * exp((-coefficients$absorption) * (distance * distance));

    if (attraction < coefficients$attraction_min) {
        attraction <- coefficients$attraction_min
    }

    return(attraction)
}

calc_dimension_move <- function(fly_left, fly_right, attraction) {
    dimensions <- length(fly_left) - 1

    vector <- numeric(dimensions)

    for(i in 1:dimensions) {
        vector[i] <- attraction * (fly_right[i] - fly_left[i])
    }

    return(vector)
}

apply_bounds <- function(fly, ranges) {
    dimensions <- length(fly) - 1

    # Safeguard against moving out of ranges
    for(k in 1:dimensions) {
        if (fly[k] > ranges[[k]]$max) {
            fly[k] <- ranges[[k]]$max
        }

        if (fly[k] < ranges[[k]]$min) {
            fly[k] <- ranges[[k]]$min
        }
    }

    return(fly)
}
