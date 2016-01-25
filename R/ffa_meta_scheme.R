#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################

#####  THE METAHEURISTIC "ENGINE" (DO NOT TOUCH)

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun <- function(initHistory, initModel, termination, evaluation)
{
    history <- initHistory()

    history <- evaluateList(history, evaluation)
    model <- initModel(history)
    while (!termination(history, model))
    {
        aa <- aggregatedOperator(history, model)
        aa$newPoints <- evaluateList(aa$newPoints, evaluation)

        history <- historyPush(history, aa$newPoints)
        model <- aa$newModel
    }
    return(history)
}

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator <- function(history, oldModel)
{
    selectedPoints <- selection(history, oldModel)
    newModel <- modelUpdate(selectedPoints, oldModel)

    newPoints <- variation(selectedPoints, newModel)

    return (list(newPoints = newPoints, newModel = newModel))
}

#push a LIST of points into the history
historyPush <- function(oldHistory, newPoints)
{
    for(i in 1:length(newPoints)) {
        oldHistory[[length(oldHistory) + 1]] <- newPoints[[i]]
    }

    return (oldHistory)
}
#read a LIST of points pushed recently into the history
historyPop <- function(history, number)
{
    stop = length(history)
    start = max(stop - number + 1, 1)

    return (history[start:stop])
}

#evaluate a LIST of points
evaluateList <- function(points, evaluation)
{
    for (i in 1:length(points))
    {
        points[[i]]$quality <- evaluation(points[[i]]$coordinates)
    }

    return (points)
}


####  METAHEURISTICS ENGINE ENDS HERE

####  USER DEFINED CODE

# Just select last fireflies population state from the history
selection <- function(history, model)
{
    return (historyPop(history, model$params$fflies_count))
}

# FFA does not require a model
# but we can use it to hold algo params & runtime variables
modelUpdate <- function(selectedPoints, oldModel)
{
    oldModel$vars$iteration <- oldModel$vars$iteration + 1

    return (oldModel)
}

## Main FFA part
# Get current fireflies
# Move them around basin on light intensity and attraction between them
# Return
variation <- function(selectedPoints, model)
{
    return (move_fflies(
        selectedPoints,
        selectedPoints,
        model$params$coefficients,
        model$params$ranges,
        model$params$rand_scaling
    ))
}

### FFA code

move_fflies <- function(fflies_current, fflies_prev, coefficients, ranges, rand_scaling) {
    # Moves all fireflies towards every relatively more bright firefly

    fflies_count <- length(fflies_current)
    dimensions <- length(fflies_current[[1]]$coordinates)

    for(i in 1:fflies_count) {
        moved <- FALSE

        for(j in 1:fflies_count) {
            move_result <- move_fly(fflies_current[[i]], fflies_prev[[j]], coefficients, ranges, rand_scaling)

            moved <- moved || move_result$moved

            fflies_current[[i]]$coordinates <- move_result$fly_coords
        }

        if (moved == FALSE) {
            # Force random move when not moved at all
            fflies_current[[i]]$coordinates <- (
                fflies_current[[i]]$coordinates +
                randomize_move_vector(dimensions, coefficients, rand_scaling)
            )

            fflies_current[[i]]$coordinates <- apply_bounds(fflies_current[[i]]$coordinates, ranges)
        }

        fflies_current[[i]]$quality <- NaN
    }

    # These flies have $quality reset to NaN
    return (fflies_current)
}

move_fly <- function(fly_current, fly_adjacent, coefficients, ranges, rand_scaling) {
    # Returns movement vector of one fly moving towards adjacent one

    dimensions <- length(fly_current$coordinates)

    result = list(
        moved = FALSE,
        fly_coords = fly_current$coordinates
    )

    distance <- euclidean_distance(fly_current$coordinates, fly_adjacent$coordinates)
    adj_intensity <- calc_intensity(distance, coefficients, fly_adjacent$quality)

    # Implemented as minimalization problem solver,
    # therefore when "current" firefly has higher value than the adjacent one,
    # it should be moved

    if (fly_current$quality <= adj_intensity) {
        # Jump out early
        return (result)
    }

    attraction <- calc_attraction(distance, coefficients)

    result <- calc_moves(fly_current$coordinates, fly_adjacent$coordinates, attraction)
    result <- result + randomize_move_vector(dimensions, coefficients, rand_scaling)

    new_coordinates <- fly_current$coordinates + result
    new_coordinates <- apply_bounds(new_coordinates, ranges)

    result = list(
        moved = TRUE,
        fly_coords = new_coordinates
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

euclidean_distance <- function(fly_left_coords, fly_right_coords) {
    dimensions <- length(fly_left_coords)

    aggr <- 0

    for(i in 1:dimensions) {
        diff <- fly_left_coords[i] - fly_right_coords[i]
        aggr <- aggr + (diff * diff)
    }

    aggr <- sqrt(aggr)

    return(aggr)
}

calc_intensity <- function(distance, coefficients, quality) {
    s <- -1
    if (quality > 0)
        s <- 1

    intensity <- quality * exp((s * coefficients$gamma) * (distance * distance));

    #     if (attraction < coefficients$attraction_min) {
    #         attraction <- coefficients$attraction_min
    #     }

    return(intensity)
}

calc_attraction <- function(distance, coefficients) {
    attraction <- coefficients$attraction_base * exp((-coefficients$absorption) * (distance * distance));

    if (attraction < coefficients$attraction_min) {
        attraction <- coefficients$attraction_min
    }

    return(attraction)
}

calc_moves <- function(fly_left_coords, fly_right_coords, attraction) {
    dimensions <- length(fly_left_coords)

    vector <- numeric(dimensions)

    for(i in 1:dimensions) {
        vector[i] <- attraction * (fly_right_coords[i] - fly_left_coords[i])
    }

    return(vector)
}

apply_bounds <- function(fly_coords, ranges) {
    dimensions <- length(fly_coords)

    # Safeguard against moving out of ranges
    for(k in 1:dimensions) {
        if (fly_coords[k] > ranges[[k]]$max) {
            fly_coords[k] <- ranges[[k]]$max
        }

        if (fly_coords[k] < ranges[[k]]$min) {
            fly_coords[k] <- ranges[[k]]$min
        }
    }

    return(fly_coords)
}
