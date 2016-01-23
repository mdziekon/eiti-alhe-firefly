maxIterations <- 20

initModel <- function(history)
{
    # ??? Empty model I guess?
    model = list(...)
    return(model)
}

terminationFunc <- function(history, model)
{
    if (length(history) <= maxIterations)
        return (TRUE)
    else
        return (FALSE)
}

initFunc <- function()
{
    # ??? What exactly should it do?
}

# ===============================================================================

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun <- function(initialization, startPoints, termination, evaluation)
{
   history <- initialization(startPoints)   # ???
   history <- evaluateList(history)
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

#evaluate a LIST of points
evaluateList <- function(points, evaluation)
{
  for (i in 1:length(points))
     points[[i]]$quality <- evaluation(points[[i]]$coordinates)
  return (points)
}

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator <- function(history, oldModel)
{
   selectedPoints <- selection(history, oldModel)
   newModel <- modelUpdate(selectedPoints, oldModel)
   newPoints <- variation(selectedPoints, newModel)
   return (list(newPoints=newPoints,newModel=newModel))
}

# ===============================================================================

#selection of a LIST of points from the history
selection <- function(history, model)
{
   #select a number of points from the history using the
   #method's parameters and the current state of the model
   return(selectedPoints)
}

#update of a model based on a LIST of points
modelUpdate <- function(selectedPoints, oldModel)
{
   return (oldModel)
}

#generation of a LIST of new points
variation <- function(selectedPoints, model)
{
   #generate the list of newPoints and then
   return (newPoints)
}

# ===============================================================================

#push a LIST of points into the history
historyPush <- function(oldHistory, newPoints)
{
   newHistory <- c(oldHistory,newPoints)
   return (newHistory)
}

#read a LIST of points pushed recently into the history
historyPop <- function(history, number)
{
   stop=length(history)
   start=max(stop-number+1,1)
   return(history[start:stop])
}
