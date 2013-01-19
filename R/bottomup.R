
source("discretization.R")
source("stop_criterion.R")
source("utils.R")

BottomUp <- function(formula, data, stop.criterion=MaxIntervalsNumCriterion(5)) {
	# Computes the top-down discretization of given data.
	#
	# Args:
	#	formula: a formula, selecting args for discretization
	#	data: training data, used to build discretization model
	#	stop.criterion.name: name of stop criterion to use
	#	stop.criterion.params: correctly named list of params for s-c
	#
	# Returns:
	#	model that can be used for discretization

	model <- CreateBaseDiscretization(formula, data, stop.criterion)

	model$call <- sys.call()

	class(model) <- c("BottomUp", "Discretization")

	model <- DiscretizeSelected(model)	# in R objects are always passed by value..

	return(model)
}

DiscretizeAttribute.BottomUp <- function(object, attribute.name) {
	# Returns set of split points, determining discretization
	# of attribute 'attr', according to 'labels'
	data <- CreateSlice(object, attribute.name)

	split.points <- CreateInitialSplitPoints(data)
	intervals <- CreateIntervals(data, split.points)

	while (!Satisfied(object$stop.criterion, intervals)) {
		chi.vector <- CalculateChiForAllIntervals(intervals)

		minChi = min(chi.vector)
		if(minChi > 0.5) {
			break
		}
		
		index.split.poin.to.merge = order(minChi)[1]
		split.points <- MergeSplitPoints(split.points, index.split.poin.to.merge)
		intervals <- CreateIntervals(data, split.points)
	}

	return(split.points)
}
