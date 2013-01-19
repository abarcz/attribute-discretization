source("discretization.R")
source("stop_criterion.R")
source("utils.R")
source("bottomup_stop_criterions.R")

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

	for (stop.criterion in model$stop.criterions) {
		if (!("BottomUpStopCriterion" %in% class(stop.criterion))) {
			stop("Argument 'stop.criterion' is not a TopDownStopCriterion")
		}
	}

	model$call <- sys.call()

	class(model) <- c("BottomUp", "Discretization")

	model <- DiscretizeSelected(model)	# in R objects are always passed by value..

	return(model)
}

DiscretizeAttribute.BottomUp <- function(object, attribute.name) {
	# Returns set of split points, determining discretization
	# of attribute 'attr', according to 'labels'
	print(paste("discretize attribute", attribute.name))

	data <- CreateSlice(object, attribute.name)

	split.points <- CreateInintialSplitPoints(data)
	print("initial split points")
	print(split.points)
	intervals <- CreateIntervals(data, split.points)
	print("initial intervals")
	print(intervals)
	chi.vector <- CalculateChiForAllIntervals(intervals)
	minChi = min(chi.vector)
	while (!StopCriterionSatisfied(object, intervals,minChi) && length(intervals)>1) {
		chi.vector <- CalculateChiForAllIntervals(intervals)

		minChi = min(chi.vector)
		print(paste("minChi",minChi))
		index.split.poin.to.merge = order(minChi)[1]
		split.points <- MergeSplitPoints(split.points, index.split.poin.to.merge)
		print("create intervals")
		print("data")		
		print(data)
		print("split.points")
		print(split.points)
		intervals <- CreateIntervals(data, split.points)
		if(length(intervals)==1) {
			print("Warning: no more intervals to merge!")
		}
	}

	return(split.points)
}
