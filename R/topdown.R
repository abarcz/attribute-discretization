source("discretization.R")
source("topdown_interval.R")
source("stop_criterion.R")

TopDown <- function(formula, data, stop.criterion=MaxIntervalsNumCriterion(5)) {
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

	class(model) <- c("TopDown", "Discretization")

	model <- DiscretizeSelected(model)	# in R objects are always passed by value..

	return(model)
}

DiscretizeAttribute.TopDown <- function(object, attribute.name) {
	# Returns set of split points, determining discretization
	# of attribute 'attr', according to 'labels'
	data <- CreateSlice(object, attribute.name)

	intervals <- list(TopDownInterval(data, "labels", "attr"))
	split.points <- c()
	while (!Satisfied(object$stop.criterion, intervals)) {
		max.entropy.decrease <- -Inf
		selected.interval.index <- 0
		index <- 1
		for (interval in intervals) {
			if (interval$weighted.entropy.decrease > max.entropy.decrease) {
					if (!is.null(interval$best.treshold)) {
						max.entropy.decrease <- interval$weighted.entropy.decrease
						selected.interval.index <- index
					}
			}
			index <- index + 1
		}
		if (selected.interval.index == 0) {
			print(paste("Warning: no more intervals to split for", attribute.name))
			break
		}
		interval.to.split <- intervals[[selected.interval.index]]
		split.points[length(split.points) + 1] <- interval.to.split$best.treshold
		if (max.entropy.decrease <= 0) {
			print(paste("Warning: splitting", attribute.name, "at", interval.to.split$best.treshold, "doesn't decrease entropy"))
		}
		new.intervals <- Split(interval.to.split)

		intervals[[selected.interval.index]] <- NULL	# delete old interval
		# concatenate two lists
		intervals[[length(intervals) + 1]] <- new.intervals[[1]]
		intervals[[length(intervals) + 1]] <- new.intervals[[2]]
	}
	return(split.points)
}
