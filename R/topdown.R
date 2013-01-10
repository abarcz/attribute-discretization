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
		max.entropy.decrease <- 0
		selected.interval.index <- 0
		index <- 1
		for (interval in intervals) {
			if (interval$weighted.entropy.decrease > max.entropy.decrease) {
					max.entropy.decrease <- interval$weighted.entropy.decrease
					selected.interval.index <- index
			}
			index <- index + 1
		}
		if (max.entropy.decrease == 0) {
			break
		}
		interval.to.split <- intervals[[selected.interval.index]]
		split.points[length(split.points) + 1] <- interval.to.split$best.treshold
		new.intervals <- Split(interval.to.split)

		intervals[[selected.interval.index]] <- NULL	# delete old interval
		# concatenate two lists
		intervals[[length(intervals) + 1]] <- new.intervals[[1]]
		intervals[[length(intervals) + 1]] <- new.intervals[[2]]
	}
	return(split.points)
}

CreateSlice.TopDown <- function(object, attribute.name) {
	# select data slice, containing label and processed attribute
	class.label <- object$class.label
	data_slice <- data.frame(labels=object$data[[class.label]],
		attr=object$data[[attribute.name]])

	# sort data_slice according to attr values
	data_slice <- data_slice[order(data_slice$attr), ]

	return(data_slice)
}

