# generic functions declarations
DiscretizeAttribute <- function(object, attribute.name) {
	UseMethod("DiscretizeAttribute")
}

DiscretizeSelected <- function(object, attribute.name) {
	UseMethod("DiscretizeSelected")
}

CreateSlice <- function(object, attribute.name) {
	UseMethod("CreateSlice")
}

StopCriterionSatisfied <- function(object, intervals, ...) {
	UseMethod("StopCriterionSatisfied")
}

# base class functions definitions

CreateBaseDiscretization <- function(formula, data, stop.criterions) {
	# Base class constructor - performs basic argument checking,
	# parses provided formula.
	#
	# Args:
	#	formula: formula defining which attributes are to be used (e.g. Z ~ .)
	#	data: data.frame
	#	stop.criterion: a StopCriterion object (or a list of)
	#
	# Returns:
	#	Discretization model.
	if (class(formula) != "formula") {
		stop("invalid formula")
	}
	if (class(data) != "data.frame") {
		stop("Argument 'data' is not a data.frame")
	}
	if ("list" %in% class(stop.criterions)) {
		if (length(stop.criterions) < 1) {
			stop("List of stop criterions cannot be empty")
		}
		for (stop.criterion in stop.criterions) {
			if (!("StopCriterion" %in% class(stop.criterion))) {
				stop("Argument 'stop.criterion' is not a StopCriterion")
			}
		}
	} else if ("StopCriterion" %in% class(stop.criterions)) {
		stop.criterions <- list(stop.criterions)
	} else {
		stop("Argument 'stop.criterions' is neither a StopCriterion nor a list of StopCriterions")
	}
	# select attribute labels to be discretized
	discretized.attrs <- labels(terms(formula, data=data))

	# TODO: ugly code
	label <- toString(formula[[2]])

	model <- list(formula=formula, class.label=label,
		stop.criterions=stop.criterions,
		discretized.attrs=discretized.attrs,
		data=data)
	class(model) <- "Discretization"
	return(model)
}

DiscretizeSelected.Discretization <- function(object) {
	# Perform discretization on all selected attributes
	all.split.points <- list()
	for (attr.name in object$discretized.attrs) {
		split.points <- DiscretizeAttribute(object, attr.name)
		if (!is.null(split.points)) {
			split.points <- split.points[order(split.points)]
			all.split.points[[attr.name]] <- split.points
		}
	}
	object[["split.points"]] <- all.split.points
	return(object)
}

CreateSlice.Discretization <- function(object, attribute.name) {
	# select data slice, containing label and processed attribute
	class.label <- object$class.label
	data_slice <- data.frame(labels=object$data[[class.label]],
		attr=object$data[[attribute.name]])

	# sort data_slice according to attr values
	data_slice <- data_slice[order(data_slice$attr), ]

	return(data_slice)
}

StopCriterionSatisfied.Discretization <- function(object, intervals, ...) {
	# checks all provided stop criterions
	for (criterion in object$stop.criterions) {
		if (Satisfied(criterion, intervals, ...)) {
			return(TRUE)
		}
	}
	return(FALSE)
}

print.Discretization <- function(object, ...) {
	print(object$call)
	split.points <- object$split.points
	for (attr.name in object$discretized.attrs) {
		cat("Attribute '")
		cat(attr.name)
		cat("' split points:\n")
		print(split.points[[attr.name]])
	}
}

summary.Discretization <- function(object, ...) {
	print(object, ...)
}

predict.Discretization <- function(object, newdata) {
	# Discretize newdata according to already
	# defined discretization.
	if (class(newdata) != "data.frame") {
		stop("Argument 'newdata' is not a data.frame")
	}
	all.split.points <- object$split.points

	# next line ommits all attrs with no discretization split points
	for (attr.name in names(all.split.points)) {
		tresholds <- all.split.points[[attr.name]]
		samples.num <- dim(newdata)[1]
		for (sample.index in 1:samples.num) {
			value.assigned <- FALSE
			for (treshold.index in 1:length(tresholds)) {
				if (newdata[[attr.name]][sample.index] <= tresholds[treshold.index]) {
					newdata[[attr.name]][sample.index] <- treshold.index
					value.assigned <- TRUE
					break
				}
			}
			if (!value.assigned) {
				newdata[[attr.name]][sample.index] <- length(tresholds) + 1
			}
		}
		newdata[[attr.name]] <- as.factor(newdata[[attr.name]])
	}
	return(newdata)
}
