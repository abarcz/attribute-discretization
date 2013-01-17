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

# base class functions definitions

CreateBaseDiscretization <- function(formula, data, stop.criterion) {
	# Base class constructor - performs basic argument checking,
	# parses provided formula.
	#
	# Args:
	#	formula: formula defining which attributes are to be used (e.g. Z ~ .)
	#	data: data.frame
	#	stop.criterion: a StopCriterion object
	#
	# Returns:
	#	Discretization model.
	if (class(formula) != "formula") {
		stop("invalid formula")
	}
	if (class(data) != "data.frame") {
		stop("Argument 'data' is not a data.frame")
	}
	if (!("StopCriterion" %in% class(stop.criterion))) {
		stop("Argument 'stop.criterion' is not a StopCriterion")
	}
	# select attribute labels to be discretized
	discretized.attrs <- labels(terms(formula, data=data))

	# TODO: ugly code
	label <- toString(formula[[2]])

	model <- list(formula=formula, class.label=label,
		stop.criterion=stop.criterion,
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
		split.points <- split.points[order(split.points)]
		all.split.points[[attr.name]] <- split.points
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

print.Discretization <- function(object, ...) {
	print(object$call)
}

summary.Discretization <- function(object, ...) {
	cat("Summary of discretization model:\n")
	print(object$call)
	split.points <- object$split.points
	for (attr.name in names(split.points)) {
		cat("Attribute '")
		cat(attr.name)
		cat("' split points:\n")
		print(split.points[[attr.name]])
	}
}

predict.Discretization <- function(object, newdata) {
	# Discretize newdata according to already
	# defined discretization.
	if (class(newdata) != "data.frame") {
		stop("Argument 'newdata' is not a data.frame")
	}
	# TODO
	print("Prediction")
}
