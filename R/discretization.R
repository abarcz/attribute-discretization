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
	if (class(formula) != "formula") {
		stop("invalid formula")
	}
	if (class(data) != "data.frame") {
		stop("Argument 'data' is not a data.frame")
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
	if (class(newdata) != "data.frame") {
		stop("Argument 'newdata' is not a data.frame")
	}
	# TODO
	print("Prediction")
}
