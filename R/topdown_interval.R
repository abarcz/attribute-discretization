
FindBestSplit <- function(object) {
	UseMethod("FindBestSplit")
}

Split <- function(object) {
	UseMethod("Split")
}

TopDownInterval <- function(data, class.label, attr.label) {
	# Interval of data, holding information about
	# the best possible split and the data itself.
	#
	# Args:
	#	data: data.frame
	#	class.label: string containing class label from data
	#	attr.label: string containing attribute name from data
	#
	# Returns:
	#	An interval with best split calculated.

	entropy <- Entropy(data[[class.label]])
	classes.num <- length(unique(data[[class.label]]))

	interval <- list(data=data, class.label=class.label, attr.label=attr.label,
			entropy=entropy, classes.num=classes.num)
	class(interval) <- "TopDownInterval"

	interval <- FindBestSplit(interval)	# in R objects are always passed by value..

	return(interval)
}

Split.TopDownInterval <- function(object) {
	# Split according to best treshold
	if (is.null(object$best.treshold)) {
		return(list(object))
	}

	data <- object$data
	selector <- data[[object$attr.label]] < object$best.treshold
	lower.part <- data[selector, ]
	upper.part <- data[!selector, ]

	lower.interval <- TopDownInterval(lower.part, object$class.label, object$attr.label)
	upper.interval <- TopDownInterval(upper.part, object$class.label, object$attr.label)
	return(list(lower.interval, upper.interval))
}

FindBestSplit.TopDownInterval <- function(object) {
	data <- object$data
	samples.num <- dim(data)[1]

	if (samples.num == 1) {
		object$entropy.decrease <- 0
		object$weighted.entropy.decrease <- 0
		object$best.treshold <- NULL
		return(object)
	}

	possible.tresholds <- unique(data[[object$attr.label]])
	if (length(possible.tresholds) < 2) {
		object$entropy.decrease <- 0
		object$weighted.entropy.decrease <- 0
		object$best.treshold <- NULL
		return(object)
	}
	possible.tresholds <- possible.tresholds[order(possible.tresholds)]
	# ommit zero-split
	prev.treshold <- possible.tresholds[1]
	possible.tresholds <- possible.tresholds[2:length(possible.tresholds)]

	min.entropy <- Inf
	selected.treshold <- NULL
	for (treshold in possible.tresholds) {
		selector <- data[[object$attr.label]] < treshold
		lower.part <- data[selector, ]
		upper.part <- data[!selector, ]

		lower.entropy <- Entropy(lower.part[[object$class.label]])
		upper.entropy <- Entropy(upper.part[[object$class.label]])

		result.entropy <-
			((lower.entropy * dim(lower.part)[1])
			+ (upper.entropy * dim(upper.part)[1])) / samples.num

		if (result.entropy < min.entropy) {
			min.entropy <- result.entropy
			selected.treshold <- (treshold + prev.treshold) / 2
		}
		prev.treshold <- treshold
	}

	entropy.decrease <- object$entropy - min.entropy
	object$entropy.decrease <- entropy.decrease
	object$weighted.entropy.decrease <- entropy.decrease * samples.num
	object$best.treshold <- selected.treshold
	return(object)
}

Entropy <- function(vec) {
	# Calculates entropy, handles special cases
	if (length(vec) <= 1) {
		return(0)
	}
	classes <- unique(vec)
	class.occurencies <- table(vec)
	samples.num <- length(vec)
	entropy <- 0
	for (class in classes) {
		class.samples.num <- class.occurencies[names(class.occurencies) == class]
		class.freq <- class.samples.num / samples.num
		entropy <- entropy - (class.freq * log(class.freq))
	}
	return(as.double(entropy))
}
