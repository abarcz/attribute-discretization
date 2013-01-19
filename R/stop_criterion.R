
Satisfied <- function(object, ...) {
	UseMethod("Satisfied")
}


StopCriterion <- function() {
	criterion <- list()
	class(criterion) <- "StopCriterion"
	return(criterion)
}

RequestedIntervalsNumCriterion <- function(intervals.num) {
	criterion <- list(intervals.num=intervals.num)
	class(criterion) <- c("RequestedIntervalsNumCriterion", "StopCriterion")
	return(criterion)
}

Satisfied.RequestedIntervalsNumCriterion <- function(object, intervals) {
	if (length(intervals) >= object$intervals.num) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

MaxIntervalsNumCriterion <- function(max.intervals.num) {
	criterion <- list(max.intervals.num=max.intervals.num)
	class(criterion) <- c("MaxIntervalsNumCriterion", "StopCriterion")
	return(criterion)
}

Satisfied.MaxIntervalsNumCriterion <- function(object, intervals) {
	if (length(intervals) >= object$max.intervals.num) {
		return(TRUE)
	} else {
		for (interval in intervals) {
			if (interval$weighted.entropy.decrease > 0) {
				return(FALSE)
			}
		}
		return(TRUE)
	}
}

MinEntropyDecreaseCriterion <- function(min.decrease) {
	criterion <- list(min.decrease=min.decrease)
	class(criterion) <- c("MinEntropyDecreaseCriterion", "StopCriterion")
	return(criterion)
}

Satisfied.MinEntropyDecreaseCriterion <- function(object, intervals) {
	found.nonsatisfied <- FALSE
	for (interval in intervals) {
		if (interval$weighted.entropy.decrease > object$min.decrease) {
			found.nonsatisfied <- TRUE
			break
		}
	}
	return(!found.nonsatisfied)
}
