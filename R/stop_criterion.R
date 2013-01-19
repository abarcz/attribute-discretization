
Satisfied <- function(object, ...) {
	UseMethod("Satisfied")
}


StopCriterion <- function() {
	criterion <- list()
	class(criterion) <- "StopCriterion"
	return(criterion)
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
		return(FALSE)
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
