
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
