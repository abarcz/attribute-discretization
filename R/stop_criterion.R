
Satisfied <- function(object, ...) {
	UseMethod("Satisfied")
}


StopCriterion <- function() {
	criterion <- list()
	class(criterion) <- "StopCriterion"
	return(criterion)
}

RequestedIntervalsNumCriterion <- function(intervals.num) {
	if (intervals.num < 1) {
		stop("intervals.num must be >= 1")
	}
	criterion <- list(intervals.num=intervals.num)
	class(criterion) <- c("RequestedIntervalsNumCriterion", "TopDownStopCriterion", "BottomUpStopCriterion", "StopCriterion")
	return(criterion)
}

Satisfied.RequestedIntervalsNumCriterion <- function(object, intervals) {
	if (length(intervals) == object$intervals.num) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}
