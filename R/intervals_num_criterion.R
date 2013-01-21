
source("R/stop_criterion.R")

RequestedIntervalsNumCriterion <- function(intervals.num) {
	# stop criterion enforcing requested number of intervals
	if (intervals.num < 1) {
		stop("intervals.num must be >= 1")
	}
	criterion <- list(intervals.num=intervals.num)
	class(criterion) <- c("RequestedIntervalsNumCriterion", "TopDownStopCriterion", "BottomUpStopCriterion", "StopCriterion")
	return(criterion)
}

Satisfied.RequestedIntervalsNumCriterion <- function(object, intervals, ...) {
	if (length(intervals) == object$intervals.num) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}
