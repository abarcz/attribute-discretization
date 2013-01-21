
source("R/stop_criterion.R")

MinEntropyDecreaseCriterion <- function(min.decrease) {
	# top-down stop criterion, checking for minimal entropy decrease
	criterion <- list(min.decrease=min.decrease)
	class(criterion) <- c("MinEntropyDecreaseCriterion", "TopDownStopCriterion", "StopCriterion")
	return(criterion)
}

Satisfied.MinEntropyDecreaseCriterion <- function(object, intervals) {
	for (interval in intervals) {
		if (interval$entropy.decrease > object$min.decrease) {
			return(FALSE)
		}
	}
	return(TRUE)
}
