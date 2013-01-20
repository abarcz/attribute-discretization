MinChiCriterion <- function(min.chi) {
	criterion <- list(min.chi=min.chi)
	class(criterion) <- c("MinChiCriterion", "BottomUpStopCriterion", "StopCriterion")
	return(criterion)
}

Satisfied.MinChiCriterion <- function(object, intervals, minChi) {
	if (minChi > object$min.chi) {
			return(TRUE)
	}
	return(FALSE)
}