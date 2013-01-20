
Satisfied <- function(object, ...) {
	UseMethod("Satisfied")
}


StopCriterion <- function() {
	# base class for all stop criterions
	criterion <- list()
	class(criterion) <- "StopCriterion"
	return(criterion)
}

