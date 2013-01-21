
source("R/stop_criterion.R")

DeltaCriterion <- function() {
	# top-down stop criterion, using entropy and minimal encoding length
	criterion <- list()
	class(criterion) <- c("DeltaCriterion", "TopDownStopCriterion", "StopCriterion")
	return(criterion)
}

Satisfied.DeltaCriterion <- function(object, intervals) {
	# note: attempt to calculate min.entropy.decrease only once for each interval
	# and storing the results in hashmap (used environment as hashmap),
	# using unique id for each interval resulted in same processing time and problems
	for (interval in intervals) {
		new.intervals <- Split(interval)
		delta <- log(3 ^ interval$classes.num - 2)
					- (interval$classes.num * interval$entropy
						- new.intervals[[1]]$classes.num * new.intervals[[1]]$entropy
						- new.intervals[[2]]$classes.num * new.intervals[[2]]$entropy)
		samples.num <- dim(interval$data)[1]
		min.entropy.decrease <- (log(samples.num - 1) + delta) / samples.num
		min.weighted.entropy.decrease <- min.entropy.decrease * samples.num
		if (interval$weighted.entropy.decrease > min.weighted.entropy.decrease) {
			return(FALSE)
		}
	}
	return(TRUE)
}
