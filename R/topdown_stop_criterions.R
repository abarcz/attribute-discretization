source("stop_criterion.R")

MinEntropyDecreaseCriterion <- function(min.decrease) {
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

DeltaCriterion <- function() {
	criterion <- list()
	class(criterion) <- c("DeltaCriterion", "TopDownStopCriterion", "StopCriterion")
	return(criterion)
}

Satisfied.DeltaCriterion <- function(object, intervals) {
	for (interval in intervals) {
		#print("CHECK")
		#print(interval$data)
		new.intervals <- Split(interval)
		#print(paste(interval$classes.num, interval$entropy))
		#print(new.intervals[[1]]$data)
		#print(paste(new.intervals[[1]]$classes.num, new.intervals[[1]]$entropy))
		#print(new.intervals[[2]]$data)
		#print(paste(new.intervals[[2]]$classes.num, new.intervals[[2]]$entropy))
		delta <- log(3 ^ interval$classes.num - 2)
					- (interval$classes.num * interval$entropy
						- new.intervals[[1]]$classes.num * new.intervals[[1]]$entropy
						- new.intervals[[2]]$classes.num * new.intervals[[2]]$entropy)
		samples.num <- dim(interval$data)[1]
		min.entropy.decrease <- (log(samples.num - 1) + delta) / samples.num
		min.weighted.entropy.decrease <- min.entropy.decrease * samples.num
		#print("RESULTS:")
		#print(min.weighted.entropy.decrease)
		#print(interval$weighted.entropy.decrease)
		if (interval$weighted.entropy.decrease > min.weighted.entropy.decrease) {
			return(FALSE)
		}
	}
	return(TRUE)
}
