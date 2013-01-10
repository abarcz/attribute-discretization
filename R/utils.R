
Entropy <- function(vec) {
	# Calculates entropy, handles special cases
	if (length(vec) <= 1) {
		return(0)
	}
	classes <- unique(vec)
	class.occurencies <- table(vec)
	samples.num <- length(vec)
	entropy <- 0
	for (class in classes) {
		class.samples.num <- class.occurencies[names(class.occurencies) == class]
		class.freq <- class.samples.num / samples.num
		entropy <- entropy - (class.freq * log(class.freq))
	}
	return(as.double(entropy))
}
