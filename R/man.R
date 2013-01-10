
source("animal.R")			# source(".../man.R", chdir=TRUE) will make man.R see animal.R file

man <- function() {
	res <- list()
	class(res) <- c("man", "animal")
	return(res)
}

eat.man <- function(object, food) {
	food <- paste(food, "with spices")
	cat("I'll cook: ")
	cat(food)
	cat("\n")
	NextMethod()			# calling methods other than "eat" doesn't work
	sleep(object)
}
