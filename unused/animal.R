
eat <- function(food, ...) {
	UseMethod("eat")
}

sleep <- function(...) {
	UseMethod("sleep")
}

animal <- function() {
	res <- list()
	class(res) <- "animal"
	return(res)
}

eat.animal <- function(object, food) {
	cat("I'll eat: ")
	cat(food)
	cat("\n")
}

sleep.animal <- function(object) {
	cat("zzz..\n")
}

