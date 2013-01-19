
source("R/topdown.R", chdir=TRUE)

test.topdown <- function()
{
	checkException(TopDown(), 'No-arg constructor not allowed')
	checkException(TopDown(1, data.frame()), 'First arg must be formula')
	checkException(TopDown(V ~ ., 0), 'Second arg must be data.frame')

	x <- c(0, 0, 1, 1, 0)
	y <- c(1, 2, 3, 4, 5)
	z <- c(1, 1, 1, 2, 2)
	m <- data.frame(x, y, z)
	m
	td <- TopDown(x ~ ., m, MaxIntervalsNumCriterion(5))
	checkEquals(td$discretized.attrs, c("y", "z"))
	checkEquals(td$class.label, "x")
	checkEquals(length(td$split.points), 2)
	checkEquals(names(td$split.points), c("y", "z"))
	checkEquals(td$split.points$y, c(2.5, 4.5)) 
	checkEquals(td$split.points$z, c(1.5)) 
}
