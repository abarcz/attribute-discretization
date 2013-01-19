
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

	td <- TopDown(x ~ ., m, MaxIntervalsNumCriterion(5))
	checkEquals(td$discretized.attrs, c("y", "z"))
	checkEquals(td$class.label, "x")
	checkEquals(length(td$split.points), 2)
	checkEquals(names(td$split.points), c("y", "z"))
	checkEquals(td$split.points$y, c(2.5, 4.5)) 
	checkEquals(td$split.points$z, c(1.5))

	checkEquals(TopDown(x ~ ., m, MaxIntervalsNumCriterion(0))$split.points, list())
	checkEquals(TopDown(x ~ ., m, MaxIntervalsNumCriterion(1))$split.points, list())
	checkEquals(TopDown(x ~ ., m, MaxIntervalsNumCriterion(2))$split.points, list(y=c(2.5), z=c(1.5)))
	checkEquals(TopDown(x ~ ., m, MaxIntervalsNumCriterion(3))$split.points, list(y=c(2.5, 4.5), z=c(1.5)))
	checkEquals(TopDown(x ~ y + z, m, MaxIntervalsNumCriterion(3))$split.points, list(y=c(2.5, 4.5), z=c(1.5)))
	checkEquals(TopDown(x ~ y, m, MaxIntervalsNumCriterion(3))$split.points, list(y=c(2.5, 4.5)))
	checkEquals(TopDown(x ~ z, m, MaxIntervalsNumCriterion(3))$split.points, list(z=c(1.5)))

	checkEquals(TopDown(x ~ ., m, MinEntropyDecreaseCriterion(0.0))$split.points, list(y=c(2.5, 4.5), z=c(1.5)))
	checkEquals(TopDown(x ~ ., m, MinEntropyDecreaseCriterion(0.1))$split.points, list(y=c(2.5, 4.5)))
	checkEquals(TopDown(x ~ ., m, MinEntropyDecreaseCriterion(1.4))$split.points, list(y=c(2.5, 4.5)))
	checkEquals(TopDown(x ~ ., m, MinEntropyDecreaseCriterion(1.5))$split.points, list())
 
}
