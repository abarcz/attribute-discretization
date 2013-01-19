
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

	td <- TopDown(x ~ ., m)
	checkEquals(td$discretized.attrs, c("y", "z"))
	checkEquals(td$class.label, "x")
	checkEquals(length(td$split.points), 2)
	checkEquals(names(td$split.points), c("y", "z"))
	checkEquals(td$split.points$y, c(2.5, 4.5)) 
	checkEquals(td$split.points$z, c(1.5))

	checkEquals(TopDown(x ~ y + z, m, MinEntropyDecreaseCriterion(0))$split.points, list(y=c(2.5, 4.5), z=c(1.5)))
	checkEquals(TopDown(x ~ y, m, MinEntropyDecreaseCriterion(0))$split.points, list(y=c(2.5, 4.5)))
	checkEquals(TopDown(x ~ z, m, MinEntropyDecreaseCriterion(0))$split.points, list(z=c(1.5)))

	checkException(TopDown(x ~ ., m, RequestedIntervalsNumCriterion(0)), "intervals.num must be >= 1")
	checkEquals(TopDown(x ~ ., m, RequestedIntervalsNumCriterion(1))$split.points, list())
	checkEquals(TopDown(x ~ ., m, RequestedIntervalsNumCriterion(2))$split.points, list(y=c(2.5), z=c(1.5)))
	checkEquals(TopDown(x ~ ., m, RequestedIntervalsNumCriterion(3))$split.points, list(y=c(2.5, 4.5), z=c(1.5)))
	checkEquals(TopDown(x ~ ., m, RequestedIntervalsNumCriterion(3))$split.points, list(y=c(2.5, 4.5), z=c(1.5)))
	checkEquals(TopDown(x ~ ., m, RequestedIntervalsNumCriterion(4))$split.points, list(y=c(1.5, 2.5, 4.5), z=c(1.5)))
	checkEquals(TopDown(x ~ ., m, RequestedIntervalsNumCriterion(5))$split.points, list(y=c(1.5, 2.5, 3.5, 4.5), z=c(1.5)))
	checkEquals(TopDown(x ~ ., m, RequestedIntervalsNumCriterion(6))$split.points, list(y=c(1.5, 2.5, 3.5, 4.5), z=c(1.5)))

	checkEquals(TopDown(x ~ ., m, MinEntropyDecreaseCriterion(0.0))$split.points, list(y=c(2.5, 4.5), z=c(1.5)))
	checkEquals(TopDown(x ~ ., m, MinEntropyDecreaseCriterion(0.1))$split.points, list(y=c(2.5, 4.5)))
	checkEquals(TopDown(x ~ ., m, MinEntropyDecreaseCriterion(1.4))$split.points, list(y=c(2.5, 4.5)))
	checkEquals(TopDown(x ~ ., m, MinEntropyDecreaseCriterion(1.5))$split.points, list())

	checkEquals(TopDown(x ~ ., m, list(RequestedIntervalsNumCriterion(4), MinEntropyDecreaseCriterion(0)))$split.points,
		list(y=c(2.5, 4.5), z=c(1.5)))	# second criterion stops splitting for y and z
	checkEquals(TopDown(x ~ ., m, list(RequestedIntervalsNumCriterion(2), MinEntropyDecreaseCriterion(0)))$split.points,
		list(y=c(2.5), z=c(1.5)))		# first criterion stops splitting for y and z
	checkEquals(TopDown(x ~ ., m, list(RequestedIntervalsNumCriterion(2), MinEntropyDecreaseCriterion(0.1)))$split.points,
		list(y=c(2.5)))					# first criterion stops y, second stops z


	checkEquals(TopDown(x ~ ., m, DeltaCriterion())$split.points, list())	# TODO this is a poor test..

	z <- c(5, 5, 5, 3, 3)	# different values to check if predict works
	m <- data.frame(x, y, z)
	checkEquals(predict(TopDown(x ~ ., m), m), data.frame(x=x, y=c(1, 1, 2, 2, 3), z=c(2, 2, 2, 1, 1)))
	checkEquals(predict(TopDown(x ~ y, m), m), data.frame(x=x, y=c(1, 1, 2, 2, 3), z=z))
	checkEquals(predict(TopDown(x ~ z, m), m), data.frame(x=x, y=y, z=c(2, 2, 2, 1, 1)))
}
