
source("R/discretization.R", chdir=TRUE)

test.discretization <- function()
{
	checkException(CreateBaseDiscretization(), 'No-arg constructor not allowed')
	checkException(CreateBaseDiscretization(1, data.frame()), 'First arg must be formula')
	checkException(CreateBaseDiscretization(V ~ ., 0), 'Second arg must be data.frame')
	checkException(CreateBaseDiscretization(V ~ ., data.frame(), list()), "List of stop criterions cannot be empty")
	checkException(CreateBaseDiscretization(V ~ ., data.frame(), list(1)), "Invalid stop.criterion")
	checkException(CreateBaseDiscretization(V ~ ., data.frame(), RequestedIntervalsNumCriterion(1)))

	x <- c(0, 0, 1, 1, 0)
	y <- c(1, 2, 3, 4, 5)
	z <- c(1, 1, 1, 2, 2)
	m <- data.frame(x, y, z)

	td <- CreateBaseDiscretization(x ~ ., m, RequestedIntervalsNumCriterion(1))
	checkEquals(td$discretized.attrs, c("y", "z"))
	checkEquals(td$class.label, "x")
}
