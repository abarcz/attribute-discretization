
source("R/discretization.R", chdir=TRUE)

test.discretization <- function()
{
	checkException(Discretization(), 'No-arg constructor not allowed')
	checkException(Discretization(1, data.frame()), 'First arg must be formula')
	checkException(Discretization(V ~ ., 0), 'Second arg must be data.frame')
}
