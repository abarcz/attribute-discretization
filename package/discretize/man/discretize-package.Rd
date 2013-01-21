\name{discretize-package}
\alias{discretize-package}
\alias{discretize}
\docType{package}
\title{
Discretization, both bottom-up and top-down with different stop criterions.
}
\description{
Discretization, both bottom-up and top-down with different stop criterions. Stop criterions may be parametrized (if applicable) and several of them may be used simultaneously for customization.
}
\details{
\tabular{ll}{
Package: \tab discretize\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-01-20\cr
License: \tab GPL\cr
}
}
\author{
Aleksy Barcz, Wojciech Koszolko
}
\references{
}
\keyword{ package }
\seealso{
}
\examples{
data(gas1)
model <- TopDown(V1 ~ V2, gas1, MinEntropyDecreaseCriterion(0.9))

summary(model)
print(model)
gas1.dV2V3 <- predict(model, gas1)

CrossValidateBayes(V1 ~ V2, gas1, 5, "TopDown", MinEntropyDecreaseCriterion(0.9))
}
