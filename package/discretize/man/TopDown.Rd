\name{TopDown}
\alias{TopDown}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a top-down discretization model}
\description{
Create a top-down supervised discretization model, by entropy-decreasing splits of attribute domain.
Each attribute is discretized separately. Attributes to be discretized are selected by \code{formula}.
Multiple stop criterions can be used (also simultaneously).
}
\usage{
TopDown(formula, data, stop.criterions = MinEntropyDecreaseCriterion(0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A formula of the form \code{class ~ x1 + x2 + ...}. Selects attributes to be discretized.}
  \item{data}{A data.frame with training data for the model.}
  \item{stop.criterions}{A single stop criterion or a list of stop criterions to be used.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Discretization model. Can be used on data with \code{predict}.}
\references{
%% ~put references to the literature/web site here ~
}
\author{Aleksy Barcz}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{MinEntropyDecreaseCriterion}}, \code{\link{DeltaCriterion}}, \code{\link{RequestedIntervalsNumCriterion}}, \code{\link{BottomUp}}, \code{\link{CrossValidateBayes}}
}
\examples{
data(gas1)
model <- TopDown(V1 ~ V2, gas1, MinEntropyDecreaseCriterion(0.9))

summary(model)
print(model)
gas1.dV2V3 <- predict(model, gas1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{TopDown}
