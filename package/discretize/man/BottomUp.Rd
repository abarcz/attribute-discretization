\name{BottomUp}
\alias{BottomUp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a bottom-up discretization model}
\description{
Create a bottom-up supervised discretization model, with chi-merge intervals merging.
Each attribute is discretized separately. Attributes to be discretized are selected by \code{formula}.
Multiple stop criterions can be used (also simultaneously).
}
\usage{
BottomUp(formula, data, stop.criterions = RequestedIntervalsNumCriterion(5))
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
\author{Wojciech Koszolko}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{MinChiCriterion}}, \code{\link{RequestedIntervalsNumCriterion}}, \code{\link{TopDown}}, \code{\link{CrossValidateBayes}}
}
\examples{
data(gas1)
model <- BottomUp(V1 ~ V2, gas1, MinChiCriterion(0.1))

summary(model)
print(model)
gas1.dV2V3 <- predict(model, gas1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{BottomUp}
