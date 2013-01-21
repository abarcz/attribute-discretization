\name{MinEntropyDecreaseCriterion}
\alias{MinEntropyDecreaseCriterion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{TopDown discretization default stop criterion.}
\description{
A TopDown discretization stop criterion, which stops discretizing an attribute when for each interval its potential entropy decrease after split is smaller than requested minimal value.
}
\usage{
MinEntropyDecreaseCriterion(min.decrease)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{min.decrease}{Minimal value of entropy decrease.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Stop criterion which can be passed to discretization model constructor.}
\references{
%% ~put references to the literature/web site here ~
}
\author{Aleksy Barcz}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DeltaCriterion}}, \code{\link{RequestedIntervalsNumCriterion}}, \code{\link{TopDown}}
}
\examples{
MinEntropyDecreaseCriterion(0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{StopCriterion}
