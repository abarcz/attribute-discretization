\name{CrossValidateBayes}
\alias{CrossValidateBayes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Evaluate discretization using Bayes and crossvalidation
}
\description{
Evaluate discretization using Bayes and crossvalidation. In each crossvalidation round, a discretization model is built on training set, then both training and test set are discretized. Then a naive Bayes classifier is trained on discretized training set and evaluated on discretized test set.
}
\usage{
CrossValidateBayes(formula, dataset, n.folds, discretization.method, discretization.stop.criterion)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{Formula for both discretization and naive Bayes classifier}
  \item{dataset}{data.frame with data to be discretized and classified}
  \item{n.folds}{Parameter for n-fold crossvalidation}
  \item{discretization.method}{"TopDown", "BottomUp" or "None"}
  \item{discretization.stop.criterion}{Discretization StopCriterion appropriate for \code{discretization.method}}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Mean and standard deviation of classifier accuracy.}
\references{
%% ~put references to the literature/web site here ~
}
\author{Aleksy Barcz}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BottomUp}}, \code{\link{TopDown}}
}
\examples{
data(gas1)
CrossValidateBayes(V1 ~ V2, gas1, 5, "TopDown", MinEntropyDecreaseCriterion(0.9))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Discretization}
\keyword{Bayes}% __ONLY ONE__ keyword per line
