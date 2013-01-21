\name{DeltaCriterion}
\alias{DeltaCriterion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{TopDown discretization delta criterion}
\description{
A TopDown discretization stop criterion, which stops discretizing an attribute when for each interval its potential entropy decrease after split is smaller than calculated treshold. Treshold is calculated for every interval separately and takes into account both entropy decrease and representation encoding cost.
}
\usage{
DeltaCriterion()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Aleksy Barcz}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{MinEntropyDecreaseCriterion}}, \code{\link{RequestedIntervalsNumCriterion}}, \code{\link{TopDown}}
}
\examples{
DeltaCriterion()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{StopCriterion}
