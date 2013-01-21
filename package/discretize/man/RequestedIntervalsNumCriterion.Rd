\name{RequestedIntervalsNumCriterion}
\alias{RequestedIntervalsNumCriterion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Discretization stop criterion forcing number of intervals.}
\description{
Discretization stop criterion forcing requested number of intervals, if possible. Applicable both to TopDown and BottomUp discretization.
}
\usage{
RequestedIntervalsNumCriterion(intervals.num)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{intervals.num}{Requested number of intervals for each processed attribute.}
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
\code{\link{TopDown}}, \code{\link{BottomUp}}
}
\examples{
RequestedIntervalsNumCriterion(5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{StopCriterion}
