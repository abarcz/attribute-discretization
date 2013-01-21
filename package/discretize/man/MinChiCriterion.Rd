\name{MinChiCriterion}
\alias{MinChiCriterion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{BottomUp discretization stop criterion.}
\description{
A BottomUp discretization stop criterion.
}
\usage{
MinChiCriterion(min.chi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{min.chi}{Minimal chi^2 value for intervals to be merged.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Stop criterion which can be passed to discretization model constructor.}
\references{
%% ~put references to the literature/web site here ~
}
\author{Wojciech Koszolko}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{RequestedIntervalsNumCriterion}}, \code{\link{BottomUp}}
}
\examples{
MinChiCriterion(0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{StopCriterion}
