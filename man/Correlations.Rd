\name{Correlations}
\alias{Correlations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Correlations(important.variable, variablenames, backgroundsites, coefficientthreshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{important.variable}{
%%     ~~Describe \code{important.variable} here~~
}
  \item{variablenames}{
%%     ~~Describe \code{variablenames} here~~
}
  \item{backgroundsites}{
%%     ~~Describe \code{backgroundsites} here~~
}
  \item{coefficientthreshold}{
%%     ~~Describe \code{coefficientthreshold} here~~
}
}
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
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (important.variable, variablenames, backgroundsites, 
    coefficientthreshold) 
{
    backgroundvalues <- read.csv(backgroundsites, sep = ",", 
        header = TRUE)
    backgroundvalues <- backgroundvalues[, (colnames(backgroundvalues) \%in\% 
        variablenames)]
    correlation.coefficients <- cor(backgroundvalues)
    correlation.coefficients <- correlation.coefficients[, colnames(correlation.coefficients) == 
        important.variable]
    correlation.coefficients <- correlation.coefficients[names(correlation.coefficients) != 
        important.variable]
    uncorrelatedvariables <- names(correlations)[abs(correlation.coefficients) < 
        coefficientthreshold]
    return(list(correlation.coefficients, uncorrelatedvariables))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
