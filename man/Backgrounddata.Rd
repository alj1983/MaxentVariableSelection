\name{Example.transformed}
\alias{Example.transformed}
\docType{data}
\title{Allelic Data of Three Populations for Three Loci}
\description{
The data set gives the fragment lengths (in base pairs) for four loci and three populations.
}
\usage{
data(Example.transformed)
}
\format{
  A data frame with 76 observations on the following 5 variables.
  \describe{
    \item{\code{individual}}{a factor with each individual representing one level}
    \item{\code{population}}{a factor with levels \code{population1}, \code{population2}, \code{population3}}
    \item{\code{allele}}{a factor with the two alleles of one individual for one locus as levels}
    \item{\code{fragment.length}}{a numeric vector listing the base pairs for the actual locus and allele}
    \item{\code{locus}}{a factor with the loci names as levels}
  }
}
\examples{
data(Example.transformed)
Example.transformed
}
\keyword{datasets}
