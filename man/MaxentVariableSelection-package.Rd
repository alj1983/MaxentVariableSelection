\name{MaxentVariableSelection-package}
\alias{MaxentVariableSelection}
\docType{package}
\title{Selecting the best set of relevant environmental variables along
  with the optimal regularization multiplier for MAXENT Niche Modeling}

\description{Complex niche models show low performance in identifying
the most important range-limiting environmental variables and in
transferring habitat suitability to novel environmental conditions
(Warren and Seifert, 2011; Warren et al., 2014). This package helps to
identify the most important set of uncorrelated variables and to
fine-tune Maxent's regularization multiplier. In combination, this
allows to constrain complexity and increase perforance of Maxent niche
models (assessed by information criteria, such as AICc (Akaike, 1974) ,
and by the area under the receiver operating characteristic (AUC)
(Fielding and Bell, 1997). Users of this package should be familiar with
MAXENT niche modelling. This \href{https://www.google.no/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0CB4QFjAAahUKEwik2uzIkJDHAhVHmIAKHRmuB-0&url=https%3A%2F%2Fwww.cs.princeton.edu%2F~schapire%2Fmaxent%2Ftutorial%2Ftutorial.doc&ei=FA7BVaSAKsewggSZ3J7oDg&usg=AFQjCNGLRzLmkwCWYL3VM30dFpe3QBLSbQ&sig2=WApol3w6L1uA9h2nuHR7Zw&bvm=bv.99261572,d.eXY}{MAXENT Tutorial} provides a great
introduction and overview. }

\details{
\tabular{ll}{
Package: \tab MaxentVariableSelection\cr
Type: \tab Package\cr
Version: \tab 1.0-0\cr
Date: \tab 2015-08-15\cr
Depends: \tab R (>= 3.1.2)\cr
Imports: \tab ggplot2, raster\cr
License: \tab GPL (>= 2)\cr
# XX 20150804ContinueHere
Literature:
\tab Gerlach G., Jueterbock A., Kraemer P., Deppermann J. and Harmand P. 2010\cr
\tab Calculations of population differentiation based on Gst and D:\cr 
\tab forget Gst but not all of statistics!\cr
\tab \emph{Molecular Ecology} \bold{19}, p. 3845--3852.\cr
\tab \cr
\tab Goudet J., Raymond M., deMeeues T. and Rousset F. 1996\cr
\tab Testing differentiation in diploid populations.\cr
\tab \emph{Genetics} \bold{144}, 4, p. 1933--1940.\cr
\tab \cr
\tab Jost, L. 2008\cr
\tab Gst and its relatives do not measure differentiation.\cr
\tab \emph{Molecular Ecology} \bold{17}, 18, p. 4015--4026.\cr
\tab \cr
\tab Manly, B.F.J. 1997\cr
\tab \emph{Randomization, bootstrap and Monte Carlo methods in
  biology}\cr
\tab Chapman & Hall.\cr
\tab \cr
\tab Nei, M. 1973\cr
\tab Analysis of gene diversity in subdivided populations.\cr
\tab \emph{Proceedings of the National Academy of Sciences of the United
  States of America}\cr
\tab \bold{70}, 12, p. 3321--3323.\cr
\tab \cr
\tab Nei M., Chesser R. 1983\cr
\tab Estimation of fixation indices and gene diversities.\cr
\tab \emph{Annals of Human Genetics} \bold{47}, 253--259.\cr
\tab \cr
\tab Wright, S.P. 1992\cr
\tab Adjusted p-values for simultaneous inference.\cr
\tab \emph{Biometrics} \bold{48}, 1005--1013.\cr
LazyLoad: \tab yes\cr
Packaged: \tab 2010-12-18 13:48:33 UTC; alexj\cr
Built: \tab R 2.10.1; ; 2010-12-18 13:48:39 UTC; unix\cr
}

Index:
\preformatted{
D.Jost-and-Gst.Nei      Comparing Populations - Differentiation and
                        Fixation Indices
Example.transformed     Allelic Data of Three Populations For Three
                        Loci
Example.untransformed   Allelic Data of Three Populations for Three
                        Loci
}
}
\author{
Alexander Jueterbock

Maintainer: Alexander Jueterbock, <Alexander-Jueterbock@web.de> }

\section{Citation}{To cite the package 'DEMEtics' in publications use:\cr\cr
Gerlach, G., Jueterbock, A., Kraemer, P., Deppermann, J. and Harmand,
P. 2010\cr Calculations of population differentiation based on G(ST) and D:
forget G(ST)\cr but not all of statistics! \emph{Molecular Ecology}
\bold{19}, 3845-3852}

\keyword{package}
