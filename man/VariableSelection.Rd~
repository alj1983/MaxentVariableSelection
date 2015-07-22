\name{D.Jost-and-Gst.Nei}
\alias{D.Jost}
\alias{Gst.Nei}
\title{Comparing Populations - Differentiation and Fixation Indices}
\description{
The degree of genetic differentiation between populations is often
measured by the fixation index Gst (Nei, 1973). However, differentiation at
polymorphic loci with more than 2 alleles is much better reflected by the D value
(Jost, 2008; Gerlach et al., 2010). The functions of this package allow
to estimate locus by locus (and averaged over loci) pairwise Gst and D
values for codominant markers between populations and their averages over all populations.
P-values (indicating the strength of evidence against the null hypothesis of no genetic
differentiation) and 95\% confidence limits are obtained from bootstrap
methods. Depending on whether or not all populations are in Hardy
Weinberg Equilibrium for a given locus, either alleles or genotypes are
randomized over populations, respectively (see Goudet, 1996).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
D.Jost(filename, bias = "correct", object = FALSE, format.table = TRUE,
pm = "pairwise", statistics = "CI", bt = 1000)
Gst.Nei(filename, bias = "correct", object = FALSE, format.table = TRUE,
pm = "pairwise", statistics = "CI", bt = 1000)
}
\arguments{
  \item{filename}{Its syntax depends on the setting of the argument \code{object}. If
       \code{object=FALSE} (default), the filename has to be a combination of (1) the
       name of the data file (\code{.txt format}) in which the raw data are
       saved and (2) the extension \code{.txt}. It has to be enclosed in
       quotes (\dQuote{filename.txt}). If \code{object=TRUE}, the filename has to be
       the name of the object under which the data table was assigned to
       the R workspace, enclosed in quotes (\dQuote{filename}).
}
  \item{bias}{An argument providing two options (\code{correct} (default) and
    \code{uncorrected}). When using the \code{correct} option, Hs and Ht are
    transformed into nearly unbiased estimators Hs.est and Ht.est derived by Nei & Chesser
    (1993), thus reducing the bias of D and Gst values (now named Dest
    and Gst.est) as they are estimated from a small sample that intends to represent the whole population (see
Jost, 2008, p. 4022).
}
  \item{object}{This argument can be set as \code{TRUE} or \code{FALSE}, depending on the format of the
    argument \code{filename}.
}
  \item{format.table}{A logical argument either set as \code{TRUE} (default) or \code{FALSE} that defines if the format of the table has to be transformed before analysis (see details).
}
  \item{pm}{A two-level argument providing the opportunity to compare
    populations pairwise (\code{pm="pairwise"}, default) or otherwise to
    average the D or Gst values over all populations (\code{pm="overall"}).
}
\item{statistics}{A four-level argument to select whether no statistics
  (\code{statistics="none"}), 95\% confidence intervals
  (\code{statistics="CI"}), p-values (\code{statistics=}\cr\code{"p"}, testing against the null hypothesis of no
   genetic differentiation) or both, CI intervals and p-values
  (\code{statistics="all"}) shall be provided. Be aware, that the bootstrapping to obtain these statistics can take long, especially when both shall be evaluated.
}
  \item{bt}{A numeric argument (default=\code{1000}) that defines the
    amount of bootstrap resamplings, that the p-values and/or the 95\% confidence intervals are based on.
}}
\details{
  \bold{The input format}\cr
  
  The input data can be of two different formats. Both of them should be
 tab-delimited. The information that has to be provided are names or
 numbers for each individual, the according population they were sampled
 from and the alleles (length in base pairs, rounded) at each locus. Two
 alleles have to be defined for each diploid individual. Haplotype data
 can not be evaluated with this package. Missing alleles have to be set
 to zero (possible: 0, 00, 000).

The data table that has to be transformed by choosing
\code{format.table=TRUE}, can be
provided in the following format:\cr
\tabular{cccccc}{
individual \tab population \tab locus1.allele.a \tab locus1.allele.b \tab locus2.allele.a \tab locus2.allele.b \cr
P1.1 \tab P1 \tab 175 \tab 183 \tab 110 \tab 110 \cr
P1.2 \tab P1 \tab 183 \tab 183 \tab 123 \tab 126 \cr
P2.1 \tab P2 \tab 230 \tab 225 \tab 110 \tab 110 \cr
. \tab . \tab . \tab . \tab . \tab . \cr 
. \tab . \tab . \tab . \tab . \tab . \cr
. \tab . \tab . \tab . \tab . \tab .
}  
The number of populations and loci are not restricted.
The column names \code{individual} and \code{population} must be included. The
other columns listing the fragment lengths in base pairs can be named
arbitrarily. It is recommended name the two columns that refer to the
same locus, equally (e.g. \code{locus1.allele.a} and \code{locus1.allele.b} should
both be named \code{Locus1}). Mathematical signs, like \code{+} or \code{-} should be
avoided and spaces are not allowed in column names.


Alternatively, when the input data are given in the following format,
they do not have to be transformed (\code{format.table=FALSE}):
\tabular{cccc}{
individual \tab population \tab fragment.length \tab locus \cr
P1.1 \tab P1  \tab 175 \tab L1 \cr
P1.1 \tab P1  \tab 183 \tab L1 \cr
P1.2 \tab P1  \tab 183 \tab L1 \cr
P1.2 \tab P1  \tab 183 \tab L1\cr
P2.1 \tab P2  \tab 230 \tab L1 \cr
P2.1 \tab P2  \tab 225 \tab L1 \cr
. \tab . \tab .  \tab . \cr
. \tab . \tab .  \tab . \cr
. \tab . \tab .  \tab . \cr
P1.1 \tab P1  \tab 110 \tab L2 \cr
P1.1 \tab P1  \tab 110 \tab L2 \cr
P1.2 \tab P1  \tab 123 \tab L2 \cr
P1.2 \tab P1  \tab 126 \tab L2 \cr
P2.1 \tab P2  \tab 110 \tab L2 \cr
P2.1 \tab P2  \tab 110 \tab L2 \cr
. \tab . \tab .  \tab . \cr
. \tab . \tab .  \tab . \cr
. \tab . \tab .  \tab .
}
The data in the column \code{fragment.length} represent numbers of base pairs.



\bold{Details on confidence interval calculation}\cr

95\% confidence intervals of the D or Gst values are based on the range
of these values from reallocated data sets that are obtained by
bootstrapping alleles (or genotypes) of one locus within populations.
Hardy Weinberg Equilibrium (HWE) is tested for each locus and each
population. If all of the tested populations are in HWE, the alleles of
a single locus, are randomized within populations. Otherwise, alleles are not
inherited independently from each other and genotypes are randomized
within populations (Goudet, 1996). The upper and lower 95\% confidence limits are evaluated as the lower
(0.025) and upper (0.975) bounds of the quantiles of D or Gst values
from the resampled data using the function \code{quantile}:\cr
Empirical D or Gst +(-) upper(lower) quantile bound

\bold{Details on p-value calculation}\cr

To be able to test the null hypothesis of absence of genetic differentiation between populations, a
bootstrap method is performed. Thereby, alleles (or genotypes) of one
locus are randomized over all compared populations.
Hardy Weinberg Equilibrium HWE is tested for each locus and each
population. If all of the tested populations are in HWE, the alleles of
a single locus, are randomized over all populations. Otherwise, alleles are not
inherited independently from each other and genotypes are randomized
over all populations (Goudet, 1996). Reallocating alleles or genotypes
simulates populations that share a common gene
pool and are not differentiated. Since the empirical value of genetic
differentiation is expected to be larger than a value obtained from
within a panmictic population when the tested populations are
significantly differentiated, a one tailed test is carried out.
The null hypothesis (panmictic populations) can be rejected at a 95\% significance level (p<0.05)
when the empirical value is larger than 95\% of the bootstrapped test
statistics. The p-value is calculated according to Manly (1997, p. 62).

When more than two populations are compared with one another, using the
option \code{pm="pairwise"}, the p-values are adjusted in order to
account for the multiple comparison from one data set, using the
function \code{p.adjust} of the package \code{stats}. They represent the
smallest overall significance levels, at which the hypothesis would be
rejected (Wright, 1992). Those p-values giving the significance levels
for different loci, are adjusted independently from each other. Those
p-values giving the significance levels for the averaged differentiation
over all loci, are adjusted to one another. The adjustment is performed
by Bonferroni correction, by Holm's method, by Hommel's method and by a
method provided by Benjamini and Hochberg. See the help file of the
function \code{p.adjust} for further information on these methods.

\bold{Test for Hardy Weinberg Equilibrium HWE}\cr

Before bootstrapping, populations are automatically tested for being in HWE by comparing the
empirical numbers of genotypes and those expected under HWE using the
function \code{chisq.test} with the arguments: \code{simulate.p.value=TRUE},
\code{b=10000}. This means, that the p-value is obtained from a Monte Carlo
method with 10000-fold resampling. The null hypothesis of HWE is
rejected when p is smaller than 0.05.}
%% ~~ If necessary, more details than the description above ~~ }

\value{
Results are saved as .txt files (space-delimited) in the actual working
directory, which is normally the one your input data were loaded
from. The path of the working directory can be requested by typing
\code{getwd()} and changed by using the function \code{setwd()}. During
the calculation, the output is printed in the R console where the kind
of data is also shortly described and how the respective .txt files are
named. The filenames include the argument \code{filename} and the actual date.

In case that you are comparing more than two populations pairwise and
are calculating p-values and/or confidence intervals, you will be
informed about the estimated end of the analysis after completion of the
first pairwise comparison. 

If the same analysis is carried out more than once at the same day on a
single dataset, the results will all be found, one written
below the other, separated by a row of column names, in the same file (if the working directory was
not changed).


The output files are described in the following paragraphs:

\item{allelefrequencies}{A data table comprising the following columns:
  \describe{
    \item{\code{allele}}{a factor with each fragment length of an allele representing one level}
    \item{\code{number}}{a numeric vector listing how often the actual allele of the actual locus occurred in the actual population}
    \item{\code{population}}{a factor with each population representing one level}
    \item{\code{locus}}{a factor with each locus representing one level}
    \item{\code{proportion}}{a numeric vector giving the proportion of the
      actual allele and the actual locus in each population}
  }
   }
\item{sample sizes}{
    A data table comprising the following columns:
  \describe{
    \item{\code{population}}{a factor with each population representing one level}
    \item{\code{sample.size}}{a numeric vector listing the number of individuals providing data for the actual locus}
    \item{\code{locus}}{a factor with each locus name representing one level}
    }
  }
  \item{heterozygosities}{A data table that lists heterozygosites which
  are calculated according to the formulas given in Jost (2008).
    \describe{
      \item{\code{locus}}{a factor with each locus name representing one
  level}
      \item{\code{Hs}}{a numeric vector providing the mean non-bias
  corrected heterozygosities for each locus within the populations}
      \item{\code{Hs.est}}{a numeric vector providing the bias
  corrected heterozygosities for each locus within the populations}
      \item{\code{Ht}}{a numeric vector providing the non-bias
  corrected total heterozygosities for each locus over all populations}
      \item{\code{Ht.est}}{a numeric vector providing the bias
	corrected total heterozygosities for each locus over all populations}
  }}
Depending on whether populations are compared pairwise
  \code{pm="pairwise"} or differentiation / fixation is estimated over all
  populations \code{pm="overall"}, the result tables comprising the
  D/Gst values differ slightly.
  When overall D or Gst values are
  evaluated, the output comprises the following two data tables (X
  stands for D, Dest, Gst or Gst.est values):
  \item{X.loci.over.all.populations}{
    \describe{
      \item{\code{X.locus}}{A numeric vector providing the D, Dest, Gst or Gst.est
  values for each locus and pairwise comparison}
      \item{\code{Locus}}{A factor with the locus names as levels, sorted
  alphabethically or numerically (depending on how the loci are named)}
      \item{(\code{Lower.0.95.CI})}{A numerical vector giving the lower
  95\% confidence limit for the empirical D, Dest, Gst or Gst.est
  value. This output is only obtained when setting \code{statistics="CI"} or \code{statistics="all"}}
      \item{(\code{Upper.0.95.CI})}{A numerical vector giving the upper
  95\% confidence limit for the empirical D, Dest, Gst or Gst.est
  value. This output is only obtained when setting \code{statistics="CI"} or \code{statistics="all"}}
      \item{(\code{P.value})}{A numerical vector comprising the p-values that
  state the level of significance of genetic differentiation. This
  output is only obtained when setting \code{statistics="p"} or \code{statistics="all"}}
 }}
  \item{X.mean.over.all.populations}{
    \describe{
      \item{\code{X.mean}}{The mean D, Dest, Gst or Gst.est value averaged
  over all populations and loci (by calculating the arithmetic mean)}
       \item{(\code{Lower.0.95.CI})}{The lower
  95\% confidence limit for the empirical D, Dest, Gst or Gst.est
  value. This output is only obtained when setting \code{statistics=}\cr\code{"CI"} or \code{statistics="all"}}
      \item{(\code{Upper.0.95.CI})}{The upper
  95\% confidence limit for the empirical D, Dest, Gst or Gst.est
  value. This output is only obtained when setting \code{statistics=}\cr\code{"CI"} or \code{statistics="all"}}
      \item{(\code{P.value})}{The p-value that
  states the level of significance of genetic differentiation. This
  output is only obtained when setting \code{statistics="p"} or \code{statistics="all"}}
 }}

  When populations are compared pairwise, INTERMEDIATE
  RESULTS are printed and saved after each comparison.
automatically.  The next INTERMEDIATE RESULT is printed to the same file, separated from the preceding result by a row of
column names. When the whole analysis is completed, the END RESULT containing the information of all the INTERMEDIATE RESULTs in a single data frame is printed and saved to the same
file, separated from the preceding INTERMEDIATE RESULTs by a row of column names.
Appending the results one below the other avoids loss of data. But you have to be careful. If you
want to work with the INTERMEDIATE RESULTs that have already been saved, it is recommended
to copy the respective file and work with the copy. Otherwise, problems can arise, when you work
with the original file and R tries to write new results to it. This
could cause interruption of the analysis.

If an analysis is carried out more than once at the same day, the results will all be found, one written
below the other, separated by a row of column names in the same file (if the working directory
was not changed).

If an analysis runs more than one day, the INTERMEDIATE RESULTs will be saved in different
files, according to the date, they had been analysed on. But all the INTERMEDIATE RESULTs will
be included in the END RESULT in which all INTERMEDIATE RESULTs are
finally saved together.


  The output comprises data
  tables with the following information (X
  stands for D, Dest, Gst or Gst.est values):

  \item{X.loci.pairwise.comparison}{
    \describe{
      \item{\code{X.locus}}{A numeric vector providing the D, Dest, Gst or Gst.est
  values for each locus and pairwise comparison}
\item{\code{Locus}}{A factor with the locus names as levels, sorted
  alphabethically or numerically (depending on how the loci are named)}
\item{\code{Population1}}{A factor listing the first of the pairwise compared populations}
\item{\code{Population2}}{A factor listing the second of the pairwise compared populations}
      \item{(\code{Lower.0.95.CI})}{A numerical vector giving the lower
  95\% confidence limit for the empirical D, Dest, Gst or Gst.est
  value. This output is only obtained when setting \code{statistics="CI"} or \code{statistics="all"}}
      \item{(\code{Upper.0.95.CI})}{A numerical vector giving the upper
  95\% confidence limit for the empirical D, Dest, Gst or Gst.est
  value. This output is only obtained when setting \code{statistics="CI"} or \code{statistics="all"}}
      \item{(\code{P.value})}{A numerical vector comprising the p-values that
  state the level of significance of genetic differentiation. This
  output is only obtained when setting \code{statistics="p"} or \code{statistics="all"}}
 }}

\item{X.mean.pairwise.comparison}{
       \describe{
      \item{\code{X.mean}}{A numerical vector giving mean D, Dest, Gst or Gst.est values for the
       respective populationpair averaged over all loci (by calculating
       the arithmetic mean)}
\item{\code{Population1}}{A factor listing the first of the pairwise compared populations}
\item{\code{Population2}}{A factor listing the second of the pairwise compared populations}
     \item{(\code{Lower.0.95.CI})}{A numerical vector listing the lower
  95\% confidence limit for the respective empirical D, Dest, Gst or Gst.est
  value. This output is only obtained when setting \code{statistics="CI"} or \code{statistics="all"}}
      \item{(\code{Upper.0.95.CI})}{A numerical vector listing the upper
  95\% confidence limit for the respective empirical D, Dest, Gst or Gst.est
  value. This output is only obtained when setting \code{statistics="CI"} or \code{statistics="all"}}
      \item{(\code{P.value})}{A numerical vector containing the p-values that
  state the level of significance of genetic differentiation for the
       respective pairwise comparison. This
  output is only obtained when setting \code{statistics="p"} or \code{statistics="all"}}
 }}
  
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

When you choose the option \code{format.table=TRUE}, a data file called
\dQuote{Output-Inputformat.txt} is created that is needed by the
functions of this package to analyze the data.
}
\references{
Gerlach G., Jueterbock A., Kraemer P., Deppermann J. and Harmand P. 2010\cr
Calculations of population differentiation based on Gst and D:\cr
forget Gst but not all of statistics!\cr
\emph{Molecular Ecology} \bold{19}, p. 3845--3852.\cr

Goudet J., Raymond M., deMeeues T. and Rousset F. 1996\cr
Testing differentiation in diploid populations.\cr
\emph{Genetics} \bold{144}, 4, p. 1933--1940.\cr

Jost, L. 2008\cr
Gst and its relatives do not measure differentiation.\cr
\emph{Molecular Ecology} \bold{17}, 18, p. 4015--4026.\cr

Manly, B.F.J. 1997\cr
\emph{Randomization, bootstrap and Monte Carlo methods in biology}\cr
Chapman & Hall.\cr

Nei, M. 1973\cr
Analysis of gene diversity in subdivided populations.\cr
\emph{Proceedings of the National Academy of Sciences of the United
  States of America}\cr
\bold{70}, 12, p. 3321--3323.\cr

Nei M., Chesser R. 1983\cr
Estimation of fixation indices and gene diversities.\cr
\emph{Annals of Human Genetics} \bold{47}, 253--259.\cr

Wright, S.P. 1992\cr
Adjusted p-values for simultaneous inference.\cr
\emph{Biometrics} \bold{48}, 1005--1013.
}
\author{Alexander Jueterbock, \email{Alexander-Jueterbock@web.de}\cr
Philipp Kraemer, \email{philipp.kraemer@mail.uni-oldenburg.de}
%%  ~~who you are~~
}

\section{Warning}{Depending on the size of your data set and the
  performance of your computer, the bootstrapping process for
  calculating p-values and confidence intervals, can take very long so
  that you might want to run the analysis over night.

When you carry out pairwise population comparisons, you will be informed
after evaluation of the data for the first population pair, when the
whole analysis is estimated to finish.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# loading data from the example files of this package

data(Example.transformed)
Example.t <- Example.transformed

data(Example.untransformed)
Example.u <- Example.untransformed

# Calculating mean Dest values (averaged over all populations) with
# p-values and confidence intervals using only 10 bootstrap resamplings

D.Jost("Example.t", bias="correct", object=TRUE, format.table=FALSE,
pm="overall", statistics="all", bt=10)

# Calculating pairwise Gst values without any statistics

Gst.Nei("Example.u", bias="uncorrected", object=TRUE, format.table=TRUE,
pm="pairwise", statistics="none")

# If you do not know where the results of these example tables have been
# saved, type getwd()
}
\keyword{nonparametric}

